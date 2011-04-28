package code.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import code.lib._
import Helpers._
import net.liftweb.http.{DispatchSnippet, S, SHtml, WiringUI}
import net.liftweb.http.js.{JsCmd, JsCmds}

class Playground {

  //SimpleWiringUI makes WiringUI a pinch easier to use for the simple case.
  object SimpleWiringUI{
    def apply[T](dynamicValue: => T)(f: T => NodeSeq) = {
      WiringUI(NodeSeq.Empty, DynamicCell(() => dynamicValue))(cellValue => ns => f(cellValue))
    }
  }


  abstract class Validator[A, B] {
    def validate(a: A): Box[B]
    def andThen[C](that: Validator[B, C]): Validator[A, C] = {
      val thisParent = this //COMMENT: Jason, is there a better way to do this?
      new Validator[A, C] {
        def validate(a: A) = for {
          b <- thisParent.validate(a)
          c <- that.validate(b)
        } yield c
      }
    }
  }

  object IntegerValidator extends Validator[String, Int] {
    def validate(s: String) = ControlHelpers.tryo(s.toInt) ?~! "Integer required"
  }
  object RequiredValidator extends Validator[String, String] {
    def validate(s: String) = if (s.isEmpty) Failure("required field") else Full(s)
  }

  abstract class VHtml[T] {
    def toForm: NodeSeq
    def valueBox: Box[T]
  }

  class Page {
    private var validatorResults: List[()=>Box[Any]] = Nil

    // Returns Empty if all validations are passing.  Otherwise returns list of Failures encountered
    def getFailures: List[Failure] = {
      validatorResults.flatMap( _() match {
        case Full(f) => Nil //ignore good cases
        case f: Failure => List(f) //collect bad ones
        case Empty => List(Failure("an error occured -- ideally the validator should have given a Failure not an Empty"))
      })
    }

    def ajaxText[T](defaultValue: String, func: T => JsCmd, validator: Validator[String, T]): VHtml[T] = {
      val result = new VHtml[T] {
        var value = defaultValue
        def valueBox = validator.validate(value)

        def toForm = <span>
                       { SHtml.ajaxText(value, v => {value = v; valueBox match { case Full(v) => func(v) case _ => JsCmds.Noop}}) }
                       { SimpleWiringUI(value)(_ => valueBox match { case Failure(msg, _, _) => Text(msg) case _ => NodeSeq.Empty })}
                     </span>

      }
      validatorResults ::= (() => result.valueBox)
      result
    }

    def validatorMsg[T](value: => T, errorMessage: String, validate: T => Box[Unit]): VHtml[Unit] = {
      val result = new VHtml[Unit]() {
        def valueBox = validate(value) match { case Empty => Failure(errorMessage) case b => Full(()) }
        def toForm = <span>{SimpleWiringUI(value)(_ => valueBox match { case Failure(msg, _, _) => Text(msg) case _ => NodeSeq.Empty })}</span>
      }
      validatorResults ::= (() => result.valueBox)
      result
    }

  }

  val myPage = new Page()

  val aVText = myPage.ajaxText[Int](a.toString, a = _, IntegerValidator)
  val bVText = myPage.ajaxText[Int]("", b = _, IntegerValidator)
  val conditionText = myPage.validatorMsg((aVText.valueBox, bVText.valueBox), "a must be less than b",
    (_:(Box[Int],Box[Int])) match {
      case (Full(a: Int), Full(b: Int)) => Some(()).filter(x => a < b)
      case x => Empty
    }
  )

  def render(xhtml: NodeSeq): NodeSeq = {
    bind("form", xhtml,
    "item1" -> aVText.toForm,
    "item2" -> bVText.toForm,
    "condition" -> conditionText.toForm,
    "submit" -> SHtml.submit("submit", () => {
      myPage.getFailures match {
        case Nil => S.error("good") //doSave()
        case failures => failures.foreach{case Failure(msg, _, _) => S.error(msg)}
      }
    }))
  }
}