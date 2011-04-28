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

  var a = 5
  var b = 0

  // Changes
  //
  //
  // #1 -- I'm liking how Validators are attached to VHtml objects directly, however I would also
  //       like to see them live by themselves so that they can produce error messages at other bindings
  //       (different locations in the HTML) as well as have additional styling.
  //
  //       I have introduced 'validatorSpan' for this purpose.  It takes a Validation as input along with
  //       a Validator to apply to it.
  //
  // #2 -- Perhaps slightly controversial, but I have decided that we should play around with a new
  //       type called Validation.  This is either Valid(T) or Invalid(errMsg).  (Like a Box without Empty).
  //       I believe this is a good direction as Validators should either declare something to be Valid or
  //       have an error.
  //
  //       Jason brought up some good comments last night on the phone about potentially having a
  //       OptionalIntegerValidator.  I bleieve its return type should be Validation[Option[Int]].
  //       I am very happy with this *except* when we chain a IntergerRangeValidator on which excepects [Int] not
  //       Option[Int].


  //SimpleWiringUI makes WiringUI a pinch easier to use for the simple case.
  object SimpleWiringUI{
    def apply[T](dynamicValue: => T)(f: T => NodeSeq) = {
      WiringUI(NodeSeq.Empty, DynamicCell(() => dynamicValue))(cellValue => ns => f(cellValue))
    }
  }

  // Validation.  Either Valid(T) or Invalid(errMsg).
  abstract class Validation[+A] {
    def map[B](f: A => B): Validation[B]
    def flatMap[B](f: A => Validation[B]): Validation[B]
  }
  case class Invalid(msg: String) extends Validation[Nothing] {
    type A = Nothing
    override def map[B](f: A => B): Validation[B] = this
    override def flatMap[B](f: A => Validation[B]): Validation[B] = this
  }
  case class Valid[A](value: A) extends Validation[A] {
    override def map[B](f: A => B): Validation[B] = Valid(f(value))
    override def flatMap[B](f: A => Validation[B]): Validation[B] = f(value)
  }

  abstract class Validator[A, B] {
    def validate(a: A): Validation[B]
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

  // This is a bit ugly of a helper method.  Perhaps it can be done better or maybe it is an indicator that
  // the model is broken.  If we have a LessThanValidator[Int, Int], we need to pass in a Validation[(Int, Int)].
  // The problem is we are getting are values from two different text boxes, and thus we have two Validation[Int]s.
  // This flattens them.  I am comfortable leaving it here for now and discussing.

  def flattenValidations[A,B](left: Validation[A], right: Validation[B]): Validation[(A, B)] = {
    for {
      a <- left
      b <- right
    } yield {
      (a,b)
    }
  }

  //The validators below are working just fine, so I'm going to convert from Box to Validator on them.
  //I also imagine this method being removed and the Validators being moved over to Validator.
  import net.liftweb.common.{Full, Empty, Failure}
  implicit def Box2Validation[T](v: Box[T]):Validation[T] = v match {
    case Full(v) => Valid(v)
    case Failure(msg, _, _) => Invalid(msg)
    case _ => Invalid("bug in validator -- returned Empty")
  }


  object IntegerValidator extends Validator[String, Int] {
    def validate(s: String) = ControlHelpers.tryo(s.toInt) ?~! "Integer required"
  }
  object RequiredValidator extends Validator[String, String] {
    def validate(s: String) = if (s.isEmpty) Failure("required field") else Full(s)
  }

  abstract class VHtml[T] {
    def toForm: NodeSeq
    def validatedValue: Validation[T]
  }

  class VSnippet {
    private var validatorResults: List[() => Validation[Any]] = Nil

    // Returns Empty if all validations are passing.  Otherwise returns list of Failures encountered
    def getFailures: List[Invalid] = {
      validatorResults.flatMap( _() match {
        case Valid(v) => Nil //ignore good cases
        case f: Invalid => List(f) //collect bad ones
      })
    }

    def ajaxText[T](defaultValue: String, func: T => JsCmd, validator: Validator[String, T]): VHtml[T] = {
      val result = new VHtml[T] {
        var value = defaultValue
        val errorElement = validatorSpan[String,T](Valid(value), None, x => NodeSeq.Empty, validator)
        def validatedValue = errorElement.validatedValue

        def toForm = <span>
                       { SHtml.ajaxText(value, v => {value = v; validatedValue match { case Valid(v) => func(v) case _ => JsCmds.Noop}}) }
                       { errorElement.toForm}
                     </span>

      }
      validatorResults ::= (() => result.validatedValue)
      result
    }

    def validatorSpan[A,B](source: => Validation[A], errorMessage: Option[String], func: B => NodeSeq, validator: Validator[A,B]): VHtml[B] = {

      val result = new VHtml[B]() {
        def validationDetailed: (Validation[B], Symbol) = source match {
          case Valid(value) => (validator.validate(value), 'evaluated)
          case x: Invalid => (x, 'shortcircuit)
        }

        def validatedValue: Validation[B] = validationDetailed._1

        def toForm = SimpleWiringUI(validationDetailed)( _ match {
            case (_, 'shortcircuit) => <div>shortcircuit</div> //NodeSeq.Empty //Do nothing, there was an earlier failure
            case (Valid(value), _) => func(value) //pass
            case (Invalid(msg), _) => <div>{errorMessage.getOrElse(msg)}</div> //fail
        })
      }
      validatorResults ::= (() => result.validatedValue)
      result
    }

  }

  val mySnippet = new VSnippet()

  val aVText = mySnippet.ajaxText[Int](a.toString, a = _, IntegerValidator)
  val bVText = mySnippet.ajaxText[Int](b.toString, b = _, IntegerValidator)

  //Example of a custom validator (LessThanValidator)
  object LessThanValidator extends Validator[(Int, Int), (Int, Int)] {
    def validate(v: (Int, Int)) = if (v._1 < v._2) Valid((v._1, v._2)) else Failure("first value must be less than second")
  }

  val conditionText = mySnippet.validatorSpan[(Int, Int), (Int, Int)](flattenValidations(aVText.validatedValue, bVText.validatedValue),
                                  Some("a must be less than b"), n => NodeSeq.Empty, LessThanValidator)

  def render(xhtml: NodeSeq): NodeSeq = {
    bind("form", xhtml,
    "item1" -> aVText.toForm,
    "item2" -> bVText.toForm,
    "condition" -> conditionText.toForm,
    "submit" -> SHtml.submit("submit", () => {
      mySnippet.getFailures match {
        case Nil => S.error("good") //doSave()
        case failures => failures.foreach{ case Invalid(msg) => S.error(msg) }
      }
    }))
  }
}

