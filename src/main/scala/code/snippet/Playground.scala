package code.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import code.lib._
import Helpers._
import net.liftweb.http.{DispatchSnippet, S, SHtml, WiringUI}
import net.liftweb.http.js.JsCmds

class Playground extends DispatchSnippet {

  val versions:List[(String, (NodeSeq => NodeSeq))] = List(
    ("basic", basic _),
    ("jasonsThoughts", jasonsThoughts _),
    ("validators", validators _),
    ("yourVersion", yourVersion _)
  )

  def dispatch : DispatchIt = {
    case "render" if versions.exists( v => S.param("version").exists( v._1 == _)) => versions.find(_._1 == S.param("version").get).get._2
    case "picker" => picker _
    case x => x => NodeSeq.Empty
  }
  def picker(xhtml: NodeSeq): NodeSeq = {
    versions.flatMap( v => <div><a href={"?version=%s" format v._1}>{v._1}</a> {if (S.param("version").exists(_ == v._1)) (<span>***</span>)}</div>)
  }

  //basic version
  //

  var a = 5
  var b = 0

  def basic(xhtml: NodeSeq): NodeSeq = {
    bind("form", xhtml,
    "item1" -> SHtml.ajaxText(a.toString, x => ControlHelpers.tryo(x.toInt) match {
      case Full(y:Int) => a = y
      case _ => S.error("a must be an Int")
    }),
    "item2" -> SHtml.text("", x => ControlHelpers.tryo(x.toInt) match {
      case Full(y:Int) => b = y
      case _ => S.error("b must be an Int")
    }),
    "condition" -> NodeSeq.Empty,
    "submit" -> SHtml.submit("submit", () => {if (a<b) (S.warning("good")) else (S.error("bad"))}))
  }

  //jasonsThoughts -- My understanding of how Jason has done this in the past.
  //
  // -every field is wrapped with an Option[Field]
  // -If all Options[Fields] are isDefined, we passed the field level tests
  // -Any cross-field testing must be put into the submit
  // -Negative: If we want to also warn that 'a is not less than b' we need duplicate logic (pop-up warning and submit button)

  var optA:Option[Int] = Some(a)
  var optB:Option[Int] = None
  var validateAlessB: Option[Unit] = None

  def jasonsThoughts(xhtml: NodeSeq): NodeSeq = {
    bind("form", xhtml,
    "item1" -> SHtml.ajaxText(a.toString, x => {optA = ControlHelpers.tryo(x.toInt); if (optA.isEmpty) (JsCmds.Alert("a must be an Int"))} ),
    "item2" -> SHtml.ajaxText("", x => {optB = ControlHelpers.tryo(x.toInt); if (optB.isEmpty) (JsCmds.Alert("b must be an Int"))} ),
    "condition" -> NodeSeq.Empty,
    "alessb" -> SimpleWiringUI((optA, optB)) {
      for { a <- optA; b <- optB } {
        validateAlessB = Some(()).filter(_ => a < b)
	validateAlessB match {
          case Some(_) => NodeSeq.Empty
          case None    => Text("a must be less than b")
        }
      }
    },
    "submit" -> SHtml.submit("submit", () => {
      (for {
        a <- optA
        b <- optB
        _ <- validateAlessB
      } yield {
        // save the values
      }) getOrElse (S.error("Please correct the highlighted fields"))
    }
  }


  //Validators
  //

  //SimpleWiringUI makes WiringUI a pinch easier to use for the simple case.
  object SimpleWiringUI{
    def apply[T](dynamicValue: => T)(f: T => NodeSeq) = {
      WiringUI(NodeSeq.Empty, DynamicCell(() => dynamicValue))(cellValue => ns => f(cellValue))
    }
  }


  abstract class Validator {
    def test(s: String): Boolean
  }
  object IntegerValidator extends Validator {
    def test(s: String) = ControlHelpers.tryo(s.toInt).isDefined
  }

  //VHtml would replace SHtml and have similar methods (text, ajaxText, textArea, etc.)
  //The "V" stands for Validator
  object VHtml {
    def ajaxText[T](value: String, errorMessage: String, convert: String => Option[T], validators: Validator*): VHtml = {
      new VHtml[T] {
        var varValue = value
        def valueOpt = if (validatorsPass) convert(varValue) else None
        private def validatorsPass = validators.forall(_.test(varValue))

        def toForm = <span>
                       { SHtml.ajaxText(varValue, varValue = _) }
                       { SimpleWiringUI(varValue)(_ => if (validatorsPass) NodeSeq.Empty else Text(errorMessage)) }
                     </span>

      }
    }

    def validatorMsg[T](value: => T, errorMessage: String, f: T => Boolean): VHtml = {
      new VHtml() {
        def valueOpt = if (f(value)) Some("") else None
        def apply() = <span>{SimpleWiringUI(value)(currentValue => if (f(currentValue)) NodeSeq.Empty else Text(errorMessage))}</span>
      }
    }

  }

  abstract class VHtml[T] {
    def toForm: NodeSeq
    def valueOpt: Option[T]
    def valueFromString: String => Option[T]
  }

  // COMMENT: re "must be an integer!" -- the validator should be in charge of producing that error message
  // COMMENT: re ControlHelpers.tryo(_.toInt) -- it would be great if the validator just returned a Box[Int] instead
  //          of a boolean, performing both functions in one. Can even take advantage of the Failure case.
  val aVText = VHtml.ajaxText[String](a.toString, "must be an integer!", ControlHelpers.tryo(_.toInt), IntegerValidator)
  val bVText = VHtml.ajaxText[String]("", "must be an integer!", ControlHelpers.tryo(_.toInt), IntegerValidator)
  val conditionText = VHtml.validatorMsg((aVText.valueIntOpt, bVText.valueIntOpt), "a must be less than b",
  { a: (Option[Int], Option[Int]) => a match {
    case ((x:Option[Int]), (y:Option[Int])) => x.exists(xv => y.exists(yv => xv < yv))
    case _ => true
  }
  })

  def validators(xhtml: NodeSeq): NodeSeq = {
    bind("form", xhtml,
    "item1" -> aVText.toForm,
    "item2" -> bVText.toForm,
    "condition" -> conditionText.toForm,
    "submit" -> SHtml.submit("submit", () => {
      (for {
        userA <- aVText.valueIntOpt
        userB <- bVText.valueIntOpt
        relationship <- conditionText.valueOpt //need to test the relationship between a and b
      } yield {
        a = userA
        b = userB
        S.warning("good")
      }) getOrElse (S.warning("bad"))
    }))
  }


  //your version
  //

  def yourVersion(xhtml: NodeSeq): NodeSeq = {
    bind("form", xhtml,
    "item1" -> SHtml.text(a.toString, x => a = x.toInt ),
    "item2" -> SHtml.text("", x => b = x.toInt ),
    "submit" -> SHtml.submit("submit", () => S.warning("show the user 'good' or 'bad'")))
  }


}