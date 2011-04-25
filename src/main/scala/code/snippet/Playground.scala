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

  def jasonsThoughts(xhtml: NodeSeq): NodeSeq = {
    bind("form", xhtml,
    "item1" -> SHtml.ajaxText(a.toString, x => {optA = ControlHelpers.tryo(x.toInt); if (optA.isEmpty) (JsCmds.Alert("a must be an Int"))} ),
    "item2" -> SHtml.ajaxText("", x => {optB = ControlHelpers.tryo(x.toInt); if (optB.isEmpty) (JsCmds.Alert("b must be an Int"))} ),
    "condition" -> NodeSeq.Empty,
    "submit" -> SHtml.submit("submit", () => if (optA.isDefined && optB.isDefined && (optA.get < optB.get)) (S.warning("good")) else (S.warning("bad"))))
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
    def ajaxText(value: String, errorMessage: String, validators: Validator*): VHtml = {
      new VHtml() {
        var varValue = value
        def valueOpt = if (validatorsPass) Some(varValue) else None
        private def validatorsPass = validators.forall(_.test(varValue))

        def apply() = <span>{SHtml.ajaxText(varValue, varValue = _)}
                          {SimpleWiringUI(varValue)(_ => if (validatorsPass) NodeSeq.Empty else Text(errorMessage))}
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

  abstract class VHtml() {
    def apply(): NodeSeq
    def valueOpt: Option[String]
    //I like the idea below for Int, Calendar, Float, etc.  But how will it be extended for external types (JodaDateTime)?
    def valueIntOpt: Option[Int] = valueOpt.flatMap(s => ControlHelpers.tryo(s.toInt))
  }

  val aVText = VHtml.ajaxText(a.toString, "must be an integer!", IntegerValidator)
  val bVText = VHtml.ajaxText("", "must be an integer!", IntegerValidator)
  val conditionText = VHtml.validatorMsg((aVText.valueIntOpt, bVText.valueIntOpt), "a must be less than b",
  { a: (Option[Int], Option[Int]) => a match {
    case ((x:Option[Int]), (y:Option[Int])) => x.exists(xv => y.exists(yv => xv < yv))
    case _ => true
  }
  })

  def validators(xhtml: NodeSeq): NodeSeq = {
    bind("form", xhtml,
    "item1" -> aVText(),
    "item2" -> bVText(),
    "condition" -> conditionText(),
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