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
    "alessb" -> SimpleWiringUI((optA, optB)) { _ =>
      (for { a <- optA; b <- optB } yield {
        validateAlessB = Some(()).filter(_ => a < b)
	      validateAlessB match {
          case Some(_) => NodeSeq.Empty
          case None    => Text("a must be less than b")
        }
      }) getOrElse (NodeSeq.Empty)
    },
    "submit" -> SHtml.submit("submit", () => {
      (for {
        a <- optA
        b <- optB
        _ <- validateAlessB
      } yield {
        // save the values
      }) getOrElse (S.error("Please correct the highlighted fields"))
    }))
  }


  //Validators
  //

  //SimpleWiringUI makes WiringUI a pinch easier to use for the simple case.
  object SimpleWiringUI{
    def apply[T](dynamicValue: => T)(f: T => NodeSeq) = {
      WiringUI(NodeSeq.Empty, DynamicCell(() => dynamicValue))(cellValue => ns => f(cellValue))
    }
  }


  abstract class Validator[A, B] {
    def validate(a: A): Box[B]
    def andThen(that: Validator[B, C]): Validator[A, C] = new Validator[A, C] {
      def validate(a: A) = for {
        b <- this.validate(a)
        c <- that.validate(b)
      } yield c
    }
  }

  object IntegerValidator extends Validator[String, Int] {
    def validate(s: String) = ControlHelpers.tryo(s.toInt)
  }
  object RequiredValidator extends Validator[String, String] {
    def validate(s: String) = if (s.isEmpty) Failure("required field") else Full(s)
  }

  // VHtml would replace SHtml and have similar methods (text, ajaxText, textArea, etc.)
  // The "V" stands for Validator
  object VHtml {
    def ajaxText[T](defaultValue: String, validator: Validator[String, T]): VHtml = {
      new VHtml[T] {
        var value = defaultValue
        def valueBox = validator.validate(value)

        def toForm = <span>
                       { SHtml.ajaxText(value, value = _) }
                       { SimpleWiringUI(value)(_ => valueBox match { case Failure(msg) => Text(msg) case _ => NodeSeq.Empty })}
                     </span>

      }
    }

    def validatorMsg[T](value: => T, errorMessage: String, validate: T => Box[Unit]): VHtml = {
      new VHtml() {
        def valueBox = validate(value) match { case Empty => Failure(errorMessage) case b => b }
        def toForm = <span>{SimpleWiringUI(value)(_ => valueBox match { case Failure(msg) => Text(msg) case _ => NodeSeq.Empty })}</span>
      }
    }

  }

  abstract class VHtml[T] {
    def toForm: NodeSeq
    def valueOpt: Option[T]
    def valueFromString: String => Option[T]
  }

  val aVText = VHtml.ajaxText[String](a.toString, IntegerValidator)
  val bVText = VHtml.ajaxText[String]("", IntegerValidator)
  val conditionText = VHtml.validatorMsg((aVText.valueBox, bVText.valueBox), "a must be less than b",
    { case (aBox, bBox) => for { a <- aBox, b <- bBox; if a < b } yield () }
  )

  def validators(xhtml: NodeSeq): NodeSeq = {
    bind("form", xhtml,
    "item1" -> aVText.toForm,
    "item2" -> bVText.toForm,
    "condition" -> conditionText.toForm,
    "submit" -> SHtml.submit("submit", () => {
      val result = for {
        userA <- aVText.valueBox
        userB <- bVText.valueBox
        _ <- conditionText.valueBox
      } yield {
        a = userA
        b = userB
      }
      result match {
        case Failure(msg) => S.error(msg)
        case _ => doSave()
      }
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