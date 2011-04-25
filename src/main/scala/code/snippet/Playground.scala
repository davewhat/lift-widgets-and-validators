package code.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import code.lib._
import Helpers._
import net.liftweb.http.{DispatchSnippet, S, SHtml}
import net.liftweb.http.js.JsCmds

class Playground extends DispatchSnippet {

  val versions:List[(String, (NodeSeq => NodeSeq))] = List(
    ("basic", basic _),
    ("jasonsThoughts", jasonsThoughts _),
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

  //your version
  //

  def yourVersion(xhtml: NodeSeq): NodeSeq = {
    bind("form", xhtml,
    "item1" -> SHtml.text(a.toString, x => a = x.toInt ),
    "item2" -> SHtml.text("", x => b = x.toInt ),
    "submit" -> SHtml.submit("submit", () => S.warning("show the user 'good' or 'bad'")))
  }


}