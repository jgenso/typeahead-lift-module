package code
package lib

import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json._
import net.liftweb.common._
import net.liftweb.http._
import SHtml._
import net.liftweb.http.js.{JsCmd, JsObj, JsCmds, JE} 
import net.liftweb.http.SHtml.ElemAttr
import net.liftweb.util.Helpers._
import JsCmds._ 
import JE._

import scala.xml._

case class Typeahead(name: String, local: List[String])

object TwitterTypeahead {
 
  implicit val formats = Serialization.formats(NoTypeHints)

  def localScript(id: String, name: String, candidates: List[String]) = { 
    JsRaw("""
      (function($) {
        $('#%s').typeahead([ %s ]);
      })(jQuery);
     """.format(id, write(Typeahead(name, candidates)))
    )
  }

  def prefectScript(id: String, name: String, candidates: List[String]) = { 
    JsRaw("""
      (function($) {
        $('#%s').typeahead([ %s ]);
     })(jQuery);
     """.format(id, write(Typeahead(name, candidates)))
   )
  } 

  def remoteScript(id: String, name: String, candidates: List[String]) = { 
    JsRaw("""
      (function($) {
        $('#%s').typeahead([ %s ]);
     })(jQuery);
     """.format(id, write(Typeahead(name, candidates)))
    )
  }  

  def local(name: String, candidates: List[String], deflt: Box[String],
    f: String => JsCmd, attrs: ElemAttr*) = {
    
    val id = discoverId(attrs: _*)
    val attributes = addIdIfNeeded(id, attrs: _*)

    _render(deflt, localScript(id, name, candidates), f, attributes: _*)
  }

  def prefetch(name: String, candidates: List[String], deflt: Box[String],
    f: String => JsCmd, attrs: ElemAttr*) = {
    
    val id = discoverId(attrs: _*)
    val attributes = addIdIfNeeded(id, attrs: _*)

    _render(deflt, prefectchScript(id, name, candidates), f, attributes: _*)
  }
  
  def remote(name: String, candidates: List[String], deflt: Box[String],
    f: String => JsCmd, attrs: ElemAttr*) = {

    val id = discoverId(attrs: _*)
    val attributes = addIdIfNeeded(id, attrs: _*)
    
    _render(deflt, remoteScript(id, name, candidates), f, attributes: _*)
  }

  private def _render(deflt: Box[String], script: JsRaw, f: String => JsCmd, attrs: ElemAttr*) = {
    <head_merge>
      { Script(OnLoad(script)) }
    </head_merge> ++
    SHtml.ajaxText(deflt openOr "", f, attrs: _*)
  }

  private def discoverId(attrs: ElemAttr*): String = {
    attrs collectFirst { 
      case BasicElemAttr(name, value) if name == "id" => value
    } getOrElse nextFuncName
  }

  private def addIdIfNeeded(id: String, attrs: ElemAttr*) = {
    val idElem = BasicElemAttr("id", id)
    if (attrs.contains(idElem)) attrs 
    else idElem +: attrs
  }
}
