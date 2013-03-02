package net.liftmodules
package typeahead

import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import net.liftweb.common._
import net.liftweb.http._
import SHtml._
import net.liftweb.http.js.{JsCmd, JsObj, JsCmds, JE} 
import net.liftweb.http.SHtml.ElemAttr
import net.liftweb.util.Helpers._
import JsCmds._ 
import JE._

import scala.xml._


case class TypeaheadOptions(
  name: String, 
  local: List[String], 
  prefetch: Box[String => String], 
  remote: Box[String => String]
) 
{
  def toJson(id: String) = {
    val json =
      ("name" -> name) ~ 
      ("local" -> JArray(local map { JString(_) }))  ~
      ("prefetch" -> JString(prefetch map { _(id) } openOr "" )) ~
      ("remote" -> JString(remote map { _(id) } openOr "" ))
 
    // Filter out any options that weren't set. This probably isn't
    // the best way to handle this but it works
    json transform {
      case JField("local", JArray(a)) if a.isEmpty => JNothing 
      case JField("prefetch", JString(s)) if s.isEmpty => JNothing 
      case JField("remote", JString(s)) if s.isEmpty => JNothing 
    }
  }
}

object TwitterTypeahead {

  case class URL(attribute: String, url: String => String)

  private val _url = "/twitter/typeahead/%s/%s"
 
  private def makeUrl(part: String)(id: String) = _url.format(part, id)

  private val prefetchUrl = makeUrl("prefetch") _

  private val remoteUrl = makeUrl("remote") _ 

  private def script(id: String, options: JValue) = 
     JsRaw("""
      (function($) {
        $('#%s').typeahead([ %s ]);
      })(jQuery);
     """.format(id, compact(render(options)))
    )

  def h(name: String, candidates: List[String], deflt: Box[String],
    f: String => JsCmd, options: TypeaheadOptions, attrs: ElemAttr*) = {
 
    val id = discoverId(attrs: _*)
    val attributes = addIdIfNeeded(id, attrs: _*)
    val s = script(id, options.toJson(id)) 
    
    TypeaheadSuggestions.register(id, candidates)
    
    _render(deflt, s, f, attributes: _*)
  }

  def local(name: String, candidates: List[String], deflt: Box[String],
    f: String => JsCmd, attrs: ElemAttr*) = {
    
    val options = TypeaheadOptions(name, candidates, Empty, Empty)
    h(name, candidates, deflt, f, options, attrs: _*)
  }

  def prefetch(name: String, candidates: List[String], deflt: Box[String],
    f: String => JsCmd, attrs: ElemAttr*) = {
    
    val options = TypeaheadOptions(name, Nil, Full(prefetchUrl), Empty)
    h(name, candidates, deflt, f, options, attrs: _*)
  }
  
  def remote(name: String, candidates: List[String], deflt: Box[String],
    f: String => JsCmd, attrs: ElemAttr*) = {

    val options = TypeaheadOptions(name, Nil, Empty, Full(remoteUrl))
    h(name, candidates, deflt, f, options, attrs: _*)
  }
  
  def remoteWithPrefetch(name: String, candidates: List[String], deflt: Box[String],
    f: String => JsCmd, attrs: ElemAttr*) = {
    
    val options = TypeaheadOptions(name, Nil, Full(prefetchUrl), Full(remoteUrl))
    h(name, candidates, deflt, f, options, attrs: _*)
  }

  private def _render(deflt: Box[String], script: JsRaw, f: String => JsCmd, attrs: ElemAttr*) = {
    <head_merge>
      { Script(OnLoad(script)) }
    </head_merge> ++
    SHtml.text(deflt openOr "", f, attrs: _*)
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
