package code
package snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import code.lib._
import Helpers._
import net.liftweb.http._
import SHtml._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds.{Replace, Noop}
import scala.xml._
import net.liftweb.http.js.JsCmd

class HelloWorld {

  def ajaxEditableSelect(opts: Seq[(String, String)], deflt: Box[String], 
    f: String => JsCmd, attrs: ElemAttr*): Elem = {
    
    val id = nextFuncName
    val attributes = attrs + "id" -> id
    val textOpt = nextFuncName
    val options = opts.toList :::  List((textOpt , "New Element"))
    var _options = options 
    lazy val func: (String) => JsCmd = (s: String) => {
      def text(t: String): JsCmd = {
        _options = (t, t) :: _options
        Replace(id, ajaxSelect(_options, deflt, func, attributes))
      }

      if (s == textOpt) Replace(id, ajaxText("", text(_), attributes)) else f(s)
    }
    ajaxSelect(options, deflt, func, attributes)
  }

  def render = {
    "@add" #> ajaxEditableSelect(Seq(("1", "1"), ("2", "2")), Empty, (s: String) => Noop)
  }
}

