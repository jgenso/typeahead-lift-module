package code
package snippet

import code.lib._

import net.liftweb._
import common._
import http._
import js.JsCmds._
import util._
import Helpers._
import scala.xml._

class TypeAhead {

  def render = {
    "@typeahead *" #> TwitterTypeahead.local("rocks", List("red rocks", "blue rocks", "green rocks"), Empty, (s:
    String) => { println(s); Noop })
  }
}

