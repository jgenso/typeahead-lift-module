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

import net.liftmodules.typeahead.{TwitterTypeahead}

class TypeAhead {

  def render = {
    val local = List("red", "blue", "green", "orange", "purple", "white", "grey")
    val prefetch = List("United States", "Canada", "United Kingdom", "Mexico", "Italy")
    val remote = List("foo", "bar", "baz")

    "@local *" #> TwitterTypeahead.local("colors", local, Empty, s =>  Noop, "class" -> "typeahead tt-query") &
    "@prefetch *" #> TwitterTypeahead.prefetch("countries", prefetch, Empty, s =>  Noop) &
    "@remote *" #> TwitterTypeahead.remote("foo", remote, Empty, s =>  Noop)
  }
}
