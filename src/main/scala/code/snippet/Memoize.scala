package code
package snippet

import net.liftweb._
import common._
import http._
import js.JsCmds._
import util._
import Helpers._
import scala.xml._

object Count extends RequestVar[Int](0)

object Memoize {

  def incrementCount = {
    val count = Count.is
    Count.set(count + 1)
  }

  def render = {
    "div" #> SHtml.idMemoize(in => {
      "@count *" #> Text(Count.is.toString) &
      "@refresh [onclick]" #> SHtml.ajaxInvoke(() => {
        incrementCount
        in.setHtml()
      })
    })
  }
}
