package net.liftmodules
package typeahead

import net.liftweb._
import common._
import http._
import rest._
import json._
import JsonDSL._


object TypeaheadRest extends RestHelper {

  serve {
    case "twitter" :: "typeahead" :: "prefetch" :: key :: Nil JsonGet _ => {
      val suggestions = TypeaheadSuggestions.getOrElseNil(key)
      JArray(suggestions map { JString (_) } )
    }
    case "twitter" :: "typeahead" :: "remote" :: key :: Nil JsonGet _ => {
      val suggestions = TypeaheadSuggestions.getOrElseNil(key)
      JArray(suggestions map { JString (_) } )
    }
  }
}
