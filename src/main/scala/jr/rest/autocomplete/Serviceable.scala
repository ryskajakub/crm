package jr.rest.autocomplete

/**
 * User: coub
 * Date: 1/7/12
 * Time: 2:46 PM
 */

import net.liftweb.http._
import net.liftweb.http.rest._
import jr.model.Logic
import net.liftweb.json.Serialization

object ServiceableRest extends RestHelper {
  val columnsServed = List("specification","manufacturer")
  serve {
    case Req("autocomplete" :: "serviceable" :: column :: query :: Nil, _, GetRequest) if columnsServed.contains(column) =>
      val namesFound = Logic.getNames(column,query)
      val serial = Serialization.write(namesFound)
      PlainTextResponse(serial)
    case Req("autocomplete" :: "serviceable" :: "fillin" :: "specification" :: query :: Nil, _, GetRequest) =>
      val found = Logic.getServiceableByName(query)
      val serial = Serialization.write(found)
      PlainTextResponse(serial)
  }
}