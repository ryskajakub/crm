package jr.snippet

import _root_.net.liftweb.util._
import jr.model._
import Helpers._
import collection.immutable.List
import jr.spec._
import scala.collection.mutable.{Set => MSet}
import xml.{Text, NodeSeq}
import net.liftweb.common.{Box, Full}

/**
 * Created by IntelliJ IDEA.
 * User: coub
 * Date: 1/4/12
 * Time: 9:04 PM
 * To change this template use File | Settings | File Templates.
 */

class ArchiveSnip(x: (Box[(Service, List[(Option[Int], Serviceable)])], List[Service])) {

  def render(in: NodeSeq) = {
    val (Full(Pair(service, serviceParts)), services) = x
    Helpers.bind("output", in,
      "type" -%> Text(service.type1.toString),
      "date" -%> Text(Misc.dateFormat.print(service.date1)),
      "result" -%> Text(service.result),
      "serviceParts" -%> ((in2: NodeSeq) => {
        serviceParts.flatMap {
          (x: (Option[Int], Serviceable)) => {
            val (machineHours, serviceable) = x
            Helpers.bind(
              "part", in2,
              "anchor" -%> {
                "a *" #> serviceable.specification &
                  "a [href] " #> Spec.serviceable.toLoc.calcHref(serviceable)
              },
              "machineHours" -%> Text(Misc.readable(machineHours))
            )
          }
        }
      }),
      "servicemen" -%> {
        "a *" #> service.getServicemen.map{_.name}
      }
    )
  }

  def list = {
    val (Full(y), services) = x
    "li *" #> services.map {
      s =>
        "a [href]" #> Spec.archive.toLoc.calcHref((Full(s, Nil), Nil)) &
          "a *" #> (Misc.dateFormat.print(s.date1) + " ")
    }
  }

  val (maybeService, services) = x

  def forempty(in: NodeSeq): NodeSeq = {
    if (maybeService.isEmpty) {
      in
    } else {
      NodeSeq.Empty
    }
  }

  def forfull(in: NodeSeq): NodeSeq = {
    if (maybeService.isDefined) {
      in
    } else {
      NodeSeq.Empty
    }
  }
}
class MatrixSnip(s:Serviceable){
  def render(in:NodeSeq):NodeSeq = {
	  val services = s.getArchive
	  services.flatMap{(service:Service) => 
    Helpers.bind("matrix",in,
	"date" -%> Text(Misc.dateFormat.print(service.date1)),
      "type" -%> Text(service.type1.toString),
      "result" -%> Text(service.result)
	  )
	  }
  }
  def name(in:NodeSeq):NodeSeq = {
	  Text(s.manufacturer + " " + s.specification)
  }
}