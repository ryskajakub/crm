package jr.snippet

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import jr.model._
import Helpers._
import jr.spec._
import scala.collection.mutable.{Set => MSet}
import xml.{Text, NodeSeq}

/**
 * User: coub
 * Date: 12/27/11
 * Time: 12:49 AM
 */

class Index(sorting: Sorting) {

  def render(xhtml: NodeSeq) = {
    val values = Logic.index(sorting)
    Helpers.bind("index", xhtml,
      "header" -%> {
        "#companyAsc [href]" #> Spec.index.toLoc.calcHref(SortingCompanyAsc) &
          "#companyDesc [href]" #> Spec.index.toLoc.calcHref(SortingCompanyDesc) &
          "#indexAsc [href]" #> Spec.index.toLoc.calcHref(Sorting()) &
          "#indexDesc [href]" #> Spec.index.toLoc.calcHref(SortingDesc.apply)
      },
      "row" -%> {
        (in: NodeSeq) =>
          values.flatMap((im: IndexMapped) => {
            lazy val maxInterval = Logic.getMaxInterval
            Helpers.bind(
              "entry", in,
              "rowStart" -%> {
                val serviceType =
                  im.serviceType match {
                    case Some('p') => "planned"
                    case Some(_) => "delay"
                    case _ => "none"
                  }
                val classSelector = "td [class]" #> serviceType
                if (serviceType == "delay") {
                  val number = (255 - (255.0 / maxInterval * im.priorityDays).toInt).toString
                  "td [style]" #> "background-color:rgb(255,%s,%s)".format(number, number) &
                    classSelector
                } else {
                  classSelector
                }
              },
              "realDate" -%> Text(im.realServiceDate.map {
                Misc.dateFormat.print(_)
              }.getOrElse("n/a")),
              "company" -%> {
                "a *" #> im.company.name.split("\\|")(0) &
                  "a [href]" #> Spec.company.toLoc.calcHref(Company(id = Full(im.companyId)))
              },
              "date" -%> Text(im.serviceDate.map {
                Misc.dateFormat.print(_)
              }.getOrElse("n/a"))
            )
          })
      }
    )
  }
}
