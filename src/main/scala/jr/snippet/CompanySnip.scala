package jr.snippet

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import jr.model._
import Helpers._
import collection.immutable.List
import java.lang.String
import jr.spec._
import js.JsCmds
import scala.collection.mutable.{Set => MSet}
import org.joda.time.DateMidnight
import xml.{Text, NodeSeq}

/**
 * User: coub
 * Date: 12/18/11
 * Time: 5:30 PM
 */

class ServiceableSnip(s: Serviceable) {
  var serviceable = s

  def render(in: NodeSeq): NodeSeq = {
    val emptyParent = (0L -> "Žádný")
    val emptyCompany = (0L -> "Žádná")
    val parentsAll = Logic.getParentsIdNameCompany.toList
    val parents: List[(Long, String)] = emptyParent :: (s.id.map {
      (x: Long) => parentsAll.filterNot(_._1 == x)
    }.getOrElse(parentsAll))
    val companies: List[(Long, String)] = emptyCompany :: Logic.getCompaniesIdName.toList
    val parentOrCompany: Pair[Long, Long] = s.parentCompany match {
      case Full(Right(l)) => (0L, l) // rightIsCompany
      case Full(Left(l)) => (l, 0L) // leftIsParent
      case _ => (0L, 0L)
    }
    Helpers.bind(
      "input", in,
      "manufacturer" -%> SHtml.text(s.manufacturer, (x: String) => serviceable = serviceable.copy(manufacturer = x)),
      "parent" -%> SHtml.selectObj[Long](parents, Full(parentOrCompany._1), (l: Long) => {
        if (l != 0 && serviceable.parentCompany.isEmpty) {
          serviceable = serviceable.copy(parentCompany = Full(Left(l)))
        }
      }),
      "company" -%> SHtml.selectObj(companies, Full(parentOrCompany._2),
        (x: Long) => {
          if (x != 0 && serviceable.parentCompany.isEmpty) {
            serviceable = serviceable.copy(parentCompany = Full(Right(x)))
          }
        }),
      "dateSold" -%> SHtml.text(s.dateSold.map(_.toString(Misc.dateFormat)).openOr(""),
        (x: String) => serviceable = serviceable.copy(dateSold = Misc.parse2JodaDate(x))),
      "intoService" -%> SHtml.text(s.intoService.map(_.toString(Misc.dateFormat)).openOr(""),
        (x: String) => serviceable = serviceable.copy(intoService = Misc.parse2JodaDate(x))),
      "serialNumber" -%> SHtml.text(s.serialNumber.openOr(""),
        (x: String) => serviceable = serviceable.copy(serialNumber = if (x.equals("")) Empty else Full(x))),
      "power" -%> SHtml.text(s.power.map((x: Double) => {
        if (x == 0.0) "" else x.toString
      }).openOr(""),
        (x: String) => serviceable = serviceable.copy(power = Misc.parse2Double(x))),
      "type" -%> SHtml.text(s.type1, (x: String) => serviceable = serviceable.copy(type1 = x)),
      "specification" -%> SHtml.text(s.specification,
        (x: String) => serviceable = serviceable.copy(specification = x)),
      "note" -%> SHtml.text(s.note, (x: String) => serviceable = serviceable.copy(note = x)),
      "interval" -%> SHtml.text(s.intervalDays.toString,
        (x: String) => serviceable = serviceable.copy(intervalDays = Misc.parse2Int(x).openOr(0))),
      "submit" -%> SHtml.submit("Ulož!", () => {
        val saved = Logic.save(serviceable)
        saved match {
          case Full(se) =>
            S.redirectTo(Spec.serviceable.toLoc.calcHref(se))
          case _ =>
        }
      })
    )
  }
}

class CompanySnip(c: Company) {
  var company = c
  var (service, listServiceParts) = Logic.findServiceFor(c)

  def archive = {
    c.id match {
      case Full(f) =>
        "a [href]" #> Spec.archive.toLoc.calcHref(Pair(Full(Service(companyId = f), Nil), Nil)) &
          "span" #> ""
      case _ =>
        "a" #> ""
    }
  }

  def machines(in: NodeSeq): NodeSeq = {
    c.id match {
      case Full(id) =>
        val machinesForCompany = Logic.machinesForCompany(id).map {
          Pair(_, new PartHours(Empty, Empty))
        }
        val default = 0L
        val empty = (default -> "Vyber")
        val selectServiceMan = empty :: Logic.getServicemenIdName.toList
        val servicemanIds = MSet[Long]()
        var servicemanEntry = NodeSeq.Empty
        Helpers.bind("input", in,
          "newMachine" -%> {
            "a [href]" #> Spec.serviceable.toLoc.calcHref(Serviceable(parentCompany = Full(Right(id))))
          },
          "row" -%> {
            (xhtml: NodeSeq) =>
              machinesForCompany.flatMap((x) => {
                val (serviceable, part) = x
                val machineHours = Logic.getMachineHours(serviceable)
                Helpers.bind("machine", xhtml,
                  "addPart" -%> {
                    if (serviceable.parentCompany.get.isLeft) {
                      "a" #> ""
                    } else {
                      "a [href]" #> Spec.serviceable.toLoc.calcHref(Serviceable(parentCompany = serviceable.id.map(Left(_))))
                    }
                  },
                  "type" -%> {
                    "a *" #> serviceable.specification &
                      "a [href]" #> Spec.serviceable.toLoc.calcHref(Serviceable(id = serviceable.id))
                  },
                  "machineHours" -%> SHtml.text(Misc.readable(machineHours), (x) => part.machineHours = Misc.parse2Int(x)),
                  "manufacturer" -%> Text(serviceable.manufacturer),
                  "checkbox" -%> SHtml.checkbox(listServiceParts.contains(serviceable.id.getOrElse(Empty)), (b: Boolean) => if (b) {
                    part.idPart = serviceable.id
                  })
                )
              }
              )
          },
          "selectServiceman" -%> SHtml.ajaxSelectObj(selectServiceMan, Full(default), (x: Long) => {
            if (x == default || servicemanIds.contains(x)) JsCmds.Noop
            else {
              def encodeServiceManId(id: Long) = "serviceman" + id
              servicemanIds += x
              val serviceman = Logic.getPerson(x)
              val transform: NodeSeq => NodeSeq = {
                val csss =
                  "#name" #> serviceman.name &
                    "a [onclick]" #> SHtml.onEvent {
                      (y) =>
                        servicemanIds -= x
                        JsCmds.Replace(encodeServiceManId(x), NodeSeq.Empty)
                    } &
                    "a *" #> "-"
                serviceman.id match {
                  case Full(id) =>
                    csss &
                      "li [id]" #> (encodeServiceManId(x))
                  case _ =>
                    csss
                }
              }
              val output = transform(servicemanEntry)
              JsCmds.Replace("servicemanPlaceholder", output)
            }
          }),
          "servicemanEntry" -%> {
            (x: NodeSeq) => {
              servicemanEntry = x
              NodeSeq.Empty
            }
          },
	       "type" -%> SHtml.selectObj(List(('n' -> "normální"), ('o' -> "oprava"), ('p' -> "plánovaná")), Full(service.type1), {
            (x: Char) => {
              service = service.copy(type1 = x)
            }
          }),
          "date" -%> SHtml.text(service.date1.toString(Misc.dateFormat),
            (x: String) => service = service.copy(date1 = Misc.parse2JodaDate(x) openOr new DateMidnight())),
          "result" -%> SHtml.text(service.result, (x: String) => service = service.copy(result = x)),
          "submit" -%> SHtml.submit("Ulož!", () => {
            val serviceSaved = Logic.save(service, machinesForCompany.map {
              _._2
            }, servicemanIds.toSet)
            S.redirectTo(Spec.company.toLoc.calcHref(company))
          })
        )
      case _ =>
        NodeSeq.Empty
    }
  }

  def render(in: NodeSeq): NodeSeq = {
    Helpers.bind(
      "input", in,
      "names" -%> SHtml.text(c.names, (x: String) => company = company.copy(names = x)),
      "address" -%> SHtml.text(c.address, (x: String) => company = company.copy(address = x)),
      "priorityDays" -%> SHtml.text(c.priorityDays.toString, (x: String) => company = company.copy(priorityDays = Misc.parse2Int(x) openOr 0)),
      "showInIndex" -%> SHtml.checkbox(c.showInIndex, (x: Boolean) => company = company.copy(showInIndex = x)),
      "submit" -%> SHtml.submit("Ulož!", () => {
        val companySaved = Logic.save(company)
        S.redirectTo(Spec.company.toLoc.calcHref(companySaved))
      })
    )
  }

  def people(in: NodeSeq): NodeSeq = {
    c.id match {
      case Full(id) =>
        val peopleForCompany: List[Person] = Logic.getPeopleForCompany(id)
        Helpers.bind(
          "people",
          in,
          "newPerson" -%> "a [href]" #> Spec.person.toLoc.calcHref(Person(companyId = Full(id))),
          "elem" -%>
            "li" #> peopleForCompany.map((x: Person) => {
              "a [href]" #> Spec.person.toLoc.calcHref(x) &
                "a *" #> x.name
            })
        )
      case _ =>
        NodeSeq.Empty
    }
  }

}

class PersonSnip(p: Person) {
  var person = p

  def render(in: NodeSeq): NodeSeq = {
    val selectedCompany = Full(p.companyId openOr 0L)
    val companies = (Pair(0L, "Žádná firma") :: Logic.getCompaniesIdName.toList)
    Helpers.bind(
      "input", in,
      "name" -%> SHtml.text(p.name, (x: String) => person = person.copy(name = x)),
      "fax" -%> SHtml.text(p.fax, (x: String) => person = person.copy(fax = x)),
      "cellPhone" -%> SHtml.text(p.cellPhone, (x: String) => person = person.copy(cellPhone = x)),
      "telephone" -%> SHtml.text(p.telephone, (x: String) => person = person.copy(telephone = x)),
      "position" -%> SHtml.text(p.position, (x: String) => person = person.copy(position = x)),
      "mail" -%> SHtml.text(p.mail, (x: String) => person = person.copy(mail = x)),
      "isServiceman" -%> SHtml.checkbox(p.isServiceman, (x: Boolean) => person = person.copy(isServiceman = x)),
      "company" -%> SHtml.selectObj(companies, selectedCompany, (x: Long) => person = person.copy(companyId = Full(x))),
      "submit" -%> SHtml.submit("Ulož!", () => {
        val personSaved = Logic.save(person)
        personSaved match {
          case Full(p) =>
            S.redirectTo(Spec.person.toLoc.calcHref(p))
          case _ =>
        }
      })
    )
  }
}

