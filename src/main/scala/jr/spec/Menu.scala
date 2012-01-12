package jr.spec

/**
 * User: coub
 * Date: 12/18/11
 * Time: 5:07 PM
 */

import _root_.net.liftweb.common._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import jr.model._

object Spec {
  val index = Menu.param[Sorting]("index", "index", (x: String) => {
    Sorting.find.lift(x) orElse Some(Sorting())
  }, (x => x.repr)) / "index" >> Value(Sorting())
  val company = Menu.param[Company]("company", "company", (x: String) => Full(Logic.getCompany(x)),
    (x: Company) => (x.id.map(_.toString).openOr("new"))) / "company" >> Value(Company.apply())
  val service = Menu.param[Service]("service", "service", (x: String) => Logic.getService(x),
    (x: Service) => (x.id.map(_.toString).openOr("new"))) / "service" >> Hidden
  val person = Menu.param[Person]("person", "person", (x: String) => Full(Logic.getPerson(x)),
    (x: Person) => (x.id.map(_.toString).openOr("new"))) / "person" >> Value(Person.apply())
  val serviceable = Menu.param[Serviceable]("serviceable", "serviceable",
    (x: String) => {
      x.toList match {
        case '#' :: (companyId@(_)) =>
          Misc.parse2Int(companyId.mkString).flatMap {
            (x: Int) => Logic.ensureCompanyExists(x.toLong)
          }
        case '$' :: (parentId@(_)) =>
          Misc.parse2Int(parentId.mkString).flatMap {
            (x: Int) => Logic.ensureParentExists(x.toLong)
          }
        case _ => Full(Logic.getServiceable(x))
      }
    },
    (x: Serviceable) => {
      x.parentCompany match {
        case Full(Right(companyId)) => "#" + companyId
        case Full(Left(parentId)) => "$" + parentId
        case _ => (x.id.map(_.toString).openOr("new"))
      }
    }) / "serviceable" >> Value(Serviceable.apply())
  val archive = Menu.param[(Box[(Service, List[(Option[Int], Serviceable)])], List[Service])]("archive", "archive", (x: String) => Logic.getArchive(x),
    (x: (Box[(Service, List[(Option[Int], Serviceable)])], List[Service])) => {
      val service = x._1
      service match {
        case Full(Pair(ser, y)) =>
          (ser.companyId.toString) + ser.id.map {
            "_" + _
          }.getOrElse("")
        case _ =>
          "error"
      }
    }) / "archive" >> Hidden

  def entries = {
    List(index, company, person, serviceable, service, archive)
  }
}