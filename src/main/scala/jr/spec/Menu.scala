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
	val matrixArchive = Menu.param[Serviceable]("matrixArchive","Tabulka - archive",(x:String) => Full(Logic.getServiceable(x)), (x:Serviceable) => (x.id.map(_.toString).openOr("new"))) / "matrixArchive" >> Hidden
  val index = Menu.param[Sorting]("index", "Seznam", (x: String) => {
    Sorting.find.lift(x) orElse Some(Sorting())
  }, (x => x.repr)) / "index" >> Value(Sorting())
  val company = Menu.param[Company]("company", "Nová Firma", (x: String) => Full(Logic.getCompany(x)),
    (x: Company) => (x.id.map(_.toString).openOr("new"))) / "company" >> Value(Company.apply())
  val service = Menu.param[Service]("service", "Servis", (x: String) => Logic.getService(x),
    (x: Service) => (x.id.map(_.toString).openOr("new"))) / "service" >> Hidden
  val person = Menu.param[Person]("person", "Nová Osoba",
    (x: String) => {
      x.toList match {
        case '#' :: (companyId@(_)) =>
          Misc.parse2Int(companyId.mkString).flatMap {
            (x: Int) => Logic.ensurePersonExists(x.toLong)
          }
        case _ => Full(Logic.getPerson(x).copy(isServiceman = false))
      }
    } ,
    (x: Person) => {
      (x.id, x.companyId) match {
        case (Full(id), _) =>
          id.toString
        case (_, Full(companyId)) =>
          '#' + companyId.toString
        case _ =>
          "new"
      }
    }) / "person" >> Value(Person.apply(isServiceman = false))
  val serviceable = Menu.param[Serviceable]("serviceable", "Kompresor",
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
    }) / "serviceable" >> Value(Serviceable.apply()) >> Hidden
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
	  List(index, company, person, serviceable, service, archive, matrixArchive)
  }
}