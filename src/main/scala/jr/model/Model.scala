package jr.model

import com.twitter.querulous.evaluator._
import _root_.net.liftweb.common._
import org.joda.time.DateMidnight

case class Sorting(sql:String = "COALESCE(service_date,'9999-01-01') asc", repr:String = "indexAsc")

object Sorting{
  val find = Map(mapifySql(Sorting()),mapifySql(SortingCompanyAsc),mapifySql(SortingCompanyDesc))
  def mapifySql(s:Sorting) = (s.repr,s)
}

case object SortingCompanyAsc extends Sorting("names asc","companyAsc")
case object SortingCompanyDesc extends Sorting("names desc","companyDesc")

class PartHours(var idPart: Box[Long], var machineHours: Box[Int])

case class IndexMapped(company: String, serviceDate: Option[DateMidnight] = None, serviceType: Option[Char],
                       companyId: Long, priorityDays: Int)

case class Service(id: Box[Long] = Empty, date1: DateMidnight = new DateMidnight(), planned: Boolean = true,
                   result: String = "", type1: Char = 'p', companyId: Long = 0L) {
  def getServicemen = {
    Logic.getServicemen(id)
  }
}

case class ServicePart(serviceId: Long, machineId: Long, machineHours: Option[Int] = Empty)

case class Company(id: Box[Long] = Empty, names: String = "", address: String = "",
                   showInIndex: Boolean = true, priorityDays: Int = 0)

case class Person(id: Box[Long] = Empty, name: String = "", position: String = "", telephone: String = "",
                  cellPhone: String = "", fax: String = "", mail: String = "", companyId: Box[Long] = Empty,
                  isServiceman: Boolean = true)

case class Serviceable(id: Box[Long] = Empty,
                       parentCompany: Box[Either[Long, Long]] = Empty, // left is parent, right is company
                       type1: String = "", specification: String = "",
                       intoService: Box[DateMidnight] = Empty, serialNumber: Box[String] = Empty,
                       manufacturer: String = "", power: Box[Double] = Empty, note: String = "",
                       intervalDays: Int = 365, dateSold: Box[DateMidnight] = Empty)

object Model {
  // create database crm character set utf8 collate utf8_czech_ci
  val queryEvaluator = QueryEvaluator("localhost", "crm", "crm", "crm",
    Map("characterEncoding" -> "UTF-8", "characterSetResults" -> "UTF-8"))
  val config = """
    config(
      key1 VARCHAR(10) PRIMARY KEY,
      value1 VARCHAR(1000)
    )
  """
  val company = """
    company(
      company_id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
      names VARCHAR(500) NOT NULL,
      address VARCHAR(500),
      show_in_index BOOLEAN,
      priority_days INTEGER UNSIGNED,
      delay VARCHAR(100)
    )
  """
  val person = """
    person(
      person_id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
      position VARCHAR(50),
      telephone VARCHAR(50),
      cell_phone VARCHAR(50),
      fax VARCHAR(50),
      mail VARCHAR(100),
      company_id INTEGER UNSIGNED,
      is_serviceman BOOLEAN NOT NULL,
      name VARCHAR(200) NOT NULL,
      FOREIGN KEY (company_id) REFERENCES company(company_id)
    )
  """
  val serviceable = """
    serviceable(
      serviceable_id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
      parent_id INTEGER UNSIGNED,
      company_id INTEGER UNSIGNED,
      manufacturer VARCHAR(100) NOT NULL,
      into_service DATE,
      date_sold DATE,
      serial_number VARCHAR(100),
      power DOUBLE,
      type1 VARCHAR(100),
      note VARCHAR(2000) NOT NULL,
      specification VARCHAR(100) NOT NULL,
      interval_days INT,
      FOREIGN KEY (company_id) REFERENCES company(company_id),
      FOREIGN KEY (parent_id) REFERENCES serviceable(serviceable_id)
    )
  """
  val service = """
    service(
      service_id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
      date1 DATE NOT NULL,
      type1 CHAR(1) NOT NULL,
      result VARCHAR(1000)
    )
  """
  val servicePart = """
    service_part(
      service_id INTEGER UNSIGNED,
      machine_hours INTEGER UNSIGNED,
      serviceable_id INTEGER UNSIGNED,
      PRIMARY KEY(service_id,serviceable_id),
      FOREIGN KEY(service_id) REFERENCES service(service_id),
      FOREIGN KEY(serviceable_id) REFERENCES serviceable(serviceable_id)
    )
  """
  val serviceman = """
    serviceman(
      serviceman_id INTEGER UNSIGNED,
      service_id INTEGER UNSIGNED,
      PRIMARY KEY(service_id,serviceman_id),
      FOREIGN KEY(service_id) REFERENCES service(service_id),
      FOREIGN KEY(serviceman_id) REFERENCES person(person_id)
    )
  """
  val inserts = List[(String, List[String])]()
  val before = "create table if not exists "
  val after = "engine = InnoDB"
  val tables = List(company, person, service, serviceable, servicePart, serviceman)

  def init() {
    queryEvaluator.transaction {
      tr =>
        for (t <- tables) {
          try {
            tr.execute(before + t + after)
          } catch {
            case tr: Throwable =>
              println(t)
              println(tr)
          }
        }
    }
  }

  for (i <- inserts) {
    queryEvaluator.transaction {
      tr =>
        try {
          tr.execute(i._1, i._2: _*)
        } catch {
          case t: Throwable =>
        }
    }
  }
}

