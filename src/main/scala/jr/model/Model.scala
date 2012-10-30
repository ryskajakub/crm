package jr.model

import _root_.net.liftweb.common._
import org.joda.time.DateMidnight
import javax.naming.Context
import javax.naming.InitialContext
import javax.sql.DataSource
import java.sql.ResultSet
import org.apache.commons.dbutils.{ResultSetHandler, QueryRunner}

case class Sorting(sql: String = "COALESCE(service_date,'0000-01-01') asc", repr: String = "indexAsc")

object SortingDesc {
  def apply = Sorting(sql = "COALESCE(service_date,'9999-01-01') desc", repr = "indexDesc")
}

object Sorting {
  val find = Map(mapifySql(Sorting()), mapifySql(SortingCompanyAsc), mapifySql(SortingCompanyDesc), mapifySql(SortingDesc.apply))

  def mapifySql(s: Sorting) = (s.repr, s)
}

case object SortingCompanyAsc extends Sorting("names asc", "companyAsc")

case object SortingCompanyDesc extends Sorting("names desc", "companyDesc")

class PartHours(var idPart: Box[Long], var machineHours: Box[Int])

case class IndexMapped(company: String, serviceDate: Option[DateMidnight] = None, serviceType: Option[Char],
                       companyId: Long, priorityDays: Int, realServiceDate: Option[DateMidnight])

case class Service(id: Box[Long] = Empty, date1: DateMidnight = new DateMidnight(), planned: Boolean = true,
                   result: String = "", type1: Char = 'n', companyId: Long = 0L) {
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
                       intervalDays: Int = 365, dateSold: Box[DateMidnight] = Empty){
	def getCompany:Box[Company] = {
		parentCompany.flatMap((either:Either[Long,Long]) => {
				val compBox:Box[Company] = either match {
					case Right(companyId) =>
						Full(Logic.getCompany(companyId.toString))
					case Left(parentId) =>
						val svc = Logic.getServiceable(parentId.toString)
						val prnt = svc.id.map{(x:Long) => Logic.getServiceable(x.toString)}
						val companyBox = prnt.flatMap{(x:Serviceable) => x.getCompany}
						companyBox
				}
				compBox
			})
	}
		       }

class Model{

  def funToResultSetHandler[A](x: ResultSet => A): ResultSetHandler[Box[A]] = {
    new ResultSetHandler[Box[A]] {
      def handle(p1: ResultSet): Box[A] = {
        if (p1.next()) {
          Full[A](x.apply(p1))
        } else {
          Empty
        }
      }
    }
  }

  def funToResultSetHandlerMany[A](x: ResultSet => A): ResultSetHandler[List[A]] = {
    new ResultSetHandler[List[A]] {
      def handle(p1: ResultSet) = {
        def addList(list: List[A]): List[A] = {
          if (p1.next()) {
            addList(x.apply(p1) :: list)
          } else {
            list
          }
        }
        addList(Nil)
      }
    }
  }

	/*
	import javax.annotation.Resource;
	@Resource(name = "")
	val ds:DataSource = null
	*/
var initContext:InitialContext = new InitialContext()
       /*
var envContext:Context = initContext.lookup("java:comp/env").asInstanceOf[Context]
var ds:DataSource = envContext.lookup("jdbc/TestDB").asInstanceOf[DataSource]
*/
var ds:DataSource = initContext.lookup("jdbc/TestDB").asInstanceOf[DataSource]
	/*
  Class.forName("com.mysql.jdbc.Driver")
  val props = new Properties
  props.setProperty("user", "jaku5_glassfish")
  props.setProperty("password", "mGZbDuJjVj")
  props.setProperty("characterEncoding", "UTF-8")
  props.setProperty("characterSetResults", "UTF-8")
  val url = "jdbc:mysql://mysql:3306/jakubryska_name_glassfish"
  val connectionFactory = new DriverManagerConnectionFactory(url, props)
  val connectionPool = new GenericObjectPool(null);
  val poolableFactory = new PoolableConnectionFactory(connectionFactory, connectionPool, null, null, false, true)
  val poolingDataSource = new PoolingDataSource(connectionPool)
  val queryRunner = new QueryRunner(poolingDataSource)
  */
  val queryRunner = new QueryRunner(ds)
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
  val before = "create table if not exists "
  val after = "engine = InnoDB"
  val tables = List(company, person, service, serviceable, servicePart, serviceman)

  def initDB() {
    for (t <- tables) {
      try {
        queryRunner.update(before + t + after)
      } catch {
        case tr: Throwable =>
          println(t)
          println(tr)
      }
    }
  }

}

object Model extends Model