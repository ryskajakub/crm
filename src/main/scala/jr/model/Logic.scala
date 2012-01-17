package jr.model

import _root_.net.liftweb.common._
import java.lang.String
import java.sql._
import com.twitter.querulous.query.NullValues
import com.twitter.querulous.evaluator.Transaction
import jr.spec.Misc
import org.joda.time.DateMidnight
import io.Source
import collection.immutable.List

/**
 * User: coub
 * Date: 12/18/11
 */

object Logic {
  def getMaxInterval = {
    Model.queryEvaluator.selectOne("select max(interval_days) from serviceable"){
      rs => rs.getInt(1)
    } getOrElse 0
  }

  def ensureCompanyExists(companyId: Long): Box[Serviceable] = {
    Model.queryEvaluator.selectOne("select company_id from company where company_id = ?", companyId) {
      rs => ""
    }.map(x => Serviceable(parentCompany = Full(Right(companyId))))
  }

  def ensureParentExists(serviceableId: Long): Box[Serviceable] = {
    Model.queryEvaluator.selectOne("select serviceable_id from serviceable where serviceable_id = ?", serviceableId) {
      rs => ""
    }.map(x => Serviceable(parentCompany = Full(Left(serviceableId))))
  }

  def getServiceableByName(x: String) = {
    Model.queryEvaluator.selectOne("" +
      "select *, count(*) as count " +
      "from serviceable " +
      "where specification = ?" +
      "group by interval_days " +
      "order by count desc " +
      "limit 0,1 ", x)(serviceableMapper)
  }


  def getNames(column: String, query: String): List[String] = {
    Model.queryEvaluator.select("select distinct %s from serviceable where %s like ?".format(column, column), "%" + query + "%") {
      rs => rs.getString(column)
    }.toList
  }


  def getServicemen(box: Box[Long]): List[Person] = {
    box.toList.flatMap(id =>
      Model.queryEvaluator.select("select * from service " +
        "join serviceman using(service_id) " +
        "join person on(person_id = serviceman_id) where service_id = ?", id)
        (personMapper)
    )
  }

  def getServicemenIdName = {
    Model.queryEvaluator.select("select person_id, name from person where is_serviceman") {
      rs => (rs.getLong("person_id"), rs.getString("name"))
    }
  }


  def getMachineHours(serviceable: Serviceable): Box[Int] = {
    serviceable.id.flatMap {
      (x: Long) =>
        Box(Model.queryEvaluator.selectOne("""
        select service_part.machine_hours from serviceable
        join service_part using(serviceable_id)
        join service using(service_id)
        where serviceable_id = ?
        order by service.date1 desc
        limit 0,1
      """, x) {
          rs => (Box !! rs.getString("machine_hours")).flatMap(Misc.parse2Int(_))
        }).flatten.headOption
    }
  }


  def getArchive(s: String): Box[(Box[(Service, List[(Option[Int], Serviceable)])], List[Service])] = {
    // companyID_[serviceId]
    if (s.length == 0) return Empty
    val rest = s.split("_")
    val sq = serviceQuery(Empty, rest.apply(0).toLong)
    val results = Model.queryEvaluator.select(sq._1, sq._2: _*)(serviceMapper)
    val pointer: Box[Service] = if (rest.length == 1) {
      results.lastOption
    } else {
      results.find((x: Service) => {
        (x.id.openOr("")).toString == rest(1)
      }).orElse(results.lastOption)
    }
    val serviceParts = pointer.map {
      (s: Service) => {
        val list = for {
          serviceId <- s.id.toList
          servicePart <- Model.queryEvaluator.select(""" select * from service_part join serviceable using(serviceable_id) where service_id = ? """, serviceId) {
            rs => Pair(
              Option(rs.getInt("machine_hours")),
              serviceableMapper(rs)
            )
          }
        } yield (servicePart)
        Pair(s, list)
      }
    }
    Full(Pair(serviceParts, results.toList))
  }

  def getService(s: String): Box[Service] = {
    Model.queryEvaluator.selectOne("select * from service where service_id = ?", s)(serviceMapper)
  }

  def getCompanies = {
    Model.queryEvaluator.select("select * from company")(companyMapper)
  }

  def indexMapper(rs: ResultSet) = {
    IndexMapped(
      company = rs.getString("names").split("\\|")(0), serviceDate = Option(rs.getDate("service_date")).map {
        new DateMidnight(_)
      }, companyId = rs.getLong("company_id"),
      priorityDays = rs.getInt("priority_days"), serviceType = Option(rs.getString("service_type")).flatMap(_.headOption)
    )
  }

  def index(sorting: Sorting) = {
    /*
   how to compute next service:
    + Either, some service is planned, so it overwrites default settings, or take the earlier date from these:
    +--+ there were some previous service and based on that + interval, there will be next service
       + or there is into_service + interval
       + or there is no next service
    */
    val s = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("index-select.sql")).mkString.format(sorting.sql)
    Model.queryEvaluator.select(s)(indexMapper)
  }

  def save(service: Service, list: List[PartHours], servicemen: Set[Long]) {
    if (list.isEmpty) return
    Model.queryEvaluator.transaction {
      (tr: Transaction) =>
        val id = service.id match {
          case Full(id) =>
            tr.execute("update service set date1 = ?, type1 = ?, result = ? where service_id = ?",
              Misc.mysqlDateFormat.print(service.date1), service.type1.toString,
              service.result, id)
            tr.execute("delete from service_part where service_id = ?", id)
            Full(id)
          case _ =>
            tr.execute("insert into service(date1, type1, result) values (?,?,?)",
              Misc.mysqlDateFormat.print(service.date1), service.type1.toString, service.result)
            lastInsertId(tr)
        }
        for {
          partHour <- list
          serviceableId <- partHour.idPart.toList
          serviceId <- id.toList
        } {
          tr.execute("insert into service_part(service_id,serviceable_id,machine_hours) values (?,?,?)", serviceId, serviceableId, partHour.machineHours.openOr(NullValues.NullInt))
        }
        for {
          serviceId <- id.toList
          servicemanId <- servicemen
        } {
          tr.execute("insert into serviceman(service_id,serviceman_id) values (?,?)", serviceId, servicemanId)
        }
    }
  }

  def getOuterPersonIdName: List[(Long, String)] = {
    Model.queryEvaluator.select("select person_id, name from person where company_id is null and person_id is not null")((rs: ResultSet) => {
      (rs.getLong("person_id"), rs.getString("name"))
    }).toList
  }

  def serviceQuery(serviceType: Box[Char], companyId: Long): (String, List[Any]) = {
    val st = serviceType match {
      case Full(c) => "and service.type1 = '%s'".format(c.toString)
      case _ => ""
    }
    val data = """
      service_id,
      service.date1 as service_date,
      company.company_id,
      company.names as company_names,
      service.type1 as service_type,
      result
    """
    ("""
    select * from (
    select
    %s
    from service
      join service_part using(service_id)
      join serviceable using(serviceable_id)
      join company using (company_id)
      where company.company_id = ? %s
    union
    select
    %s
    from service
      join service_part using(service_id)
      join serviceable as child on(service_part.serviceable_id = child.serviceable_id)
      join serviceable as parent on(child.parent_id = parent.serviceable_id)
      join company on (parent.company_id = company.company_id)
      where company.company_id = ? %s
     ) as a
     order by service_date asc
      """.format(data, st, data, st), List.fill(2)(companyId))
  }

  def findServiceFor(company: Company): Pair[Service, List[Long]] = {
    val maybeService = company.id.flatMap {
      (x: Long) => {
        val sq = serviceQuery(Full('p'), x)
        Model.queryEvaluator.selectOne(sq._1, sq._2: _*)(serviceMapper)
      }
    }
    val maybeWithServices = maybeService.map {
      (x: Service) =>
        val list = x.id match {
          case Full(id) =>
            Model.queryEvaluator.select(
              "select * from service " +
                "join service_part using(service_id) " +
                "join serviceable using(serviceable_id) " +
                "where service_id = ?", id)((rs: ResultSet) => {
              rs.getLong("serviceable_id")
            }).toList
          case _ => Nil
        }
        Pair(x, list)
    }
    maybeWithServices.getOrElse(Pair(Service.apply(), Nil))
  }

  def serviceMapper(rs: ResultSet): Service = {
    Service(id = Full(rs.getLong("service_id")), date1 = new DateMidnight(rs.getDate("service_date")),
      type1 = rs.getString("service_type").head, result = rs.getString("result"), companyId = rs.getLong("company_id"))
  }

  def machinesForCompany(companyId: Long): List[Serviceable] = {
    Model.queryEvaluator.transaction(tr => {
      val mainOnes = tr.select("select * from serviceable where company_id = ?", companyId)(serviceableMapper)
      mainOnes.flatMap((s: Serviceable) => {
        s :: tr.select("select * from serviceable where parent_id = ?", s.id.getOrElse(0))(serviceableMapper).toList
      }).toList
    })
  }

  def save(serviceable: Serviceable): Box[Serviceable] = {
    val (name, value) = serviceable.parentCompany match {
      case Full(Left(parent)) => ("parent_id", parent)
      case Full(Right(company)) => ("company_id", company)
      case _ => return Empty
    }
    val sureNamesList = List(name, "type1", "specification", " into_service", "serial_number",
      "manufacturer", "power", "note", "interval_days", "date_sold")
    val sureValues = value :: serviceable.productIterator.drop(2).toList.map {
      case Full(x: DateMidnight) => Misc.mysqlDateFormat.print(x)
      case Full(x) => x.toString
      case x: Box[Double] => NullValues.NullDouble
      case x: Box[Int] => NullValues.NullInt
      case x: Box[DateMidnight] => NullValues.NullTimestamp
      case x: Box[String] => NullValues.NullString
      case x => x
    }
    serviceable.id match {
      case Full(id) =>
        val setter = sureNamesList.map(_ + " = ?").mkString(",")
        val setterString = String.format("update serviceable set %s where serviceable_id = ?", setter)
        Model.queryEvaluator.execute(setterString, (sureValues :+ id): _*)
        Full(serviceable)
      case _ =>
        Model.queryEvaluator.transaction {
          tr =>
            val str = sureNamesList.mkString(",")
            val ct1 = sureNamesList.size
            val ct2 = sureValues.size
            tr.execute(String.format("insert into serviceable (%s) values(?,?,?,?,?,?,?,?,?,?)", str), sureValues: _*)
            Full(serviceable.copy(id = lastInsertId(tr)))
        }
    }
  }

  def getParentsIdNameCompany: Seq[(Long, String)] = {
    Model.queryEvaluator.select("select note, serviceable_id, company.names as company_names," +
      " specification from company join serviceable using(company_id)") {
      rs => {
        def sub(x: String) = {
          x.substring(0, if (x.size < 10) x.size else 10)
        }
        (rs.getLong("serviceable_id"), sub(rs.getString("company_names").split("\\|")(0)) + "|"
          + sub(rs.getString("specification")) + "|" + sub(rs.getString("note")))
      }
    }
  }


  def serviceableMapper(rs: ResultSet) = {
    val parent = rs.getLong("parent_id")
    val company = rs.getLong("company_id")
    val pc = (parent, company) match {
      case (x, 0L) => Full(Left(x))
      case (0L, x) => Full(Right(x))
      case _ => Empty
    }
    Serviceable(
      id = Full(rs.getLong("serviceable_id")), type1 = rs.getString("type1"),
      specification = rs.getString("specification"), serialNumber = Option(rs.getString("serial_number")),
      power = Option(rs.getDouble("power")).flatMap(x => if (x > 0.0) Some(x) else None), parentCompany = pc,
      intoService = Option(rs.getDate("into_service")).map(d => new DateMidnight(d)),
      dateSold = Option(rs.getDate("date_sold")).map(d => new DateMidnight(d)),
      intervalDays = rs.getInt("interval_days"),
      manufacturer = rs.getString("manufacturer"), note = rs.getString("note")
    )
  }

  def getServiceable(s: String): Serviceable = {
    Model.queryEvaluator.selectOne("select * from serviceable where serviceable_id = ?", s)(serviceableMapper) getOrElse (Serviceable.apply())
  }

  def personMapper(rs: ResultSet): Person = {
    Person(id = Full(rs.getLong("person_id")), name = rs.getString("name"), fax = rs.getString("fax"),
      cellPhone = rs.getString("cell_phone"), telephone = rs.getString("telephone"), mail = rs.getString("mail"),
      companyId = Full(rs.getInt("company_id")), position = rs.getString("position"), isServiceman = rs.getBoolean("is_serviceman"))
  }

  def getPeopleForCompany(l: Long): List[Person] = {
    Model.queryEvaluator.select("select * from company join person using(company_id) where company_id = ?", l)(personMapper).toList
  }

  def getCompaniesIdName = {
    Model.queryEvaluator.select("select company_id, names from company") {
      rs => Pair(rs.getLong("company_id"), rs.getString("names").split("\\|")(0))
    }
  }

  def getPerson(x: Any): Person = {
    (Model.queryEvaluator.selectOne("select * from person where person_id = ?", x)(personMapper)).getOrElse(Person.apply())
  }

  def save(person2: Person): Box[Person] = {
    val person = person2.copy(companyId = person2.companyId.flatMap {
      (x: Long) => if (x == 0L) Empty else Full(x)
    })
    person.id match {
      case (Full(id)) =>
        Model.queryEvaluator.execute("update person set fax = ?, name = ?, telephone = ?, is_serviceman = ?, " +
          "cell_phone = ?, position = ?, company_id = ?, mail = ? where person_id = ?",
          person.fax, person.name, person.telephone, person.isServiceman, person.cellPhone, person.position,
          person.companyId openOr NullValues.NullLong, person.mail, id)
        Full(person)
      case _ =>
        Model.queryEvaluator.transaction {
          tr =>
            tr.execute("insert into person(fax, name, telephone, cell_phone, position, company_id, mail, is_serviceman) " +
              "values(?,?,?,?,?,?,?,?)",
              person.fax, person.name, person.telephone, person.cellPhone, person.position,
              person.companyId openOr NullValues.NullLong, person.mail, person.isServiceman)
            Full(person.copy(id = lastInsertId(tr)))
        }
    }
  }

  def lastInsertId(t: Transaction): Box[Long] = {
    t.selectOne[Long]("select LAST_INSERT_ID()") {
      rs =>
        rs.getLong(1)
    }
  }

  def save(company: Company): Company = {
    company.id match {
      case Full(id) =>
        Model.queryEvaluator.execute("update company set address = ?, names = ?, show_in_index = ?, priority_days = ? where company_id = ?",
          company.address, company.names, company.showInIndex, company.priorityDays, id)
        company
      case _ =>
        Model.queryEvaluator.transaction {
          tr =>
            tr.execute("insert into company(address, names, show_in_index, priority_days) values(?,?,?,?)",
              company.address, company.names, company.showInIndex, company.priorityDays)
            company.copy(id = lastInsertId(tr))
        }
    }
  }

  def companyMapper(rs: ResultSet) = {
    Company(id = Option(rs.getLong("company_id")), names = rs.getString("names"), address = rs.getString("address"),
      showInIndex = rs.getBoolean("show_in_index"), priorityDays = rs.getInt("priority_days"))
  }

  def getCompany(x: String): Company = {
    Model.queryEvaluator.selectOne("select * from company where company_id = ?", x)(companyMapper).getOrElse(Company.apply())
  }

}
