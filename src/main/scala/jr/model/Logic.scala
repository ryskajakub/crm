package jr.model

import _root_.net.liftweb.common._
import java.lang.String
import java.sql._
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
    Model.queryRunner.query("select max(interval_days) from serviceable",
      Model.funToResultSetHandler(rs => rs.getInt(1))
    ) getOrElse 0
  }

  def ensurePersonExists(companyId: Long): Box[Person] = {
    Model.queryRunner.query("select company_id from company where company_id = ?",
      Model.funToResultSetHandler(rs => ""), companyId.asInstanceOf[AnyRef])
      .map((x: String) => Person(companyId = Full(companyId)))
  }

  def ensureCompanyExists(companyId: Long): Box[Serviceable] = {
    Model.queryRunner.query("select company_id from company where company_id = ?",
      Model.funToResultSetHandler(rs => ""), companyId.asInstanceOf[AnyRef])
      .map((x: String) => Serviceable(parentCompany = Full(Right(companyId))))
  }

  def ensureParentExists(serviceableId: Long): Box[Serviceable] = {
    Model.queryRunner.query("select serviceable_id from serviceable where serviceable_id = ?",
      Model.funToResultSetHandler[String](rs => ""), serviceableId.asInstanceOf[AnyRef])
      .map((x: String) => Serviceable(parentCompany = Full(Left(serviceableId))))
  }

  def getServiceableByName(x: String) = {
    Model.queryRunner.query(
      "select *, count(*) as count " +
        "from serviceable " +
        "where specification = ?" +
        "group by interval_days " +
        "order by count desc " +
        "limit 0,1 ", Model.funToResultSetHandler(serviceableMapper), x)
  }


  def getNames(column: String, query: String): List[String] = {
    Model.queryRunner.query("select distinct %s from serviceable where %s like ?"
      .format(column, column), "%" + query + "%",
      Model.funToResultSetHandlerMany(rs => rs.getString(column)))
  }


  def getServicemen(box: Box[Long]): List[Person] = {
    box.toList.flatMap(id =>
      Model.queryRunner.query("select * from service " +
        "join serviceman using(service_id) " +
        "join person on(person_id = serviceman_id) where service_id = ?",
        Model.funToResultSetHandlerMany(personMapper), id.asInstanceOf[AnyRef])
    )
  }

  def getServicemenIdName = {
    Model.queryRunner.query("select person_id, name from person where is_serviceman",
      Model.funToResultSetHandlerMany(rs => (rs.getLong("person_id"), rs.getString("name"))))
  }


  def getMachineHours(serviceable: Serviceable): Box[Int] = {
    serviceable.id.flatMap {
      (x: Long) =>
        Box(Model.queryRunner.query("""
        select service_part.machine_hours from serviceable
        join service_part using(serviceable_id)
        join service using(service_id)
        where serviceable_id = ?
        order by service.date1 desc
        limit 0,1
      """,
          Model.funToResultSetHandler(
            rs => (Box !! rs.getString("machine_hours")).flatMap(Misc.parse2Int(_))
          ), x.asInstanceOf[AnyRef])).flatten.headOption
    }
  }


  def getArchive(s: String): Box[(Box[(Service, List[(Option[Int], Serviceable)])], List[Service])] = {
    // companyID_[serviceId]
    if (s.length == 0) return Empty
    val rest = s.split("_")
    val sq = serviceQuery(Empty, rest.apply(0).toLong)
    val results = Model.queryRunner.query(sq._1, Model.funToResultSetHandlerMany(serviceMapper),
      sq._2.map(_.asInstanceOf[AnyRef]): _*)
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
          servicePart <- Model.queryRunner.query(
            """ select * from service_part join serviceable using(serviceable_id) where service_id = ? """,
            Model.funToResultSetHandlerMany(
              rs => Pair(
                Option(rs.getInt("machine_hours")),
                serviceableMapper(rs)
              ))
            , serviceId.asInstanceOf[AnyRef])
        } yield (servicePart)
        Pair(s, list)
      }
    }
    Full(Pair(serviceParts, results.toList))
  }

  def getService(s: String): Box[Service] = {
    Model.queryRunner.query("select * from service where service_id = ?",
      Model.funToResultSetHandler(serviceMapper), s)
  }

  def getCompanies = {
    Model.queryRunner.query("select * from company", Model.funToResultSetHandlerMany(companyMapper))
  }

  def indexMapper(rs: ResultSet) = {
    IndexMapped(
      company = rs.getString("names").split("\\|")(0), serviceDate = Option(rs.getDate("service_date")).map {
        new DateMidnight(_)
      },
      realServiceDate = Option(rs.getDate("real_service_date")).map {
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
    Model.queryRunner.query(s, Model.funToResultSetHandlerMany(indexMapper))
  }

  def save(service: Service, list: List[PartHours], servicemen: Set[Long]) {
    if (list.isEmpty) return
    val id = service.id match {
      case Full(id) =>
        Model.queryRunner.update("update service set date1 = ?, type1 = ?, result = ? where service_id = ?",
          Misc.mysqlDateFormat.print(service.date1), service.type1.toString,
          service.result, id.asInstanceOf[AnyRef])
        Model.queryRunner.update("delete from service_part where service_id = ?", id)
        Full(id)
      case _ =>
        Model.queryRunner.update("insert into service(date1, type1, result) values (?,?,?)",
          Misc.mysqlDateFormat.print(service.date1), service.type1.toString, service.result)
        lastInsertId
    }
    for {
      partHour <- list
      serviceableId <- partHour.idPart.toList
      serviceId <- id.toList
    } {
      Model.queryRunner.update("insert into service_part(service_id,serviceable_id,machine_hours) values (?,?,?)",
        serviceId.asInstanceOf[AnyRef], serviceableId.asInstanceOf[AnyRef], partHour.machineHours.map(_.asInstanceOf[AnyRef]).orNull)
    }
    for {
      serviceId <- id.toList
      servicemanId <- servicemen
    } {
      Model.queryRunner.update("insert into serviceman(service_id,serviceman_id) values (?,?)",
        serviceId.asInstanceOf[AnyRef], servicemanId.asInstanceOf[AnyRef])
    }
  }

  def getOuterPersonIdName: List[(Long, String)] = {
    Model.queryRunner.query(
      "select person_id, name from person where company_id is null and person_id is not null",
      Model.funToResultSetHandlerMany((rs: ResultSet) => {
        (rs.getLong("person_id"), rs.getString("name"))
      }))
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
        Model.queryRunner.query(sq._1,
          Model.funToResultSetHandler(serviceMapper), sq._2.map(_.asInstanceOf[AnyRef]): _*)
      }
    }
    val maybeWithServices = maybeService.map {
      (x: Service) =>
        val list = x.id match {
          case Full(id) =>
            Model.queryRunner.query(
              "select * from service " +
                "join service_part using(service_id) " +
                "join serviceable using(serviceable_id) " +
                "where service_id = ?",
              Model.funToResultSetHandlerMany(
                (rs: ResultSet) => {
                  rs.getLong("serviceable_id")
                }), id.asInstanceOf[AnyRef])
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
    val mainOnes = Model.queryRunner.query(
      "select * from serviceable where company_id = ?",
      Model.funToResultSetHandlerMany(serviceableMapper), companyId.asInstanceOf[AnyRef])
    mainOnes.flatMap((s: Serviceable) => {
      s :: Model.queryRunner.query(
        "select * from serviceable where parent_id = ?", Model.funToResultSetHandlerMany(serviceableMapper)
        , s.id.getOrElse(0).asInstanceOf[AnyRef])
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
    val sureValues: List[Any] = value :: serviceable.productIterator.drop(2).toList.map {
      case Full(x: DateMidnight) => Misc.mysqlDateFormat.print(x)
      case Full(x) => x.toString
      case x: Box[Double] => null
      case x: Box[Int] => null
      case x: Box[DateMidnight] => null
      case x: Box[String] => null
      case x => x
    }
    serviceable.id match {
      case Full(id) =>
        val setter = sureNamesList.map(_ + " = ?").mkString(",")
        val setterString = String.format("update serviceable set %s where serviceable_id = ?", setter)
        Model.queryRunner.update(setterString, (sureValues :+ id) map (_.asInstanceOf[AnyRef]): _*)
        Full(serviceable)
      case _ =>
        val str = sureNamesList.mkString(",")
        Model.queryRunner.update(
          "insert into serviceable (%s) values(?,?,?,?,?,?,?,?,?,?)".format(str), sureValues.map(
            _.asInstanceOf[AnyRef]): _*)
        Full(serviceable.copy(id = lastInsertId))
    }
  }

  def getParentsIdNameCompany: Seq[(Long, String)] = {
    Model.queryRunner.query("select note, serviceable_id, company.names as company_names," +
      " specification from company join serviceable using(company_id)",
      Model.funToResultSetHandlerMany(
        rs => {
          def sub(x: String) = {
            x.substring(0, if (x.size < 10) x.size else 10)
          }
          (rs.getLong("serviceable_id"), sub(rs.getString("company_names").split("\\|")(0)) + "|"
            + sub(rs.getString("specification")) + "|" + sub(rs.getString("note")))
        }))
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
    Model.queryRunner.query(
      "select * from serviceable where serviceable_id = ?", Model.funToResultSetHandler(serviceableMapper), s)
      .openOr(Serviceable.apply())
  }

  def personMapper(rs: ResultSet): Person = {
    Person(id = Full(rs.getLong("person_id")), name = rs.getString("name"), fax = rs.getString("fax"),
      cellPhone = rs.getString("cell_phone"), telephone = rs.getString("telephone"), mail = rs.getString("mail"),
      companyId = Full(rs.getInt("company_id")), position = rs.getString("position"), isServiceman = rs.getBoolean("is_serviceman"))
  }

  def getPeopleForCompany(l: Long): List[Person] = {
    Model.queryRunner.query(
      "select * from company join person using(company_id) where company_id = ?",
      Model.funToResultSetHandlerMany(personMapper), l.asInstanceOf[AnyRef])
  }

  def getCompaniesIdName = {
    Model.queryRunner.query("select company_id, names from company",
      Model.funToResultSetHandlerMany(rs => Pair(rs.getLong("company_id"), rs.getString("names").split("\\|")(0))))
  }

  def getPerson(x: Any): Person = {
    (Model.queryRunner.query("select * from person where person_id = ?",
      Model.funToResultSetHandler(personMapper), x.asInstanceOf[AnyRef]).getOrElse(Person.apply()))
  }

  def save(person2: Person): Box[Person] = {
    val person = person2.copy(companyId = person2.companyId.flatMap {
      (x: Long) => if (x == 0L) Empty else Full(x)
    })
    person.id match {
      case (Full(id)) =>
        Model.queryRunner.update("update person set fax = ?, name = ?, telephone = ?, is_serviceman = ?, " +
          "cell_phone = ?, position = ?, company_id = ?, mail = ? where person_id = ?",
          person.fax, person.name, person.telephone, person.isServiceman.asInstanceOf[AnyRef],
          person.cellPhone, person.position,
          person.companyId.map(_.asInstanceOf[AnyRef]).orNull, person.mail, id.asInstanceOf[AnyRef])
        Full(person)
      case _ =>
        Model.queryRunner.update(
          "insert into person(fax, name, telephone, cell_phone, position, company_id, mail, is_serviceman) " +
            "values(?,?,?,?,?,?,?,?)",
          person.fax, person.name, person.telephone, person.cellPhone, person.position,
          person.companyId.map(_.asInstanceOf[AnyRef]).orNull, person.mail,
          person.isServiceman.asInstanceOf[AnyRef])
        Full(person.copy(id = lastInsertId))
    }
  }

  def lastInsertId: Box[Long] = {
    Model.queryRunner.query("select LAST_INSERT_ID()",
      Model.funToResultSetHandler(rs => rs.getLong(1)))
  }

  def save(company: Company): Company = {
    company.id match {
      case Full(id) =>
        Model.queryRunner.update(
          "update company set address = ?, names = ?, show_in_index = ?, priority_days = ? where company_id = ?",
          company.address, company.names, company.showInIndex.asInstanceOf[AnyRef], company.priorityDays.asInstanceOf[AnyRef], id.asInstanceOf[AnyRef])
        company
      case _ =>
        Model.queryRunner.update("insert into company(address, names, show_in_index, priority_days) values(?,?,?,?)",
          company.address, company.names, company.showInIndex.asInstanceOf[AnyRef], company.priorityDays.asInstanceOf[AnyRef])
        company.copy(id = lastInsertId)
    }
  }

  def companyMapper(rs: ResultSet) = {
    Company(id = Option(rs.getLong("company_id")), names = rs.getString("names"), address = rs.getString("address"),
      showInIndex = rs.getBoolean("show_in_index"), priorityDays = rs.getInt("priority_days"))
  }

  def getCompany(x: String): Company = {
    Model.queryRunner.query(
      "select * from company where company_id = ?", Model.funToResultSetHandler(companyMapper), x)
      .getOrElse(Company.apply())
  }

}
