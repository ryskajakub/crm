package jr.spec

import org.joda.time.DateMidnight
import org.joda.time.format.DateTimeFormat
import java.lang.String
import net.liftweb.common.{Box, Empty, Full}


/**
 * User: coub
 * Date: 12/19/11
 * Time: 12:00 AM
 */

object Misc {
  def readable[A](box: Box[A]): String = box.map(_.toString) openOr ""

  def parse2Int(x: String) = {
    try {
      Full(x.toInt)
    } catch {
      case t: Throwable =>
        Empty
    }
  }
  val mysqlDateFormat = DateTimeFormat.forPattern("yyyy-MM-dd")
  val dateFormat = DateTimeFormat.forPattern("dd.MM.yyyy")

  def parse2JodaDate(x: String) = {
    try {
	 
	    val dateFull = x.trim.split("\\.")
	    val day = if( dateFull(0).size == 1) "0" + dateFull(0) else dateFull(0)
	    val month = if( dateFull(1).size == 1) "0" + dateFull(1) else dateFull(1)
	    val full = day + "." + month + "." + dateFull(2)
	  
      Full(DateMidnight.parse(full, dateFormat))
    } catch {
      case t: Throwable =>
	      print(t)
        Empty
    }
  }

  def parse2Double(x: String) = {
    try {
      Full(x.toDouble)
    } catch {
      case t: Throwable =>
        Empty
    }
  }
}