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
      Full(DateMidnight.parse(x, dateFormat))
    } catch {
      case t: Throwable =>
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