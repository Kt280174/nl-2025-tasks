
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable


case class AccessLog(
  ip: String,
  datetime: String,
  method: String,
  path: String,
  status: Int,
  bytesSent: Long
)

object Main {

  // Regex parse access.log 
  val logPattern: Regex =
    raw"""^(\S+)\s+\S+\s+\S+\s+\[([^\]]+)\]\s+"(\S+)\s+([^"]+?)\s+(\S+)"\s+(\d{3})\s+(\S+)\s+"([^"]*)"\s+"([^"]*)"""".r

  
  def parseLine(line: String): Option[AccessLog] = {
    line match {
      case logPattern(ip, datetime, method, path, _, status, bytes, _, _) =>
        val bytesSent =
          if (bytes == "-" || bytes.isEmpty) 0L else bytes.toLong

        Some(
          AccessLog(
            ip = ip,
            datetime = datetime,
            method = method,
            path = path,
            status = status.toInt,
            bytesSent = bytesSent
          )
        )
      case _ => None
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "/uploads/input.txt"

    val source =
      try {
        Source.fromFile(filename)
      } catch {
        case _: Exception =>
          println(s"ERROR: file $filename not found")
          return
      }

    val logs = source
      .getLines()
      .flatMap(parseLine)
      .toVector

    source.close()

    if (logs.isEmpty) {
      println("No valid access.log lines found.")
      return
    }

    // ----------------------------
    // Aggregation
    // ----------------------------
    val countByMethod = mutable.Map[String, Int]().withDefaultValue(0)
    val countByStatus = mutable.Map[Int, Int]().withDefaultValue(0)
    val countByPath   = mutable.Map[String, Int]().withDefaultValue(0)
    val countByIP     = mutable.Map[String, Int]().withDefaultValue(0)

    var totalBytes: Long = 0L

    logs.foreach { e =>
      countByMethod(e.method) += 1
      countByStatus(e.status) += 1
      countByPath(e.path)     += 1
      countByIP(e.ip)         += 1
      totalBytes              += e.bytesSent
    }

    // ----------------------------
    // Output
    // ----------------------------
    println("=== ACCESS.LOG SUMMARY ===\n")

    println(s"Total requests: ${logs.length}")
    println(s"Total bytes sent: $totalBytes\n")

    println("=== BY HTTP METHOD ===")
    countByMethod.foreach { case (m, c) =>
      println(s"  $m: $c")
    }
    println()

    println("=== BY STATUS CODE ===")
    countByStatus.keys.toSeq.sorted.foreach { code =>
      println(s"  $code: ${countByStatus(code)}")
    }
    println()

    println("=== TOP 5 PATHS ===")
    countByPath.toSeq
      .sortBy(-_._2)
      .take(5)
      .foreach { case (path, cnt) =>
        println(s"  $path -> $cnt hits")
      }
    println()

    println("=== TOP 5 IPs ===")
    countByIP.toSeq
      .sortBy(-_._2)
      .take(5)
      .foreach { case (ip, cnt) =>
        println(s"  $ip -> $cnt requests")
      }
  }
}
