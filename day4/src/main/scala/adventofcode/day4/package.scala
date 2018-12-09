package adventofcode

import java.time.LocalDateTime

import cats.Eval
import cats.instances.int._
import cats.instances.list._
import cats.instances.map._
import cats.syntax.foldable._
import cats.syntax.semigroup._

import scala.util.Try
import scala.util.matching.Regex

package object day4 {

  val crude: Regex = "\\[(\\d+)\\-(\\d+)\\-(\\d+) (\\d+):(\\d+)\\] (.*)".r
  val shiftBegin: Regex = "Guard #(\\d+) begins shift".r
  val fallsAsleep: Regex = "falls asleep".r
  val wakesUp: Regex = "wakes up".r

  // Inverted for foldRight
  implicit val localDateTimeOrdering: Ordering[LocalDateTime] = (x: LocalDateTime, y: LocalDateTime) => y.compareTo(x)

  def countMinutesAsleep(records: List[CrudeRecord]): Eval[Map[(Int, Int), Int]] = {
    case class State(map: Map[(Int, Int), Int], current: Int, start: Int)

    records.foldr(Eval.now(State(Map.empty, 0, 0))) {
      case (record, estate) ⇒
        record.text match {
          case shiftBegin(id) ⇒
            estate.map(_.copy(current = id.toInt))
          case fallsAsleep() ⇒
            estate.map(_.copy(start = record.date.getMinute))
          case wakesUp() ⇒
            estate.flatMap { state ⇒
              (state.start until record.date.getMinute).toList.foldr(Eval.now(state.map)) {
                case (minute, emap) ⇒ emap.map(_ |+| Map((state.current → minute) → 1))
              }.map(map ⇒ state.copy(map = map))
            }
        }
    }.map(_.map)
  }

  def parseCrudeRecord(string: String): Option[CrudeRecord] = {
    string match {
      case crude(syear, smonth, sday, shour, sminute, text) ⇒
        for {
          year ← Try(syear.toInt).toOption
          month ← Try(smonth.toInt).toOption
          day ← Try(sday.toInt).toOption
          hour ← Try(shour.toInt).toOption
          minute ← Try(sminute.toInt).toOption
        } yield CrudeRecord(LocalDateTime.of(year, month, day, hour, minute), text)
      case _ ⇒ None
    }
  }

  case class CrudeRecord(date: LocalDateTime, text: String)

}
