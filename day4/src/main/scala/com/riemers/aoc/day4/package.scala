package com.riemers.aoc

import java.time.LocalDateTime

import cats.instances.int._
import cats.instances.list._
import cats.instances.map._
import cats.kernel.Semigroup
import cats.{Eval, Foldable}

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
    // Unorthodox? I think so
    case class State(map: Map[(Int, Int), Int] = Map.empty, current: Int = 0, start: Int = 0)

    Foldable[List].foldRight(records, Eval.now(State())) {
      case (record, estate) ⇒
        record.text match {
          case shiftBegin(id) ⇒
            estate.map(_.copy(current = id.toInt))
          case fallsAsleep() ⇒
            estate.map(_.copy(start = record.date.getMinute))
          case wakesUp() ⇒
            estate.flatMap { state ⇒
              Foldable[List].foldRight((state.start until record.date.getMinute).toList, Eval.now(state.map)) {
                case (minute, emap) ⇒ emap.map { map ⇒
                  Semigroup[Map[(Int, Int), Int]].combine(map, Map((state.current → minute) → 1))
                }
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
