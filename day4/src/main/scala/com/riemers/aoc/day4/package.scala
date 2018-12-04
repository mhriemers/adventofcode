package com.riemers.aoc

import java.time.LocalDateTime

import monix.eval.Task

import scala.util.Try
import scala.util.matching.Regex

package object day4 {

  val crude: Regex = "\\[(\\d+)\\-(\\d+)\\-(\\d+) (\\d+):(\\d+)\\] (.*)".r
  val shiftBegin: Regex = "Guard #(\\d+) begins shift".r
  val fallsAsleep: Regex = "falls asleep".r
  val wakesUp: Regex = "wakes up".r

  implicit val localDateTimeOrdering: Ordering[LocalDateTime] = (x: LocalDateTime, y: LocalDateTime) => x.compareTo(y)

  def countMinutesAsleep(records: List[CrudeRecord]): Task[Map[(Int, Int), Int]] = Task {
    // Unorthodox? I think so
    case class State(map: Map[(Int, Int), Int] = Map.empty, current: Int = 0, start: Int = 0)

    records.foldLeft(State()) {
      case (state, record) ⇒
        record.text match {
          case shiftBegin(id) ⇒ state.copy(current = id.toInt)
          case fallsAsleep() ⇒ state.copy(start = record.date.getMinute)
          case wakesUp() ⇒ state.copy(map = (state.start until record.date.getMinute).foldLeft(state.map) {
            case (map, minute) ⇒ map updated((state.current, minute), map.getOrElse((state.current, minute), 0) + 1)
          }, start = 0)
        }
    }.map
  }

  def parseCrudeRecord(string: String): Task[Option[CrudeRecord]] = Task {
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
