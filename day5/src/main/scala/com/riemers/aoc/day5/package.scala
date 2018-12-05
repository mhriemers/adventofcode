package com.riemers.aoc

import java.io.{BufferedReader, InputStreamReader}

import cats.instances.list._
import cats.{Eval, Foldable}
import monix.eval.Task
import monix.reactive.Observable

package object day5 {

  val CASE_DIFFERENCE: Int = Math.abs('a' - 'A')

  def readFileFromResourceAsChars(file: String): Observable[Array[Char]] = {
    Observable.fromCharsReader(
      Task(
        new BufferedReader(
          new InputStreamReader(
            getClass.getClassLoader.getResourceAsStream(file)
          )
        )
      )
    )
  }

  def collapse(chars: List[Char]): Eval[List[Char]] = {
    Foldable[List].foldRight(chars, Eval.now(List.empty[Char])) {
      case (char, state) ⇒ state.map {
        case l@Nil ⇒ char :: l
        case l@head :: tail ⇒ if (Math.abs(char - head) == CASE_DIFFERENCE) tail else char :: l
      }
    }
  }

}
