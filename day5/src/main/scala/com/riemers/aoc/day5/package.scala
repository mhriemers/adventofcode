package com.riemers.aoc

import java.io.{BufferedReader, InputStreamReader}

import cats.Eval
import cats.instances.list._
import cats.syntax.foldable._
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
    chars.foldr(Eval.now(List.empty[Char])) {
      case (char, state) ⇒ state.map {
        case l@Nil ⇒ char :: l
        case l@head :: tail ⇒ if (Math.abs(char - head) == CASE_DIFFERENCE) tail else char :: l
      }
    }
  }

}
