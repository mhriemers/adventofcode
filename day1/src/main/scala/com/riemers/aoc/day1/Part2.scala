package com.riemers.aoc.day1

import cats.data.{NonEmptyList ⇒ Nel}
import cats.effect.ExitCode
import cats.{Applicative, Defer}
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}

import scala.language.higherKinds

object Part2 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = for {
    frequencies ← readFileFromResource("input.txt").toListL
    long ← func(frequencies)
    _ ← Task(println(long))
  } yield ExitCode.Success

  def func[F[_] : Defer : Applicative](strings: List[String]): F[Long] = {
    def rec(curr: List[String], freqs: Nel[Long] = Nel.one(0)): F[Long] = {
      (curr, freqs) match {
        case (Nil, _) ⇒ Defer[F].defer(rec(strings, freqs))
        case (head :: tail, Nel(freq, _)) ⇒
          val l = freq + frequencyToLong(head)
          if (freqs.exists(_ == l)) Applicative[F].pure(l)
          else Defer[F].defer(rec(tail, l :: freqs))
      }
    }

    rec(strings)
  }

}

