package com.riemers.aoc.day2

import cats.data.OptionT
import cats.effect.ExitCode
import cats.instances.list._
import cats.instances.option._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.{Alternative, Monad}
import com.riemers.aoc.common.{ObservableHelpers, putStrLn}
import monix.eval.{Task, TaskApp}

import scala.language.higherKinds

object Part2 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = for {
    list ← readFileFromResource("input.txt").toListL
    opt ← func[Task](list).value
    _ ← putStrLn(opt)
  } yield ExitCode.Success

  def func[F[_]](list: List[String])(implicit M: Monad[F]): OptionT[F, String] = {
    list match {
      case Nil ⇒ OptionT.none[F, String]
      case head :: tail ⇒
        OptionT(
          M.pure(
            tail.map { str ⇒
              intersection(head, str)
            }.find(_.length + 1 == head.length)
          )
        ).orElse(func[F](tail))
    }
  }

  def intersection(s1: String, s2: String): String = {
    val tuples: List[(Char, Char)] = (s1 zip s2).toList
    tuples.mapFilter {
      case (c1, c2) ⇒ Alternative[Option].guard(c1 == c2).as(c1)
    }.mkString
  }

}
