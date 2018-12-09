package adventofcode.day1

import adventofcode.common.{FoldableM, ObservableHelpers, console}
import cats.effect._
import cats.instances.long._
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

import scala.language.higherKinds

object Part1 extends TaskApp with ObservableHelpers {

  override def run(args: List[String]): Task[ExitCode] = {
    val file = readFileFromResource("input.txt")
    for {
      frequency ← countFrequency[Observable, Task](file)
      _ ← console.putStrLn(frequency)
    } yield ExitCode.Success
  }

  def countFrequency[F[_], G[_]](strings: F[String])(implicit F: FoldableM[F, G]): G[Long] = {
    F.foldLeftM(strings, 0l)((b, string) ⇒ b + frequencyToLong(string))
  }

}
