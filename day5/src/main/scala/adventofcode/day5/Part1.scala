package adventofcode.day5

import adventofcode.common.console
import cats.effect.ExitCode
import cats.instances.int._
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

object Part1 extends TaskApp {
  override def run(args: List[String]): Task[ExitCode] = for {
    chars ← readFileFromResourceAsChars("input.txt")
      .flatMap(array ⇒ Observable(array: _*))
      .toListL
    string ← Task.fromEval(collapse(chars))
    _ ← console.putStrLn(string.length)
  } yield ExitCode.Success
}
