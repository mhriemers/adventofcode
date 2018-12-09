package adventofcode.day4

import adventofcode.common.{ObservableHelpers, console}
import cats.TraverseFilter
import cats.effect.ExitCode
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import monix.eval.{Task, TaskApp}

object Part2 extends TaskApp with ObservableHelpers {
  override def run(args: List[String]): Task[ExitCode] = for {
    list ← readFileFromResource("input.txt")
      .toListL
    records = TraverseFilter[List].mapFilter(list)(parseCrudeRecord)
    sorted = records.sortBy(_.date)
    map ← Task.fromEval(countMinutesAsleep(sorted))
    l = func(map)
    _ ← console.putStrLn(l)
  } yield ExitCode.Success

  def func(map: Map[(Int, Int), Int]): Option[Int] = for {
    ((guard, minute), _) ← map.reduceLeftOption[((Int, Int), Int)] {
      case (a@(_, x), b@(_, y)) ⇒ if (x >= y) a else b
    }
  } yield guard * minute
}
