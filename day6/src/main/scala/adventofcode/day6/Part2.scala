package adventofcode.day6

import adventofcode.common.{ObservableHelpers, console}
import cats.data.{NonEmptyList ⇒ Nel}
import cats.effect.ExitCode
import cats.instances.int._
import cats.instances.long._
import monix.eval.{Task, TaskApp}

object Part2 extends TaskApp with ObservableHelpers {
  override def run(args: List[String]): Task[ExitCode] = for {
    points ← parseF(readFileFromResource("input.txt")).toListL
    result = func(Nel.fromListUnsafe(points))
    _ ← console.putStrLn(result)
  } yield ExitCode.Success

  def func(points: Nel[Point]): Long = {
    val (xmin, xmax, ymin, ymax) = findExtremes(points)
    val coords = generateCoordinates(xmin, xmax, ymin, ymax)

    coords.map {
      case (x, y) ⇒ points.map(_.dist(x, y)).reduce
    }.count(_ < 10000)
  }
}
