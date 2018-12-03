package com.riemers.aoc.day3

import cats.effect.ExitCode
import com.riemers.aoc.common.ObservableHelpers
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

object Part1 extends TaskApp with ObservableHelpers {

  type Board = List[List[Long]]

  override def run(args: List[String]): Task[ExitCode] = {
    val claims = parseObservable(readFileFromResource("input.txt"))
    for {
      board ← constructBoard(1100, 1100)
      long ← func(claims, board)
      _ ← Task(println(long))
    } yield ExitCode.Success
  }

  def func(claims: Observable[Claim], board: Board): Task[Long] = {
    claims.flatMap(claimToPoints).foldLeftL(board)(processPoint).flatMap(countSquares)
  }

  def countSquares(board: Board): Task[Long] = Task {
    board.map { inner ⇒
      inner.foldLeft(0l)((state, count) ⇒ if (count >= 2) state + 1 else state)
    }.sum
  }

  def processPoint(board: Board, point: Point): Board = {
    board.updated(point.x.toInt, board(point.x.toInt).updated(point.y.toInt, board(point.x.toInt)(point.y.toInt) + 1))
  }

  def constructBoard(width: Long, height: Long): Task[Board] =
    Task.pure(List.fill(width.toInt, height.toInt)(0l))

  def claimToPoints(claim: Claim): Observable[Point] = {
    Observable.range(claim.left, claim.left + claim.width).flatMap { x ⇒
      Observable.range(claim.top, claim.top + claim.height).map { y ⇒
        Point(x, y)
      }
    }
  }

}
