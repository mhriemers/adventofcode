package adventofcode.common

import monix.reactive.Observable
import simulacrum.typeclass

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.NumericRange
import scala.language.higherKinds

@typeclass trait FromRange[F[_]] {

  def range(start: Long, end: Long, step: Long = 1L): F[Long]

  def rangeInclusive(start: Long, end: Long, step: Long = 1L): F[Long]

}

object FromRange {

  abstract class NumericRangeBased[F[_]](implicit cbf: CanBuildFrom[Nothing, Long, F[Long]]) extends FromRange[F] {
    override def range(start: Long, end: Long, step: Long): F[Long] =
      NumericRange(start, end, step).to[F]

    override def rangeInclusive(start: Long, end: Long, step: Long): F[Long] =
      NumericRange.inclusive(start, end, step).to[F]
  }

  implicit object fromRangeList extends NumericRangeBased[List]

  implicit object fromRangeVector extends NumericRangeBased[Vector]

  implicit val rangeObservable: FromRange[Observable] = new FromRange[Observable] {
    override def range(start: Long, end: Long, step: Long = 1L): Observable[Long] =
      Observable.range(start, end, step)

    override def rangeInclusive(start: Long, end: Long, step: Long = 1L): Observable[Long] =
      range(start, end + 1, step)
  }

}
