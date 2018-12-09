package adventofcode.common

import monix.reactive.Observable
import simulacrum.typeclass

@typeclass trait ZipWithIndex[F[_]] {

  def zipWithIndex[A](fa: F[A]): F[(A, Long)]

}

object ZipWithIndex {

  implicit val observableZipWithIndex: ZipWithIndex[Observable] = new ZipWithIndex[Observable] {
    override def zipWithIndex[A](fa: Observable[A]): Observable[(A, Long)] = fa.zipWithIndex
  }

  implicit val listZipWithIndex: ZipWithIndex[List] = new ZipWithIndex[List] {
    override def zipWithIndex[A](fa: List[A]): List[(A, Long)] = fa.zipWithIndex.map {
      case (a, i) ⇒ a → i.toLong
    }
  }

  implicit val vectorZipWithIndex: ZipWithIndex[Vector] = new ZipWithIndex[Vector] {
    override def zipWithIndex[A](fa: Vector[A]): Vector[(A, Long)] = fa.zipWithIndex.map {
      case (a, i) ⇒ a → i.toLong
    }
  }
}
