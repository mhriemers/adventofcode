package adventofcode.common

import java.io.{BufferedReader, InputStream, InputStreamReader}

import cats.effect.IO
import monix.eval.Task
import monix.reactive.Observable
import simulacrum.typeclass

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

@typeclass trait FromInputStream[F[_]] {

  def fromInputStream(inputStream: IO[InputStream]): F[String]

}

object FromInputStream {

  implicit val inputStreamObservable: FromInputStream[Observable] =
    (inputStream: IO[InputStream]) => {
      val task = Task.fromIO(inputStream.map(s ⇒ new BufferedReader(new InputStreamReader(s))))
      Observable.fromLinesReader(task)
    }

  abstract class StreamBased[F[_]](implicit cbf: CanBuildFrom[Nothing, String, F[String]])
    extends FromInputStream[λ[A => IO[F[A]]]] {
    override def fromInputStream(inputStream: IO[InputStream]): IO[F[String]] =
      inputStream
        .map(is ⇒ new BufferedReader(new InputStreamReader(is)))
        .map(br ⇒ Stream.continually(br.readLine()).takeWhile(_ != null).to[F])
  }

  implicit def inputStreamList: FromInputStream[λ[A => IO[List[A]]]] = new StreamBased[List]() {}

  implicit def inputStreamVector: FromInputStream[λ[A => IO[Vector[A]]]] = new StreamBased[Vector]() {}

}
