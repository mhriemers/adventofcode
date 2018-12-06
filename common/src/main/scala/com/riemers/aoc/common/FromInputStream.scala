package com.riemers.aoc.common

import java.io.{InputStream, InputStreamReader}

import cats.Functor
import cats.effect.Effect
import cats.syntax.functor._
import monix.eval.TaskLike
import monix.reactive.Observable

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait FromInputStream[F[_], G[_]] {

  def fromInputStream(inputStream: F[InputStream]): G[Byte]

}

object FromInputStream {

  def apply[F[_], G[_]](implicit instance: FromInputStream[F, G]): FromInputStream[F, G] = instance

  implicit def inputStreamObservable[G[_] : Functor](implicit TL: TaskLike[G]): FromInputStream[G, Observable] =
    (inputStream: G[InputStream]) => {
      val task = TaskLike[G].toTask(inputStream.map(s ⇒ new InputStreamReader(s)))
      Observable.fromCharsReader(task).flatMap(array ⇒ Observable(array: _*)).map(_.toByte)
    }

  abstract class StreamBased[F[_] : Functor, G[_]](implicit cbf: CanBuildFrom[Nothing, Byte, G[Byte]])
    extends FromInputStream[F, λ[A => F[G[A]]]] {
    override def fromInputStream(inputStream: F[InputStream]): F[G[Byte]] =
      inputStream.map(is ⇒ Stream.continually(is.read).takeWhile(_ != -1).map(_.toByte).to[G])
  }

  implicit def inputStreamList[F[_] : Effect]: FromInputStream[F, λ[A => F[List[A]]]] = new StreamBased[F, List]() {}

  implicit def inputStreamVector[F[_] : Effect]: FromInputStream[F, λ[A => F[Vector[A]]]] = new StreamBased[F, Vector]() {}

}
