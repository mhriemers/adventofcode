package adventofcode.common

import java.io.{BufferedReader, InputStreamReader}

import cats.{Functor, FunctorFilter}
import monix.eval.Task
import monix.reactive.Observable

import scala.language.higherKinds

/**
  * TODO: Replace with typeclass "FromInputStream"
  */
trait ObservableHelpers {

  /**
    * Temporary polyfill
    */
  implicit val observableFunctorFilter: FunctorFilter[Observable] = new FunctorFilter[Observable] {
    override def functor: Functor[Observable] = Observable.catsInstances

    override def mapFilter[A, B](fa: Observable[A])(f: A ⇒ Option[B]): Observable[B] =
      fa.collect(Function.unlift(f))
  }

  def readFileFromResource(file: String): Observable[String] = {
    Observable.fromLinesReader(
      Task(
        new BufferedReader(
          new InputStreamReader(
            getClass.getClassLoader.getResourceAsStream(file)
          )
        )
      )
    )
  }

}

object ObservableHelpers extends ObservableHelpers
