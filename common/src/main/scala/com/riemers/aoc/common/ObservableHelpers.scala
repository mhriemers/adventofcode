package com.riemers.aoc.common

import java.io.{BufferedReader, InputStreamReader}

import monix.eval.Task
import monix.reactive.Observable

trait ObservableHelpers {

  def readFileFromResource(file: String): Task[Observable[String]] = Task {
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

  def fileToList(observable: Observable[String]): Task[List[String]] = {
    observable.toListL
  }

}

object ObservableHelpers extends ObservableHelpers
