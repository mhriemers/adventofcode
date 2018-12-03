package com.riemers.aoc.common

import java.io.{BufferedReader, InputStreamReader}

import monix.eval.Task
import monix.reactive.Observable

trait ObservableHelpers {

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
