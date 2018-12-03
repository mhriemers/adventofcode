package com.riemers.aoc

package object day1 {

  def frequencyToLong(string: String): Long = {
    string.charAt(0) match {
      case '+' ⇒ string.substring(1).toLong
      case '-' ⇒ -string.substring(1).toLong
    }
  }

}
