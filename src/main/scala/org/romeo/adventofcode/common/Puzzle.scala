package org.romeo.adventofcode.common

import com.typesafe.config.ConfigFactory

/**
  * User: tylerromeo
  * Date: 12/7/16
  * Time: 9:22 PM
  *
  */
abstract class Puzzle(inputUrl: String) {
  val authKey = "authCookie"
  val webReader = new WebInputReader(inputUrl, getConfigOptional(authKey))

  def getConfigOptional(key: String): Option[String] = {
    val config = ConfigFactory.load()
    if(config.hasPath(key)) Some(config.getString(key)) else None
  }

  def run(): Unit = {
    webReader.content match {
      case Right(x) => {
        validateInput(x)
        println(s"Part one's answer is ${solvePart1(x)}")
        println(s"Part two's answer is ${solvePart2(x)}")
      }
      case Left(e) => println(e)
    }
  }

  /**
    * read the problem's input and crash hard if it's invalid
    * @param input
    */
  def validateInput(input: String): Unit

  /**
    * solve part 1 of the day's problem
    * @param input
    * @return
    */
  def solvePart1(input: String): String

  /**
    * solve part 2 of the day's problem
    * @param input
    * @return
    */
  def solvePart2(input: String): String
}
