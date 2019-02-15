package org.romeo.adventofcode.day3

import org.romeo.adventofcode.common.Puzzle

object DayThree {
  def main(args: Array[String]): Unit = {
    new DayThree().run()
  }
}

class DayThree extends Puzzle("https://adventofcode.com/2018/day/3/input") {

  case class Claim(id: Int, leftOffset: Int, topOffset: Int, width: Int, height: Int) {
    //list of coords of all squares of fabric that will be taken by this claim
    def coords: List[(Int, Int)] = {
      val crossProduct = for {
        x <- leftOffset until leftOffset + width
        y <- topOffset until topOffset + height
      } yield (x,y)
      crossProduct.toList
    }
  }

  def parseClaim(input: String): Claim = {
    val spaceSplit = input.split(" ")
    val id = spaceSplit(0).drop(1)
    val left = spaceSplit(2).takeWhile(_ != ',')
    val top = spaceSplit(2).dropWhile(_ != ',').drop(1).dropRight(1)
    val width = spaceSplit(3).takeWhile(_ != 'x')
    val height = spaceSplit(3).dropWhile(_ != 'x').drop(1)
    Claim(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
  }

  def parseInput(input: String): List[Claim] = {
    input.split("\n").map(parseClaim).toList
  }

  override def validateInput(input: String): Unit = {
    assert(parseInput(input).nonEmpty)
  }

  override def solvePart1(input: String): String = {
    val claims = parseInput(input)
    val allCoords = claims.flatMap(_.coords)
    val coordCounts = allCoords.foldLeft(Map.empty[(Int, Int), Int])(
      (acc, coord) => {
        acc + (coord -> (acc.getOrElse(coord, 0) + 1))
      }
    )
//    printQuilt(coordCounts)
    coordCounts.values.count(_ >= 2).toString
  }

  def printQuilt(coords:Map[(Int, Int), Int]): Unit = {
    val allCoords = for{
      x <- 0 until 1000
      y <- 0 until 1000
    } yield (x,y)

    println(allCoords.map(c => coords.getOrElse(c, 0)).grouped(1000).map(_.mkString("")).mkString("\n"))
  }

  override def solvePart2(input: String): String = {
    val claims = parseInput(input)
    val allCoords = claims.flatMap(_.coords)
    val coordCounts = allCoords.foldLeft(Map.empty[(Int, Int), Int])(
      (acc, coord) => {
        acc + (coord -> (acc.getOrElse(coord, 0) + 1))
      }
    )
    val goodCut = claims.find(claim => claim.coords.forall(coord => coordCounts.get(coord).contains(1)))
    goodCut.get.id.toString
  }
}
