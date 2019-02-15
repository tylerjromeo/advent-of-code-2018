package org.romeo.adventofcode.day4

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.romeo.adventofcode.common.Puzzle



object DayFour {
  def main(args: Array[String]): Unit = {
    new DayFour().run()
  }
}

class DayFour extends Puzzle("https://adventofcode.com/2018/day/4/input") {

  implicit val localDateOrdering: Ordering[LocalDateTime] = _ compareTo _

  //[1518-05-30 00:27] wakes up
  //  [1518-11-02 00:00] Guard #433 begins shift
  //    [1518-07-05 23:56] Guard #2593 begins shift
  //    [1518-03-31 00:55] falls asleep
  case class Entry(date: LocalDateTime, action:Action)

  sealed trait Action

  case class BeginShift(guardId: Int) extends Action

  case object WakeUp extends Action

  case object FallAsleep extends Action

  val dateFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  def parseEntry(input: String): Entry = {
    val date = input.drop(1).takeWhile(_ != ']')
    val parsedDate = LocalDateTime.parse(date, dateFormat)

    if (input.contains("wakes up")) {
      Entry(parsedDate, WakeUp)
    } else if (input.contains("falls asleep")) {
      Entry(parsedDate, FallAsleep)
    } else {
      val id = input.dropWhile(_ != '#').drop(1).takeWhile(_ != ' ').toInt
      Entry(parsedDate, BeginShift(id))
    }
  }

  def parseInput(input: String): List[Entry] = {
    input.split("\n").map(parseEntry).sortBy(_.date).toList
  }

  override def validateInput(input: String): Unit = {
    assert(parseInput(input).nonEmpty)

  }

  def fold(sleepTimes: Map[Int, List[Int]],
           currentGuardId: Int,
           fellAsleepMinute: Option[Int],
           nextEntry: Entry): (Map[Int, List[Int]], Int, Option[Int]) = nextEntry.action match {
    case BeginShift(guardId) => (sleepTimes, guardId, None) // store the guard who is on shift
    case WakeUp => ( // add the minutes that the guard slept to the map
      sleepTimes + (
        currentGuardId ->
        (sleepTimes.getOrElse(currentGuardId, Nil) ::: (fellAsleepMinute.get until nextEntry.date.getMinute).toList)
        ),
      currentGuardId,
      None
    )
    case FallAsleep => (sleepTimes, currentGuardId, Some(nextEntry.date.getMinute)) // store the time the cirrent guard fell asleep
  }

  override def solvePart1(input: String): String = {
    val entries = parseInput(input)

    // A map of guard ids to a list of all the minutes they were asleep
    val minutesAsleep = entries.foldLeft((Map.empty[Int, List[Int]], 0, Option.empty[Int]))((z, b) => {
      fold(z._1, z._2, z._3, b)
    })._1

    //get the id of the guard who slept the most minutes
    val sleepiestGuardId = minutesAsleep.maxBy(_._2.size)._1

    //for the guard who slept the most, get the minutes where they were asleep most often
    val sleepiestMinute = minutesAsleep(sleepiestGuardId).groupBy(i => i).maxBy(_._2.size)._1

    (sleepiestGuardId * sleepiestMinute).toString
  }


  def fold2(sleepTimes: Map[Int, List[Int]],
           currentGuardId: Int,
           fellAsleepMinute: Option[Int],
           nextEntry: Entry): (Map[Int, List[Int]], Int, Option[Int]) = nextEntry.action match {
    case BeginShift(guardId) => (sleepTimes, guardId, None) // store the guard who is on shift
    case WakeUp => ( // add the minutes that the guard slept to the map
      (fellAsleepMinute.get until nextEntry.date.getMinute).foldLeft(sleepTimes)((acc, minute) => {
        acc + (minute -> (sleepTimes.getOrElse(minute, Nil) :+ currentGuardId))
      }),
      currentGuardId,
      None
    )
    case FallAsleep => (sleepTimes, currentGuardId, Some(nextEntry.date.getMinute)) // store the time the cirrent guard fell asleep
  }

  override def solvePart2(input: String): String = {
    val entries = parseInput(input)

    // A map of guard ids to a list of all the minutes they were asleep
    val guardsAsleepByMinute = entries.foldLeft((Map.empty[Int, List[Int]], 0, Option.empty[Int]))((z, b) => {
      fold2(z._1, z._2, z._3, b)
    })._1

    val (_, maxAsleepGuardId, maxAsleepMinute) = guardsAsleepByMinute.foldLeft((0, 0, -1)){
      case ((maxCount, maxGuardId, maxMinute), (minute, guardsAsleep)) => {
        val mostAsleepGuard = guardsAsleep.groupBy(i => i).maxBy(_._2.size)
        val mostAsleepGuardId = mostAsleepGuard._1
        val mostAsleepAmount = mostAsleepGuard._2.size
        if (mostAsleepAmount > maxCount) {
          (mostAsleepAmount, mostAsleepGuardId, minute)
        } else {
          (maxCount, maxGuardId, maxMinute)
        }
      }
    }
    (maxAsleepGuardId * maxAsleepMinute).toString
  }
}
