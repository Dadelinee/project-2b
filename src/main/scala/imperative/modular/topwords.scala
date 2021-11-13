package imperative.modular
import scala.collection.mutable.Queue
import scala.collection.{immutable, mutable}
import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
/**
 * Provides a main method for reading lines and printing line count along with line itself.
 * Depends on a suitable Output provider.
 */
trait countWordFrequency extends Task[String] {
  override def run(input: Iterator[String], args: Array[String] = Array.empty): Iterator[Map[String, Int]] = {
    //Process Command Line Arguments
    println("Command line arguments are: ")
    println(args.mkString(", "))

    println("TEST")
    if args.length > 0 then {
      System.err.nn.println("args: " + args.toSeq)
    }

    val capacity = 3

    val words = input.flatMap(line => line.split("(?U)[^\\p{Alpha}0-9']+"))
    val empty = immutable.Queue.empty[String]
    val emptyMap = immutable.Map[String, Int]()


    def build2(acc: immutable.Queue[String], word: String): (immutable.Queue[String], String) = {
      val q = acc.enqueue(word)
      if q.size > capacity then (q.dequeue._2, q.dequeue._1) else (q, "")
    }

    def buildMap(acc: Map[String, Int], next: (immutable.Queue[String], String)): Map[String, Int] = {
      val freq = acc.getOrElse(next._1.last, 0) //frequency count of the input
      val MAP = acc + (next._1.last -> (freq + 1)) // maps the input to frequency + 1
      val removedFreq = MAP.getOrElse(next._2, 0) //frequency count of the removed word
      if next._1.last != next._2 && removedFreq == 1 then { //checks to see if the inputted and removed word arent the same
        MAP - next._2 //if so, check if frequency is 1 (instead of subtracting from freq to equal 0, we remove the key; 0= doesnt exist
      } else if next._1.last != next._2 && removedFreq > 1 then { //same thing but if greater than 1 then subtract frequency
        MAP + (next._2 -> (removedFreq - 1))

      } else if next._1.last == next._2 && removedFreq > 1 then { //checks to see if the inputted and removed word ARE the same
        MAP + (next._2 -> (removedFreq - 1)) //if so, then check if frequency is higher than 1
      }
      else MAP //satisfies the last option-- inputted and removed word are equal but freq = 1
      //instead of subtracting and adding simultaneously, we do nothing (freq - 1 + 1 = freq)
    }

    val queues = words.scanLeft(empty, "") {
      (a, b) =>
        val (x, y) = build2(a._1, b)
        (x, y)
    }.drop(1).scanLeft(emptyMap)(buildMap).drop(1)
    queues
    }
  }


  /** A concrete main application object. */
  object topwords extends Main[Map[String, Int]] with countWordFrequency
