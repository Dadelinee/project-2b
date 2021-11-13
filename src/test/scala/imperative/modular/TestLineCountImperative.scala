
package imperative.modular

import org.scalatest.wordspec.AnyWordSpec
import common.canequal.given

given[T, U](using teq: CanEqual[T, T], ueq: CanEqual[U, U]): CanEqual[(T, U), (T, U)] = CanEqual.derived

  class TestLineCountImperative extends AnyWordSpec {

    /** Creates a (mutable!) SUT instance. */
    def createSUT() = new countWordFrequency {}


    "A topwords instance" when {
      "given an empty iterator" should {
        "produce an empty output" in {
          // create SUT instance for this test case
          val sut = createSUT()
          // exercise SUT
          val result = sut.run(Iterator.empty)
          // check effect on output observer
          assert(result.isEmpty)
        }
      }


      "given a nonempty topwords instance" should {
        "produce the correct nonempty output" in {
          // input data for this test case
          val data = Iterator("hello", "world", "what", "up")
          // create SUT instance for this test case
          val sut = createSUT()
          // exercise SUT
          val result = sut.run(data).toList
          val map1 = Map[String, Int]("hello" -> 1)
          val map2 = Map[String, Int]("hello" -> 1, "world" -> 1)
          val map3 = Map[String, Int]("hello" -> 1, "world" -> 1, "what" -> 1)
          val map4 = Map[String, Int]("hello" -> 1, "world" -> 1, "what" -> 1, "up" -> 1)
          val result1 = result(0)
          val result2 = result(1)
          val result3 = result(2)
          val result4 = result(3)
          assert(result1 === map1)
          assert(result2 === map2)
          assert(result3 === map3)
          assert(result4 === map4)
        }
      }
    }

    "given a nonempty topwords instance with multiple same inputs" should {
      "produce the correct nonempty output" in {
        // input data for this test case
        val data = Iterator("aa", "bb", "aa")
        // create SUT instance for this test case
        val sut = createSUT()
        // exercise SUT
        val result = sut.run(data).toList
        val map1 = Map[String, Int]("aa" -> 1)
        val map2 = Map[String, Int]("aa" -> 1, "bb" -> 1)
        val map3 = Map[String, Int]("aa" -> 2, "bb" -> 1)
        val result1 = result(0)
        val result2 = result(1)
        val result3 = result(2)
        assert(result1 === map1)
        assert(result2 === map2)
        assert(result3 === map3)
      }
    }
  }
/*
    "given a nonempty iterator" should {
      "exhibit the correct interactive behavior" in {
        // input data for this test case
        val input = Iterator("hello", "world", "what", "up")
        // create SUT instance for this test case
        val sut = new CountLines with Tracing[String, Map[String, Int]]
        // exercise SUT
        sut.run(input)
        // check correctness of resulting interactions
        import sut.TraceEvent.{InputEvent as i, OutputEvent as o}
        assert(sut.trace == Seq(
          i("hello"), o((1, "hello")),
          i("world"), o((2, "world")),
          i("what"), o((3, "what")),
          i("up"), o((4, "up"))))
      }
    }*/

