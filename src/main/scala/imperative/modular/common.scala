package imperative.modular
import sun.misc.Signal
import sun.misc.SignalHandler

/** Defines a dependency (plug-in contract) on a run method that processes an input stream. */
trait Task[Input] {
  def run(input: Iterator[Input], args: Array[String] = Array.empty):Iterator[Map[String, Int]]
}

/**
 * Provides a reusable main task tied to stdin and stdout.
 * Depends on a suitable run method.
 */
trait Main[Result] extends Task[String]  { //TODO might have to
  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.stdin.getLines()
    val queues = run(lines, args)
    for queue <- queues do { //pulling in each queue at a time
      println(queue)
    }
  }
}