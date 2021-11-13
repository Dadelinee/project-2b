//package imperative.modular
//
///** A mini-framework for trace-based testing of interactive behavior. */
//trait Tracing[Input, Result] extends Task[Input] {
//
//  enum TraceEvent derives CanEqual:
//  case InputEvent(value: Input)
//  case OutputEvent(value: Result)
//
//  import TraceEvent.*
//
//  //val trace = Buffer.empty[TraceEvent]
//
//  /** Instruments the input such that accessing the item appends the corresponding event to the trace. */
//  protected def traced(input: Iterator[Input]) = input.map { s => trace.append(InputEvent(s)); s }
//
//  /** Adds the output to the trace. */
//  override def doOutput(result: Result) = trace.append(OutputEvent(result))
//
//  /**
//   * Invokes the original run method on the instrumented input.
//   * `abstract override` lets us override a run method that is still abstract
//   * as we are defining this trait but will be available later from a provider such as `CountLines`.
//   */
//  abstract override def run(input: Iterator[Input], args: Array[String] = Array.empty) =
//    super.run(traced(input), args)
//}
