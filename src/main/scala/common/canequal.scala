package common

/** Project-wide missing typesafe equality instances. */
object canequal {

  /** Equality for 2-tuples (pairs). */
  given [T, U](using teq: CanEqual[T, T], ueq: CanEqual[U, U]): CanEqual[(T, U), (T, U)] = CanEqual.derived

  /** Equality for any nullable type and null. */
  given CanEqual[Any | Null, Null] = CanEqual.derived
}