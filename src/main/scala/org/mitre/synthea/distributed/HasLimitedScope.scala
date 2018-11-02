package org.mitre.synthea.distributed

trait HasLimitedScope[A <: B, B] {

  val allowed: Set[B]

  def apply(x: B): A = x match {
    case x1 if allowed contains x1 => x.asInstanceOf[A]
    case _ => throw new IllegalArgumentException(s"Value ($x) must be one of ${allowed.mkString(", ")}")
  }
}

trait HasLimitedRange[A <: B, B <: Ordering[B]] {
  val start: B
  val stop: B

  def apply(f: B): A =
    if (f.gteq(f, start) && f.lteq(f, stop)) {
      f.asInstanceOf[A]
    } else {
      throw new IllegalArgumentException(s"Value ($f) must be in range [$start, $stop]")
    }
}