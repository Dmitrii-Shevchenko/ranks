package com.example

import scala.util.{Failure, Success, Try}
import scala.util.control.NoStackTrace

trait WrappedValue[T] extends Any {
  def value: T

  override def toString = this.getClass.getName + "(" + value.toString + ")"

  override def equals(other: Any): Boolean = {
    if (this.getClass.isInstance(other)) {
      value.equals(other.asInstanceOf[WrappedValue[T]].value)
    } else {
      false
    }
  }
  override def hashCode: Int = value.hashCode
}

class PositiveInt private (val value: Int) extends AnyVal with WrappedValue[Int]

object PositiveInt {

  def from(value: Int): Try[PositiveInt] = {
    if (value >= 0) {
      Success(new PositiveInt(value))
    } else {
      Failure(
        new IllegalArgumentException(s"$value must be positive")
        with NoStackTrace
      )
    }
  }

  def unapply(wrapped: PositiveInt): Option[Int] = Some(wrapped.value)
}

class PositiveString private (val value: String)
    extends AnyVal
    with WrappedValue[String]

object PositiveString {

  def from(value: String): Try[PositiveString] = {
    if (value.length >= 0) {
      Success(new PositiveString(value))
    } else {
      Failure(
        new IllegalArgumentException(s"$value must be positive")
        with NoStackTrace
      )
    }
  }

  def unapply(wrapped: PositiveString): Option[String] = Some(wrapped.value)
}

object Main extends App {
  val a = PositiveInt.from(5)
  val c = PositiveInt.from(5)
  a match {
    case a if a == c => println(true)
    case _           => println(false)
  }
  val b = PositiveString.from("hello")
  println(a)
  println(b)
  println(PositiveString.unapply(b.get))
}
