package com.example.next

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated}

trait WrappedValue[T] extends Any {
  def value: T
  override def toString = value.toString
}

trait EnableUnsafe[WrappedType]

object WrappedValue {
  trait Companion {
    type InnerType
    type Error
    type WrappedType <: WrappedValue[InnerType]
    type ValidationResult[A] = Validated[NonEmptyChain[Error], A]

    protected def construct(value: InnerType): WrappedType
    protected def validate(value: InnerType): ValidationResult[InnerType]
    implicit def unwrap(wr: WrappedType): InnerType = wr.value

    def from(value: InnerType): ValidationResult[WrappedType] =
      validate(value).map(construct)
    def unapply(wrapped: WrappedType): Option[InnerType] = Some(wrapped.value)

    object Unsafe {
      implicit object Enable extends EnableUnsafe[WrappedType]
    }

    def apply(
      value: InnerType
    )(implicit ev: EnableUnsafe[WrappedType]): WrappedType = {
      validate(value) match {
        case Valid(x) => construct(x)
        case Invalid(errors) =>
          throw new Exception(errors.toString)
      }
    }
  }
}

class PositiveInt private (val value: String)
    extends AnyVal
    with WrappedValue[String]

object PositiveInt extends WrappedValue.Companion {
  type InnerType = String
  type WrappedType = PositiveInt
  type Error = String

  override protected def construct(value: String): PositiveInt =
    new PositiveInt(value)

  override protected def validate(value: String): ValidationResult[String] =
    Validated.cond(
      value.matches(
        "(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$"
      ),
      value,
      NonEmptyChain.one(
        "Email should match the following pattern <text_1>@<text_2>.<text_3>"
      )
    )
}

object Main extends App {

  import PositiveInt.Unsafe._
  val g = PositiveInt("Nasdf123")
  println(g)
//  println(g.value)
//
//  def giveMeInt(value: Int) = println(value)
//  giveMeInt(g)
//  println(s"azazaa ${g}")
//
  def valid(x: PositiveInt) = println(x)
  valid(PositiveInt("Hello"))
}
