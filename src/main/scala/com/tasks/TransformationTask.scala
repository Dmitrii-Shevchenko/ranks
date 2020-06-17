package com.tasks

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class TransformationTask {
  type Transformation[T] = T => Option[T]

  def mergeTwoTransformations[T](first: Transformation[T],
                                 second: Transformation[T]) =
    first andThen {
      case Some(value) => second(value)
      case None        => None
    }

  def foldSeqTransformation[T](list: Seq[Transformation[T]]) =
    list.tail.fold(list.head)((acc, elem) => mergeTwoTransformations(acc, elem))
}

class FutureSeqTask {
  implicit val executionContext = ExecutionContext.global

  def toFutureSeq(tasks: Seq[Future[String]]) = {
    Future.sequence(
      tasks
        .map(
          future => future.map(v => Success(v)).recover { case e => Failure(e) }
        )
    )
  }

  def toFutureTuple(futureTaskWithTry: Future[Seq[Try[String]]]) =
    futureTaskWithTry.map(
      seq =>
        (seq.collect { case Success(value) => value }, seq.collect {
          case Failure(e)                  => e
        })
    )
}
