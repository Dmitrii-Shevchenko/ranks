package com.tasks

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

object BatchedTask extends App {
  implicit val executionContext = ExecutionContext.global

  val list = (0 to 50).toList

  val result = batchedSum(list, 10)

  Await.result(result, Duration.Inf)

  println(result)

  def intToFuture(value: Int): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    println(value)
    value
  }

  def seqSum(list: Seq[Int]): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    list.sum
  }

  def batchedSum(xs: List[Int], batchSize: Int): Future[Seq[Int]] = {
    val groupedList: List[List[Int]] = list.grouped(batchSize).toList

    def loop(batches: List[List[Int]],
             acc: Future[List[List[Int]]]): Future[List[Int]] = {
      batches match {
        case Nil => acc.map(_.reverse.flatten)
        case head :: tail =>
          val updatedAcc = for {
            acc_ <- acc
            batchRes <- Future.traverse(head)(intToFuture)
          } yield batchRes +: acc_
          loop(tail, updatedAcc)
      }
    }
    loop(groupedList, Future.successful(Nil))

//    def loop(batches: List[List[Int]]): Future[List[Int]] = {
//      batches match {
//        case Nil => Future.successful(batches.flatten)
//        case head :: tail =>
//          Future
//            .sequence(head.map(intToFuture))
//            .flatMap(first => loop(tail).map(second => first ++ second))
//      }
//    }
//    loop(groupedList)
  }
}
