package com.example

import scala.io.StdIn.{readInt, readLine}

object InmatesTaskTry extends App {
  var inmatesCount = 0
  val chainList = ChainList(List(Chain(List())))
  val inputs = readInputs()
  val result = inputs.map(x => chainList.add(x))
  println(inputs)
  println(result)

  class Pair(val left: Int, val right: Int) {
    type B <: List[Int]
    def contains(number: Int): Boolean = {
      number == left || number == right
    }
    def equals(pair: Pair): Boolean =
      this.contains(pair.left) || this.contains(pair.right)
  }

  case class Chain(inmates: List[Int]) {
    def contains(pair: Pair): Boolean =
      inmates.contains(pair.left) || inmates.contains(pair.left)
    def add(pair: Pair): Chain = {
      if (inmates.contains(pair.left)) Chain(pair.right :: inmates)
      else Chain(pair.left :: inmates)
    }
    def size() = inmates.size
  }

  case class ChainList(chainList: List[Chain]) {
    def add(pair: Pair): ChainList = {
      def loop(list: List[Chain]): ChainList = {
        list match {
          case Nil => loop(Chain(List(pair.left, pair.right)) :: Nil)
          case head :: tail =>
            if (head.contains(pair)) loop(head.add(pair) :: tail)
            else if (relationExist(pair)) loop(tail :+ head)
            else loop(Chain(List(pair.left, pair.right)) :: head :: tail)
        }
      }
      loop(chainList)
    }
  }

  def relationExist(pair: Pair): Boolean = {
    inputs.exists(x => x.equals(pair))
  }

  def readInputs(): List[Pair] = {
    inmatesCount = readInt()
    val pairsCount = readInt()
    (1 to pairsCount)
      .map(_ => {
        val line = readLine().split("\\s").toList
        (line.head.toInt -> line.last.toInt)
        new Pair(line.head.toInt, line.last.toInt)
      })
      .toList
  }
}
