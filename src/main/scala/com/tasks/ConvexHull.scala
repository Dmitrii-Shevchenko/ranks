package com.example

import scala.annotation.tailrec

object ConvexHull extends App {

  import scala.collection.mutable.ListBuffer
  import scala.io.StdIn.{readInt, readLine}

  case class Point(x: Int, y: Int)

  def read(): List[Point] = {
    (1 to readInt())
      .map(_ => {
        val line = readLine().split("\\s").toList
        Point(line.head.toInt, line.last.toInt)
      })
      .toList
  }

  def convexHull(points: List[Point]): List[Point] = {
    val sortedPoints = points.sortBy(_.y)
    val upp = takeHull(sortedPoints).dropRight(1)
    val low = takeHull(sortedPoints.reverse).dropRight(1)
    (upp ++ low).toList
  }

  def orientation(p1: Point, p2: Point, p3: Point): Int = {
    (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)
  }

  def takeHull(points: List[Point]): ListBuffer[Point] = {
    val list = new ListBuffer[Point]()
    for (i <- points) {
      while (list.length >= 2 && orientation(
               list(list.length - 2),
               list.last,
               i
             ) <= 0) {
        list -= list.last
      }
      list += i
    }
    list
  }

  def perimeter(list: List[Point]): Double = {
    val first = list.head
    @tailrec
    def loop(list: List[Point], sum: Double = 0): Double = {
      list match {
        case Nil          => sum
        case head :: Nil  => sum + getInterval(head, first)
        case head :: tail => loop(tail, getInterval(head, tail.head) + sum)
      }
    }
    loop(list)
  }

  def getInterval(p1: Point, p2: Point): Double = {
    scala.math
      .sqrt((p2.x - p1.x) * (p2.x - p1.x) + (p2.y - p1.y) * (p2.y - p1.y))
  }

  println(perimeter(convexHull(read())))
}
