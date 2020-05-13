package com.example.bst

trait BST {
  def insert(elem: Int): BST
  def ordering(z: List[Int])(f: (Int, List[Int]) => List[Int]): List[Int]
  def toList = ordering(List[Int]())(_ :: _).reverse
}

object BST {
  def apply() = EmptyBST

  def apply(elem: Int, elems: Int*): BST = {
    def loop(elems: List[Int], bst: BST): BST = elems match {
      case Nil => bst
      case _   => loop(elems.tail, bst.insert(elems.head))
    }
    loop(elems.toList, NonEmptyBST(elem, EmptyBST, EmptyBST))
  }
}

case object EmptyBST extends BST {
  def insert(elem: Int) = BST(elem)
  def ordering(z: List[Int])(f: (Int, List[Int]) => List[Int]) = z
}

case class NonEmptyBST(elem: Int, left: BST, right: BST) extends BST {

  def insert(newElem: Int) =
    if (newElem < elem) withLeft(left.insert(newElem))
    else if (newElem > elem) withRight(right.insert(newElem))
    else this

  def withLeft(newLeft: BST) =
    NonEmptyBST(elem, newLeft, right)
  def withRight(newRight: BST) =
    NonEmptyBST(elem, left, newRight)

  def ordering(list: List[Int])(connectReverse: (Int, List[Int]) => List[Int]) =
    right.ordering(left.ordering(connectReverse(elem, list))(connectReverse))(
      connectReverse
    )
}

object Main extends App {
  val test =
    BST(37, 8, 4, 3, 1, 2, 7, 6, 5, 30, 14, 9, 13, 11, 10, 12, 25, 23, 18, 15,
      16, 17, 22, 21, 20, 19, 24, 26, 29, 27, 28, 36, 35, 34, 31, 33, 32, 38)

  println(test.toList)

}
