package fintech.homework04

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // реализовать функцию fold
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    def iter(t: Tree[A]): B = t match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => g(iter(left), iter(right))
    }
    iter(t)
  }

  // реализовать следующие функции через fold

  def size[A](t: Tree[A]): Int = ???

  def max(t: Tree[Int]): Int = ???

  def depth[A](t: Tree[A]): Int = ???

  //здесь возможно придется прибегнуть к насильному указанию типа: Leaf(a): Tree[A]
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = ???
}

object Main extends App {
  val tree = Branch(Branch(Leaf(1), Leaf("2")), Branch(Leaf(3), Leaf("4")))
  println(Tree.fold(tree)(_.toString)(_ + _))
}