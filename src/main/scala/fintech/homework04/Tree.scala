package fintech.homework04

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    def iter(t: Tree[A]): B = t match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => g(iter(left), iter(right))
    }
    iter(t)
  }

  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _)

  def max(t: Tree[Int]): Int = fold(t)(identity[Int])(scala.math.max)

  def depth[A](t: Tree[A]): Int = fold(t)(_ => 1)(_.max(_) + 1) - 1

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    def iter(t: Tree[A]): Tree[B] = t match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(iter(left), iter(right))
    }
    iter(t)
  }
}

object Main extends App {
  val tree = Branch(Branch(Leaf(1), Branch(Leaf(4), Leaf(3))), Leaf(3))
  val tree2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  println("fold: " + Tree.fold(tree)(_.toString)(_ + _))
  println("size: " + Tree.size(tree))
  println("max: " + Tree.max(tree))
  println("depth: " + Tree.depth(tree2))

  println(Tree.map(tree2)(_ * 10))
}