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

  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

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

}