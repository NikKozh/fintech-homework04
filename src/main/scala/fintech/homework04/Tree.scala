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

  def tailrecFold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    def iter(stack: List[Tree[A]], acc: Option[B]): B = stack match {
      case Nil => acc.get

      case Leaf(value) :: tail =>
        val newAcc = acc match {
          case Some(a) => f.andThen(g(a, _))(value)
          case None    => f(value)
        }
        iter(tail, Some(newAcc))

      case Branch(left, right) :: tail =>
        iter(left :: right :: tail, acc)
    }

    iter(List(t), None)
  }

  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def dataSize[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _)

  def max(t: Tree[Int]): Int = fold(t)(identity[Int])(scala.math.max)

  def depth[A](t: Tree[A]): Int = fold(t)(_ => 1)(_.max(_) + 1) - 1

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(value => Leaf(f(value)).asInstanceOf[Tree[B]])((left, right) => Branch(left, right))
}

object Main extends App {

}