package scalafp

/**
 * Created by f.casasus on 03/12/14.
 */
sealed trait Tree[+A]
case class Leaf[+A](value: A) extends Tree[A]
case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def map[A, B](root: Tree[A])(f: A => B): Tree[B] = root match {
    case Branch(r, l) => Branch(map(r)(f), map(l)(f))
    case Leaf(v) => Leaf(f(v))
  }

  // Type annotation needed in Leaf in order to avoid Scala bad inference
  // with algebraic data types
  def map2[A, B](root: Tree[A])(f: A => B): Tree[B] = {
    fold(root)(x => Leaf(f(x)):Tree[B])(Branch(_ ,_ ))
  }

  def size[A](root: Tree[A]): Int = fold(root)(x => 1)(1 + _ + _ )

  def maximum(root: Tree[Int]): Int = fold(root)(x => x)(_ max _)

  def depth[A](root: Tree[A]): Int = fold(root)(x => 1)(1 + _ max _)

  def fold[A, B](root: Tree[A])(f: A => B)(g:(B, B) => B): B = root match{
      case Leaf(x) => f(x)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

}
