package scalafp

/**
 * Created by f.casasus on 06/11/14.
 */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def tail(xs: List[_]): List[_] = xs match {
    case Nil => throw new NoSuchElementException
    case Cons(x, rest) => rest
  }

  def drop(xs: List[_], n: Int): List[_] = xs match {
    case _ if n < 0 => throw new NoSuchElementException
    case x if n == 0 => x
    case x => drop(tail(xs), n - 1)
  }

  def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else Cons(x, xs)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def setHead[T](xs: List[T], y: T): List[T] = xs match {
    case Nil => throw new NoSuchElementException
    case Cons(x, xs) => Cons(y, xs)
  }

  def init(xs: List[_]): List[_] = xs match {
    case Nil => throw new NoSuchMethodException
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(xs), z) { (z, x) => f(x, z)}
  }

  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = xs match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def map[A, B](xs: List[A])(f: A => B): List[B] = xs match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs) (f))
  }

  def filter[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  def filterWithFP[A](xs: List[A])(f: A => Boolean): List[A] = flatMap(xs) (x => if(f(x)) List(x) else Nil)

  def flatMap[A,B](xs: List[A])(f: A => List[B]): List[B] = xs match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs) (f))
  }

  def zipWith[A, B, C](xs: List[A], ys: List[B]) (f:(A, B) => C): List[C] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x,xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def flatten[A](list: List[List[A]]): List[A] = foldRight(list, Nil:List[A]){ (x, z) => append(x, z) }

  def length(xs: List[_]): Int = foldLeft (xs, 0) { (z, x) => z + 1 }

  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil:List[A]){ (z, x) => Cons(x, z) }

  def contains[A](sub: List[A], xs: List[A]): Boolean = {
      def all[A] (xs: List[A], ys: List[A]): Boolean = (xs, ys) match {
        case (Nil, _) => true
        case (Cons(x, xs), Cons(y, ys)) if (x == y) => all(xs, ys)
        case _ => false
      }

      (sub, xs) match {
        case (Nil, _) => true
        case (Cons(x, xs), Nil) => false
        case (Cons(x, xs), Cons(y, ys)) =>
          if (x != y) contains(sub, ys)
          else if (all(sub, Cons(y, ys))) true
          else contains(sub, ys)
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}

