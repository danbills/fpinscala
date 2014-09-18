package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {

    def go(s: Stream[A], a: Stream[A], i: Int): Stream[A] = {
      if (i <= 0) a
      else s match {
        case Cons(h, t) => cons(h(), go(t(), a, i-1))
        case _ => a
      }
    }

    go(this, Stream[A](), n)
  }

  def drop(n: Int): Stream[A] = {

    @annotation.tailrec
    def go(s: Stream[A], i: Int): Stream[A] = {
      if (i <= 0) s
      else s match {
        case Cons(h, t) => go(t(), i - 1)
        case _ => Stream()
      }
    }

    go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {

    def go(s: Stream[A], a: Stream[A]): Stream[A] = s match {
      case Cons (h, t) if (p (h () ) ) => cons(h(), go(t(), a))
      case _ => a
    }

    go(this, Stream[A]())
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Stream[A]())((i, a) => if (p(i)) cons(i, a) else Empty)
  }

  def headOption: Option[A] = foldRight(None: Option[A])((i, _) => Some(i))

  def map[B](f: A => B): Stream[B] = foldRight(Stream[B]())((i, a) => cons(f(i), a))

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream[A]())((i, a) => if (f(i)) cons(i, a) else a)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((i, a) => cons(i, a))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream[B]())((i, a) => f(i).append(a))

  def forAll(p: A => Boolean): Boolean = foldRight(true)((i, m) => p(i) && m)

  def startsWith[A](s: Stream[A]): Boolean = sys.error("todo")

  def toList: List[A] = {

    @annotation.tailrec
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: l)
      case _ => l
    }

    go(this, List[A]()).reverse
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def countUpForeverFrom(n: Int): Stream[Int] = cons(n, countUpForeverFrom(n+1))
}