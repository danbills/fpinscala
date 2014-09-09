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

  // Ex 1
  def toList: List[A] = {
    @annotation.tailrec
    def toReversedList(in: Stream[A], out: List[A]): List[A] = in match {
      case Cons(h,t) => toReversedList(t(), h() :: out)
      case Empty => out
    }
    toReversedList(this, List()).reverse
  }

  // Ex 2
  def take(n: Int): Stream[A] = {
    if (n < 1) Stream()
    else this match {
      case Cons(h,t) => cons(h(), t().take(n-1))
      case Empty => Stream()
    }
  }
  def drop(n: Int): Stream[A] = {
    if (n < 1) this
    else this match {
      case Cons(h,t) => t().drop(n-1)
      case Empty => Empty
    }
  }

  // Ex 3
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h,t) => {
        lazy val hh = h()
        if (p(hh)) cons(hh, t().takeWhile(p)) else Stream()
      }
      case Empty => Stream()
    }
  }

  // Ex 4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => if (p(h())) t().forAll(p) else false
    case Empty => true
  }

  // Ex 5
  def takeWhileByFolding(p: A => Boolean): Stream[A] =
    this.foldRight(Stream[A]())((h,t) => if (p(h)) cons(h,t) else Stream())

  // Ex 6
  def headOptionByFolding: Option[A] =
    this.foldRight(None: Option[A])( (h,t) => Some(h) )

  // Ex 7
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream[B]())( (h,t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream[A]())( (h,t) => if (f(h)) cons(h, t) else t )

  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)(cons(_,_))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream[B]())( (h,t) => f(h).append(t) )

  // Ex 13
  def mapByUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h,t) => Some(f(h()), t())
    case Empty => None
  }

  def takeByUnfold(n: Int): Stream[A] = unfold((this, n)){
    case (Cons(h,t), n) => if (n<1) None else Some(h(), (t(), n-1))
    case (Empty, _) => None
  }

  def takeWhileByUnfold(f: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h,t) => {
      lazy val hh = h()
      if (f(hh)) Some(hh, (t())) else None
    }
    case Empty => None
  }

  def zipWithByUnfold[B,C](s2: Stream[B])(f: (A,B)=>C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(ah,at),Cons(bh,bt)) => Some( f(ah(),bh()), (at(),bt()) )
      case _ => None
    }

  def zipAllByUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(ah,at),Cons(bh,bt)) => Some( (Some(ah()), Some(bh())), (at(),     bt())     )
      case (Cons(ah,at),Empty)       => Some( (Some(ah()), None),       (at(),     Stream()) )
      case (Empty,Cons(bh,bt))       => Some( (None,       Some(bh())), (Stream(), bt())     )
      case _ => None
    }

  // Ex 14
  def startsWith[A](s: Stream[A]): Boolean = (this, s) match {
    case (_, Empty) => true
    case (Empty, _) => false
    case (Cons(h1,t1), Cons(h2,t2)) =>
      if (h1() == h2()) t1().startsWith(t2())
      else false
  }

  def startsWithByZipAll[A](s: Stream[A]): Boolean =
    zipAllByUnfold(s).takeWhile(_._2 != None).forAll {
      case (a,b) => a==b
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

  // Ex 8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // Ex 9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  // Ex 10
  def fibs: Stream[Int] = {
    def fibs2(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, fibs2(f0, f0+f1))
    fibs2(0,1)
  }

  // Ex 11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => Stream.cons(a, unfold(s)(f))
    case None => Stream()
  }

  // Ex 12
  def fibsByUnfold: Stream[Int] = unfold((0,1)) {
    case (v0, v1) => Some( (v0, (v1, v0+v1)) )
  }

  def fromByUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n+1))

  def constantByUnfold[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

  def onesByUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))
}
