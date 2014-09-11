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

  def exists_2(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Cons(h, t) =>  h() :: t().toList
//    if the Stream isn't a Cons, it's Empty, so return an Empty list
    case _ => List()
  }

  def toListTailRec: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
 //  this needs to be .reverse because you have to do head :: list
    go(this, List()).reverse
  }

  def toListAppend: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), acc :+ h())
      case _ => acc
    }
    go(this, List())
  }

  def take(n: Int): Stream[A] =
    if (n > 0) this match {
//      if n is 1 this is the last element we want, so its cons is an empty stream
      case Cons(h, t) if n == 1 => cons(h(), Stream())
      case Cons(h, t) => cons(h(), t().take(n-1))
      case _ => Stream()
    }
    else Stream()

  def drop(n: Int): Stream[A] = {
    if (n > 0) this match {
      case Cons(h, t) if n == 0 => this
      case Cons(h, t) => t().drop(n - 1)
      case _ => Stream()
    }
    else this
  }

  def drop_2(n: Int): Stream[A] = {
    def go(s: Stream[A], n: Int): Stream[A] =
      if (n <= 0) s
      else s match {
        case Cons(h, t) => go(t(), n - 1)
        case _ => Stream()
      }
    go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t()takeWhile p)
//    if the predicate isn't true, then takeWhile returns Stream()
    case _ => Stream()
  }

  def takeWhile_2(p: A => Boolean): Stream[A] =
//  why is the [A] necessary?
    foldRight(Stream[A]())((a, b) =>
      if (p(a)) cons(a, b)
      else      Stream()
    )

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forAll_2(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
//  because signature is foldRight[A,B](z: => B)(f: (A, => B) => B): B the function can choose not to evaluate the
//  second argument. In that case there is no recursion
    foldRight(None: Option[A])((h, t) => Some(h))

//  stream is [+A] so f must A => B
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream[B]())((a, b) => cons(f(a), b))

//  I did not get the B>:A part on my own; had to look at the answers for that
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a,b) => cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream[B]())((a,b) => f(a) append b)

  def mapUnfold[B](f: A => B): Stream[B] =
//    can just start with case when first action of function literal is matching an expression
    unfold(this) {
//      unfold takes a function and then matches f(this), expecting a Some((head, stream))
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
//      again because unfold will map Some((h,t)) h to the next term, when n is 1 we map that next term to be h() and
//      the term after to be empty so that the recursion ends, making h() the last term
      case (Cons(h, t), n) if n == 1 => Some((h(), (empty, n - 1)))
      case (Cons(h, t), n) if n > 0  => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h())  => Some((h(), t()))
      case _ => None
    }

  def zipWithUnfold[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()), (t1(),t2())))
//      when one of the streams is out of elements, stop zipping
      case _ => None
    }

//zipAll is super hard
//so is startsWith (because it needs zipAll)
  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
//    unfold this, if it's a stream, return an option containing a tuple of the current term (the stream itself, and the
//    next (the stream minus the first element)
      case s => Some((s, s drop 1))
//    why do you have to append an empty stream? Shouldn't you eventually have cons(Empty, unfold(Empty)(f) which then
//    means f(Empty) matches case _ => Stream() and you end up with an empty stream as the last element?
    } append Stream()
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
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

//  f returns an option with a tuple that contains the next value and the next state
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case _ => Stream()
    }

  def onesUnfold: Stream[Int] = unfold(1)(_ => Some((1,1)))
  def constantUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a,a)))
  def fibsUnfold: Stream[Int] = unfold((0,1)) {
//        think of this as making f0 the next term, and setting up the next state so that f1 will be the next term, then f0 + f1, etc.
      case (f0, f1) => Some((f0, (f1, f0 + f1)))
  }

}