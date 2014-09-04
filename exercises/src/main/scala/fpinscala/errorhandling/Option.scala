package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    // #map's argument function is A => B, so it is cast back to an Option
    // #flatMap's is A => Option[B] so just return f(a)
    case Some(a) => f(a)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    // if this exists, use Some(_) to get the value a in Some(a) and then map it to Some(a)
    // this is effectively (x => Some(x))
    // otherwise return option b
    this map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    // this is a function that returns Some(a) if the predicate is true, otherwise it filters it out and returns None
    // flatMap just calls this on a for Some(a)
    flatMap(a => if (f(a)) Some(a) else None)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m,2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    // call flatMap on a, pass in function that returns f(aa, bb) where bb is just b mapped (i.e. None or its value)
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
//      I don't really understand what's going on with the argument function to map
      case h::t => h.flatMap(hh => sequence(t).map(hh::_))
    }

  def sequence_2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](
      Some(Nil)
    )(
      (x,y) => map2(x,y)(_ :: _)
    )

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_::_))

  def traverse_2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a.map(f))
}