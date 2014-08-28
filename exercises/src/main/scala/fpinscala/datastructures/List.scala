package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(oh, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(List.tail(l), n-1)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (!f(h)) l
      else dropWhile(t, f)
  }

  def init[A](l: List[A]): List[A] = {
    def go(acc: List[A], itr: List[A]): List[A] = itr match {
      case Nil => Nil
      case Cons(h, t) => t match {
        case Nil => acc
        case Cons(h1, t1) => t1 match {
          case Nil => Cons(h, acc)
          case _ => go(Cons(h, acc), t)
        }
      }
    }
    go(List(), l)
  }


  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((i, s) => s + 1)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) =>
      foldLeft(t, f(z, h))(f)
  }

  def sumLeft(l: List[Int]) = l match {
    case Nil => 0
    case _ => foldLeft(l, 0)(_ + _)
  }

  def productLeft(l: List[Double]) = l match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case _ => foldLeft(l, 1.0)(_ * _)
  }

  def sizeLeft[A](l: List[A]) = l match {
    case Nil => 0
    case _ => foldLeft(l, 0)((s, i) => s + 1)
  }

  def reverse[A](l: List[A]) = l match {
    case Nil => Nil
    case _ => foldLeft(l, List[A]())((acc, a) => Cons(a, acc))
  }

  def foldAppend[A](l1: List[A], l2: List[A]): List[A] = foldRightLeft(l1, l2)((acc, i) => Cons(i, acc))

  def flatten[A](l: List[List[A]]): List[A] = foldRightLeft(l, List[A]())((acc, i) => foldAppend(i, acc))

  def foldLeftRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldRight(reverse(l), z)(f)

  def foldRightLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldLeft(reverse(l), z)(f)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRightLeft(l, List[B]())((acc, i) => Cons(f(i), acc))

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = foldLeft(l, List[B]())((acc, i) => foldAppend(acc, f(i)))

  def mapAddOne(l: List[Int]): List[Int] = foldRightLeft(l, List[Int]())((acc, i) => Cons((i+1), acc))

  def mapDoubleToString(l: List[Double]): List[String] = foldRightLeft(l, List[String]())((acc, i)  => Cons(i.toString, acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRightLeft(l, List[A]())((acc, i) => if (f(i)) Cons(i, acc) else acc)
  }

  def flatMapFilter[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)((i) => if (f(i)) List(i) else Nil)
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
    @annotation.tailrec
    def go(il1: List[A], il2: List[A], acc: List[A]): List[A] = il1 match {
      case Nil => acc
      case Cons(h, t) => il2 match {
        case Nil => flatten(List(acc, il2))
        case Cons(h1, t1) => go(t, t1, flatten(List(acc, List(f(h, h1)))))
      }
    }

    go(l1, l2, List[A]())
  }

  def zipSum(l1: List[Int], l2: List[Int]): List[Int] = {
    @annotation.tailrec
    def go(il1: List[Int], il2: List[Int], acc: List[Int]): List[Int] = il1 match {
      case Nil => acc
      case Cons(h, t) => il2 match {
        case Nil => flatten(List(acc, il2))
        case Cons(h1, t1) => go(t, t1, flatten(List(acc, List((h + h1)))))
      }
    }

    go(l1, l2, List[Int]())
  }

  def hasSubSequence[A](l: List[A], s: List[A]): Boolean = {
    @annotation.tailrec
    def go(l1: List[A], s1: List[A]): Boolean = s1 match {
      case Nil => true
      case Cons(sh, st) => l1 match {
        case Nil => false
        case Cons(lh, lt) => {
          if (sh == lh) go(lt, st)
          else go(lt, s)

        }
      }
    }

    go(l, s)
  }
}

