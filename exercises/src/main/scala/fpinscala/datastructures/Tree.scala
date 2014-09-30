package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def fold[A, B](t: Tree[A])(bf: Branch[A] => B)(lf: Leaf[A] => B): B = t match {
    case Branch(l, r) => bf(Branch(l, r))
    case Leaf(v) => lf(Leaf(v))
  }

  def size[A](t: Tree[A]): Int = fold(t)(b => size(b.left) + size(b.right) + 1)(_ => 1)

  def maximum(t: Tree[Int]): Int = fold(t)(b => maximum(b.left).max(maximum(b.right)))(l => l.value)

  def depth[A](t: Tree[A]): Int = {
    def go(t: Tree[A], d: Int): Int = fold(t)(b => go(b.left, d+2).max(go(b.right, d+1)))(l => d)

    go(t, 0)
  }

  def map[A](t: Tree[A])(f: A => A): Tree[A] = {
    fold(t)(b => Branch(map(b.left)(f), map(b.right)(f)): Tree[A])(l => Leaf(f(l.value)): Tree[A])
  }

}