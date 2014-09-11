import fpinscala.laziness._

val s = Stream(1,2,3)
s.take(2).toList
s.drop(2).toList
s.drop_2(2).toList
val s2 = (1 to 10).toStream
s2.takeWhile(_ <= 5).toList
s2.takeWhile(_ <= 8).toList
s.toListTailRec
s.toListAppend
s2.exists(_ <= 10)

val s3 = Stream(1,2,3,4,5)
s3.exists(_ <= 5)
s3.exists(_ > 5)
s3.forAll(_ <= 5)
s3.forAll(_ <= 4)
s3.forAll_2(_ <= 5)
s3.forAll_2(_ <= 4)
s3.takeWhile_2(_ <= 2).toList

s.headOption
s3.headOption

s.map(_ * 2).toList

val ones: Stream[Int] = Stream.cons(1, ones)
ones.map(_ + 1).exists(_ % 2 == 0)
ones.takeWhile(_ == 1)
ones.forAll(_ != 1)

Stream.onesUnfold.map(_ + 1).exists(_ % 2 == 0)
val a = Stream.constantUnfold("foo").take(3)
a.toList

val fibs = Stream.fibsUnfold.take(10)
fibs.toList

s.mapUnfold(_ * 2).toList
s.takeUnfold(2).toList
s3.takeWhileUnfold(_ <= 4).toList

s3.zipWithUnfold(s)(_ + _).toList
s.tails.toList
