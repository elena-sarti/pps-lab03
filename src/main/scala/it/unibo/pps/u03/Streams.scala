package u03

import scala.annotation.tailrec

object Streams extends App :

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _          => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _                => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail)                   => filter(tail())(pred)
      case _                                  => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _                              => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _                                => Empty()

    def fill[A](n: Int)(k: A): Stream[A] = n match
      case 0          => empty()
      case n if n > 0 => cons(k, fill(n - 1)(k))
      case _          => throw new IllegalArgumentException()

    def fibonacci(): Stream[Int] =
      def _fibonacci(f2: Int, f1: Int): Stream[Int] =
        cons(f1 + f2, _fibonacci(f1, f1 + f2))
      cons(0, cons(1, _fibonacci(0, 1)))

    def interleave[A] (s1: Stream[A], s2: Stream[A]): Stream[A] = (s1, s2) match
      case (Empty(), Empty())           => empty()
      case (Cons(h, t), Empty())        => cons(h(), interleave(t(), empty()))
      case (Empty(), Cons(h, t))        => cons(h(), interleave(empty(), t()))
      case (Cons(h1, t1), Cons(h2, t2)) => cons(h1(), cons(h2(), interleave(t1(), t2())))

    def cycle[A](list: Sequence[A]): Stream[A] =
      def _cycle(list: Sequence[A], originalList: Sequence[A]): Stream[A] = list match
        case Sequence.Nil()                   => empty()
        case Sequence.Cons(h, Sequence.Nil()) => cons(h, cycle(originalList))
        case Sequence.Cons(h, t)              => cons(h, _cycle(t, originalList))
      _cycle(list, list)

  end Stream


@main def tryStreams =
  import Streams.*
  import Sequences.Sequence.*

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  val str5 = Stream.takeWhile(Stream.iterate(0)(_ + 1))(_ < 5)
  val str6 = Stream.fill(3)("a")
  val str7 = Stream.take(Stream.fibonacci())(5)
  val s1 = Stream.take(Stream.iterate(1)(_ + 2))(3)
  val s2 = Stream.take(Stream.iterate(2)(_ + 2))(5)
  val str8 = Stream.interleave(s1, s2)
  val repeat = Stream.cycle(Cons("a", Cons("b", Cons ("c", Nil()))))
  val str9 = Stream.take(repeat)(5)

  println(Stream.toList(str5)) //  Cons (0 , Cons (1 , Cons (2 , Cons (3 , Cons (4 , Nil ())))))
  println(Stream.toList(str6)) // Cons (a, Cons (a, Cons (a, Nil ())))
  println(Stream.toList(str7)) // Cons (0 , Cons (1 , Cons (1 , Cons (2 , Cons (3 , Nil ()))))
  println(Stream.toList(str8)) //  Cons (1 , Cons (2 , Cons (3 , Cons (4 , Cons (5 , Cons (6 , Cons (8 , Cons (10, Nil())))))))
  println(Stream.toList(str9)) // Cons (a, Cons (b, Cons (c, Cons (a, Cons (b, Nil ())))))

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  //println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]
