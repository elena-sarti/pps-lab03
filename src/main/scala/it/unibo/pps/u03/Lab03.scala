package it.unibo.pps.u03

import u03.Optionals.Optional

import scala.annotation.tailrec

object Lab03 extends App:

  //Task 1, svolto da sola
  object Task1:

    import u03.Sequences.*
    import Sequence.*

    /*
    * Skip the first n elements of the sequence
    * E.g., [10, 20, 30], 2 => [30]
    * E.g., [10, 20, 30], 3 => []
    * E.g., [10, 20, 30], 0 => [10, 20, 30]
    * E.g., [], 2 => []
    */
    @tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = (s, n) match
      case (Nil(), _)            => Nil()
      case (Cons(head, tail), 0) => Cons(head, tail)
      case (Cons(head, tail), n) => skip(tail)(n - 1)

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Nil(), _)                   => Nil()
      case (_, Nil())                   => Nil()
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case (Nil(), Nil())               => Nil()
      case (Nil(), Cons(h, t))          => Cons(h, t)
      case (Cons(h, t), Nil())          => Cons(h, t)
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1, concat(t1, Cons(h2, t2)))

    /*
    * Reverse the sequence
    * E.g., [10, 20, 30] => [30, 20, 10]
    * E.g., [10] => [10]
    * E.g., [] => []
    */
    def reverse[A](s: Sequence[A]): Sequence[A] = s match
      case Nil()      => Nil()
      case Cons(h, t) => concat(reverse(t), Cons(h, Nil()))

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = (s, mapper) match
      case (_, Nil)        => Nil()
      case (Nil(), _)      => Nil()
      case (Cons(h, t), f) => concat(f(h), flatMap(t)(f))

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    @tailrec
    def min(s: Sequence[Int]): Optional[Int] = s match
      case Nil()                           => Optional.Empty()
      case Cons(h, Nil())                  => Optional.Just(h)
      case Cons(h, Cons(h1, t1)) if h < h1 => min(Cons(h, t1))
      case Cons(h, Cons(h1, t1))           => min(Cons(h1, t1))

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
      case Nil()                 => Nil()
      case Cons(h, Nil())        => Cons(h, Nil())
      case Cons(h, Cons(h1, t1)) => Cons(h, evenIndices(t1))

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    @tailrec
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Nil()                   => false
      case Cons(h, Nil())          => h == elem
      case Cons(h, t) if h != elem => contains(t)(elem)
      case _                       => true

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def _distinct(sequence: Sequence[A], acc: Sequence[A]): Sequence[A] = sequence match
        case Nil()                              => acc
        case Cons(h, Nil()) if contains(acc)(h) => acc
        case Cons(h, Nil())                     => concat(acc, Cons(h, Nil()))
        case Cons(h, t) if contains(acc)(h)     => _distinct(t, acc)
        case Cons(h, t)                         => _distinct(t, concat(acc, Cons(h, Nil())))
      _distinct(s, Nil())

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = s match
      case Nil()                            => Nil()
      case Cons(h, Nil())                   => Cons(Cons(h, Nil()), Nil())
      case Cons(h, Cons(h1, t1)) if h == h1 => Cons(Cons(h, Cons(h1, Nil())), group(t1))
      case Cons(h, t)                       => Cons(Cons(h, Nil()), group(t))

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      @tailrec
      def recursivePartition(sequence: Sequence[A], f: A => Boolean, acc1: Sequence[A], acc2: Sequence[A]): (Sequence[A], Sequence[A]) = sequence match
        case Nil()                  => (acc1, acc2)
        case Cons(h, Nil()) if f(h) => (concat(acc1, Cons(h, Nil())), concat(acc2, Nil()))
        case Cons(h, Nil())         => (acc1, concat(acc2, Cons(h, Nil())))
        case Cons(h, t) if f(h)     => recursivePartition(t, f, concat(acc1, Cons(h, Nil())), acc2)
        case Cons(h, t)             => recursivePartition(t, f, acc1, concat(acc2, Cons(h, Nil())))
      recursivePartition(s, pred, Nil(), Nil())


  //Task 2, svolto da sola
  object Task2:

    enum Person:
      case Student(name: String, year: Int)
      case Teacher(name: String, course: String)

    object Person:

      import u03.Sequences.*
      import Sequence.*

      def name(p: Person): String = p match
        case Student(n, _) => n
        case Teacher(n, _) => n

      def teacherToCourse(p: Person): String = p match
        case Teacher(_, course) => course

      def isTeacher(p: Person): Boolean = p match
        case Student(_, _) => false
        case _             => true

      def getCourses(sequence: Sequence[Person]): Sequence[String] =
        map(filter(sequence)(isTeacher))(teacherToCourse)

      @tailrec
      def foldLeft(sequence: Sequence[Int])(acc: Int)(f: (Int, Int) => Int): Int = sequence match
        case Nil()          => f(acc, 0)
        case Cons(h, Nil()) => f(acc, h)
        case Cons(h, t)     => foldLeft(t)(f(acc, h))(f)

      def getCoursesNumber(sequence: Sequence[Person]): Int =
        length(Task1.distinct(getCourses(sequence)))


  //Task 3, svolto da sola
  object Task3

    enum Stream[A]:
      private case Empty()
      private case Cons(head: () => A, tail: () => Stream[A])

    object Stream:

      import u03.Sequences.*
      import Sequence.*

      def empty[A](): Stream[A] = Empty()

      def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)

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

      def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = (s1, s2) match
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