package it.unibo.pps.u03

import org.junit.Assert.assertEquals
import org.junit.Test

object Lab03Test extends App:

  class Task1Test:

    import u03.Sequences.*
    import Sequence.Cons
    import Sequence.Nil
    import Lab03.Task1.*
    import u03.Optionals.*
    import Optional.*

    val sequence: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

    @Test def testSkip() =
      assertEquals(Cons(30, Nil()), skip(sequence)(2))
      assertEquals(Nil(), skip(sequence)(3))
      assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), skip(sequence)(0))
      assertEquals(Nil(), skip(Nil())(2))

    @Test def testZip() =
      val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
      assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(sequence, l2))
      assertEquals(Nil(), zip(sequence, Nil()))
      assertEquals(Nil(), zip(Nil(), l2))
      assertEquals(Nil(), zip(Nil(), Nil()))

    @Test def testConcat() =
      val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
      assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(sequence, l2))
      assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

    @Test def testReverse() =
      assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(sequence))
      assertEquals(Nil(), reverse(Nil()))

    @Test def testFlatMap() =
      assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(sequence)(v => Cons(v + 1, Nil())))
      assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

    @Test def testMin() =
      assertEquals(Just(10), min(sequence))
      assertEquals(Just(1), min(Cons(1, Nil())))
      assertEquals(Empty(), min(Nil()))

    @Test def testEvenIndices() =
      assertEquals(Cons(10, Cons(30, Nil())), evenIndices(sequence))
      assertEquals(Nil(), evenIndices(Nil()))

    @Test def testContains() =
      assertEquals(true, contains(sequence)(10))
      assertEquals(false, contains(sequence)(15))
      assertEquals(false, contains(Nil())(10))

    @Test def testDistinct() =
      assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), distinct(sequence))
      assertEquals(Cons(10, Cons(20, Nil())), distinct(Cons(10, Cons(20, Cons(10, Nil())))))
      assertEquals(Nil(), distinct(Nil()))

    @Test def testGroup() =
      val sequence = Cons(10, Cons(10, Cons(20, Cons(30, Cons(20, Nil())))))
      val grouped =
        Cons(Cons(10, Cons(10, Nil())), Cons(Cons(20, Nil()), Cons(Cons(30, Nil()), Cons(Cons(20, Nil()), Nil()))))
      assertEquals(group(sequence), grouped)
      assertEquals(Nil(), group(Nil()))

    @Test def testPartition() =
      val sequence = Cons(11, Cons(20, Cons(31, Nil())))
      val (even, odd) = partition(sequence)(x => x % 2 == 0)
      assertEquals(Cons(20, Nil()), even)
      assertEquals(Cons(11, Cons(31, Nil())), odd)

  class Task2Test:

    import Lab03.Task2.*
    import Person.*
    import u03.Sequences.*
    import Sequence.Cons
    import Sequence.Nil
    import Lab03.Task1.*

    @Test def tryCourses(): Unit =
      val sequence1 = Cons(Student("Sarti", 2003), Cons(Teacher("Aguzzi", "PPS"), Nil()))
      assertEquals(getCourses(sequence1), Cons("PPS", Nil()))

    @Test def tryFoldLeft(): Unit =
      val list = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
      val expectedValue = -16
      assertEquals(foldLeft(list)(0)(_ - _), expectedValue)

    @Test def tryGetCoursesNumber(): Unit =
      val list = Cons(Teacher("Viroli", "PPS"), Cons(Student("Sarti", 2003),  Cons(Teacher("Aguzzi", "PPS"), Cons(Teacher("Ricci", "PCD"), Nil()))))
      val coursesNumber = 2
      assertEquals(getCoursesNumber(list), coursesNumber)

  class Task3Test:

    