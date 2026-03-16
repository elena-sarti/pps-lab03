package it.unibo.pps.u03

import scala.annotation.tailrec

object Task2 extends App:

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
      case _ => true

    def getCourses(sequence: Sequence[Person]): Sequence[String] =
      map(filter(sequence)(isTeacher))(teacherToCourse)

    @main
    def tryCourses(): Unit =
      val sequence = Cons(Student("Elena Sarti", 2003), Cons(Teacher("Gianluca Aguzzi", "PPS"), Nil()))
      println(getCourses(sequence))


    @tailrec
    def foldLeft(sequence: Sequence[Int])(acc: Int)(f: (Int, Int) => Int): Int = sequence match
      case Nil() => f(acc, 0)
      case Cons(h, Nil()) => f(acc, h)
      case Cons(h, t) => foldLeft(t)(f(acc, h))(f)

    @main
    def tryFoldLeft(): Unit =
      val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
      println(foldLeft(lst)(0)(_ - _))