package org.vorpal
package myeither

import mylist.MyList
import mylist.{sum, flatten}
import showable.Showable
import showable.Showable.given

import scala.annotation.targetName
import scala.util.control.NonFatal

// If we want to make MyLeft Showable depending only on E being Showable, and similarly for MyRight,
// then we cannot use an enum and must use sealed class MyEither and case classes for MyLeft and MyRight.
enum MyEither[+E, +A]:
  case MyLeft(value: E)
  case MyRight(value: A)

  def isRight: Boolean = this match
    case MyRight(_) => true
    case MyLeft(_) => false

  def isLeft: Boolean = this match
    case MyRight(_) => false
    case MyLeft(_) => true

  def swap: MyEither[A, E] = this match
    case MyRight(a) => MyLeft(a)
    case MyLeft(e) => MyRight(e)

  def map[AA](f: A => AA): MyEither[E, AA] = this match
    case MyRight(a) => MyRight(f(a))
    case MyLeft(e) => MyLeft(e)

  def lmap[EE](g: E => EE): MyEither[EE, A] = this match
    case MyRight(a) => MyRight(a)
    case MyLeft(e) => MyLeft(g(e))

  def bimap[EE, AA](l: E => EE, r: A => AA): MyEither[EE, AA] =
    map(r).lmap(l)

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match
    case MyRight(a) => f(a)
    case MyLeft(e) => MyLeft(e)

  def orElse[EE >: E, B >: A](or: => MyEither[EE, B]): MyEither[EE, B] = this match
    case MyRight(a) => MyRight(a)
    case MyLeft(_) => or

object MyEither:
  // In order to handle Nothing properly, we make sure Showable[Nothing] does not exist,
  // and we define specific instances for when one side is Nothing.
  // Note that we need to name these explicitly to be different, or there will be clashes
  // between the givens.
  // This allows us to perform operations like:
  //
  // print(MyRight(5).show) <--- MyRight is MyEither[Nothing, Int].
  given [E: Showable, A: Showable]: Showable[MyEither[E, A]] with
    extension (e: MyEither[E, A]) def show: String =
      e match
        case MyLeft(e) => s"MyLeft(${e.show})"
        case MyRight(a) => s"MyRight(${a.show})"

  given nothingLeft[A: Showable]: Showable[MyEither[Nothing, A]] with
    extension (e: MyEither[Nothing, A]) def show: String = e match
      case MyRight(a) => s"MyRight(${a.show})"
      case _ => ""

  given nothingRight[E: Showable]: Showable[MyEither[E, Nothing]] with
    extension (e: MyEither[E, Nothing]) def show: String = e match
      case MyLeft(e) => s"MyLeft(${e.show})"
      case _ => ""

  // Reimplementation of mean using Either.
  def mean(xs: MyList[Double]): MyEither[Throwable, Double] =
    xs.sum /? xs.length

  // More generic code to catch a non-fatal throwable.
  def catchNonFatal[A](a: => A): MyEither[Throwable, A] =
    try MyRight(a)
    catch case NonFatal(t) => MyLeft(t)

  // Now we can do things like this:
  def parseDivision(numerator: String, denominator: String): MyEither[Throwable, Double] =
    for
      n <- catchNonFatal(numerator.toInt)
      d <- catchNonFatal(denominator.toInt)
      r <- catchNonFatal(n / d)
    yield r

  // Continue and stop at the first error or gather the entire list of rights.
  def sequence[E, A](lst: MyList[MyEither[E, A]]): MyEither[E, MyList[A]] =
    lst.traverse(identity)

  // Continue and gather all the errors or gather the entire list of rights.
  def sequenceAll[E, A](lst: MyList[MyEither[MyList[E], A]]): MyEither[MyList[E], MyList[A]] =
    lst.traverseAll(identity)

  // The MyLeft can contain EA or EB.
  def map2[EA, A, EB, B, C](ea: MyEither[EA, A], eb: MyEither[EB, B])(f: (A, B) => C): MyEither[EA | EB, C] =
    for
      a <- ea
      b <- eb
    yield f(a, b)

  // We can also define it like this. EC must be a superclass of EA or EB or both.
  // In this case, however, EC will always be either EA or EB, so the above code is preferable.
//  def map2[EA, A, EB, B, EC >: EA | EB, C](ea: MyEither[EA, A], eb: MyEither[EB, B])(f: (A, B) => C): MyEither[EC, C] =
//    for
//      a <- ea
//      b <- eb
//    yield f(a, b)

  // This implementation allows us to report both errors if both parameters are MyLeft.
  def map2Both[EA, A, EB, B, EC >: EA | EB, C](ea: MyEither[EA, A],
                                               eb: MyEither[EB, B])
                                              (f: (A, B) => C): MyEither[MyList[EC], C] =
    (ea, eb) match
      case (MyRight(a), MyRight(b)) => MyRight(f(a, b))
      case (MyLeft(e), MyRight(_)) => MyLeft(MyList(e))
      case (MyRight(_), MyLeft(e)) => MyLeft(MyList(e))
      case (MyLeft(e1), MyLeft(e2)) => MyLeft(MyList(e1, e2))

  // This implementation combines the errors if the lefts contain lists of errors.
  def map2All[EA, A, EB, B, EC >: EA | EB, C](ea: MyEither[MyList[EA], A],
                                              eb: MyEither[MyList[EB], B])
                                             (f: (A, B) => C): MyEither[MyList[EC], C] =
    (ea, eb) match
      case (MyRight(a), MyRight(b)) => MyRight(f(a, b))
      case (MyLeft(es), MyRight(_)) => MyLeft(es)
      case (MyRight(_), MyLeft(es)) => MyLeft(es)
      case (MyLeft(es1), MyLeft(es2)) => MyLeft(MyList(es1, es2).flatten)

extension (numerator: Double)
  @targetName("safeDiv")
  def /?(denominator: Int): MyEither[Throwable, Double] =
    if denominator == 0 then MyEither.MyLeft(ArithmeticException("Division by zero"))
    else MyEither.MyRight(numerator / denominator)

// Now we use Either to validate data.
// We want to collect errors instead of just stop at the first error.
// See the map2Both implementation above.
object ValidationTesting:
  import MyEither.*

  case class Name private (value: String)
  object Name:
    // Note: when creating a case class, Scala creates an apply method by default.
    // Since we are overriding this apply method, we must use the new keyword when creating a Name.
    def apply(name: String): MyEither[String, Name] =
      if name == null then MyLeft("ERROR: name is null")
      else if name == "" then MyLeft("ERROR: name is missing")
      else MyRight(new Name(name))

  given Showable[Name] with
    extension (n: Name) def show: String = s"Name(${n.value.show})"

  case class Age private(value: Int)
  object Age:
    def apply(age: Int): MyEither[String, Age] =
      if age < 0 || age > 150 then MyLeft(s"""ERROR: age "$age" is out of range""")
      else MyRight(new Age(age))

  given Showable[Age] with
    extension (a: Age) def show: String = s"Age(${a.value.show})"

  case class Person(name: Name, age: Age)
  object Person:
    def make(name: String, age: Int): MyEither[MyList[String], Person] =
      MyEither.map2Both(Name(name), Age(age))(Person(_, _))

  given Showable[Person] with
    extension (p: Person) def show: String = s"Person(${p.name.show}, ${p.age.show})"

extension[A] (lst: MyList[A])
  // traverse iterates over the lst, returning the first error encountered if there is one.
  def traverse[E, B](f: A => MyEither[E, B]): MyEither[E, MyList[B]] =
    lst.foldRight[MyEither[E, MyList[B]]](MyEither.MyRight(MyList.MyNil)) { (a, b) =>
      // b is the accumulation of the MyEither[_, List[B]].
      MyEither.map2(f(a), b)(MyList.MyCons.apply)
    }

  // A variant of traverse that returns all errors.
  def traverseAll[E, B](f: A => MyEither[MyList[E], B]): MyEither[MyList[E], MyList[B]] =
    lst.foldRight[MyEither[MyList[E], MyList[B]]](MyEither.MyRight(MyList.MyNil)) { (a, acc) =>
      MyEither.map2All(f(a), acc)(MyList.MyCons.apply)
    }

@main
def main_myeither(): Unit =
  import MyEither.*
  import ValidationTesting.*
  import ValidationTesting.given

  val values = MyList(55.0, 78.3, 11.0, 99.9)
  println(s"Mean of ${values.show} is ${MyEither.mean(values).show}")

  val novalues: MyList[Double] = MyList.MyNil
  println(s"Mean of ${novalues.show} is ${MyEither.mean(novalues).show}")

  println(s"1 / 2 = ${MyEither.parseDivision("1", "2").show}")
  println(s"1 / 0 = ${MyEither.parseDivision("1", "0").show}")
  println(s"1 / q = ${MyEither.parseDivision("1", "q").show}")

  // Show that the different givens work.
  // Note the type here is MyEither[Nothing, Int].
  val right = MyRight(5)
  println(right.show)

  // Note the type here is MyEither[String, Nothing].
  val left = MyLeft("Hello")
  println(left.show)

  // Show that sequence works. It should return a Right[List] if everything is Right,
  // and the first Left if one is in the list.
  val seq1 = MyList(MyRight(4), MyRight(2), MyRight(3), MyRight(1))
  println(s"${seq1.show} -> ${MyEither.sequence(seq1).show}")
  val seq2 = MyList(MyRight(1), MyRight(2), MyLeft("3"), MyRight(4), MyLeft("5"), MyRight(6))
  println(s"${seq2.show} -> ${MyEither.sequence(seq2).show}")
  println()

  println("*** PEOPLE VALIDATION ***")
  // Now show that we can collect errors when creating people.
  // We can even collect a list of errors.
  //  val people = MyList(
  //    Person.make("", -1),
  //    Person.make(null, 25),
  //    Person.make("Aluminium", 600),
  //    Person.make("Smol", 33),
  //    Person.make("Sebastian", 45)
  //  )

  val pairs = MyList(("", -1), (null, 25), ("Aluminium", 600), ("smol", 33), ("Sebastian", 45))
  val people = pairs.map(Person.make.tupled)

  println(s"people data: ${pairs.show}")
  println(s"people result: ${people.show}")

  val p1 = Person.make("Curry", 34)
  val p2 = Person.make("Howard", 45)
  println(s"map2Both pass (works on pair only): ${map2Both(p1, p2)((_, _)).show}")
  println(s"map2All pass (works on pair only):  ${map2All(p1, p2)((_, _)).show}")

  val pe1 = Person.make(null, -1)
  val pe2 = Person.make("", 3289482)
  println(s"map2Both fail (works on pair only): ${map2Both(pe1, pe2)((_, _)).show}")
  println(s"map2All fail (works on pair only):  ${map2All(pe1, pe2)((_, _)).show}")

  // In this case, it gathers all of the errors that occur when people was processed.
  println(s"All errors (works on list): ${MyEither.sequenceAll(people).show}")

  // In this case, it should just show the people that emerged.
  println(s"All people (works on list): ${MyEither.sequenceAll(people.filter(_.isRight)).show}")

