package org.vorpal
package myvalidated

import myeither.MyEither
import mylist.MyList
import myoption.MyOption
import showable.Showable

import org.vorpal.myvalidated.MyValidated.MyInvalid

// Basically taking MyEither[List[E], A] along with functions
// map2All, traverseAll, sequenceAll so that the accumulation behavior
// they offer is named to be more intuitive.

// Note that the -All has been dropped from function names.
enum MyValidated[+E, +A]:
  case MyValid(get: A)
  // This used to be MyList[E], but we want to avoid a direct dependency on any particular collection.
  // To exploit this, we need to modify map2.
  case MyInvalid(error: E)

  def isValid: Boolean = this match
    case MyValid(_) => true
    case MyInvalid(_) => false

  def isInvalid: Boolean = this match
    case MyValid(_) => false
    case MyInvalid(_) => true

  def toEither: MyEither[E, A] = this match
    case MyValid(a) => MyEither.MyRight(a)
    case MyInvalid(e) => MyEither.MyLeft(e)

  def toOption: MyOption[A] = this match
    case MyValid(a) => MyOption.MySome(a)
    case MyInvalid(_) => MyOption.MyNone

  def map[B](f: A => B): MyValidated[E, B] = this match
    case MyValid(a) => MyValid(f(a))
    case MyInvalid(es) => MyInvalid(es)

object MyValidated:
  given[E: Showable, A: Showable]: Showable[MyValidated[E, A]] with
    extension (v: MyValidated[E, A]) def show: String = v match
        case MyInvalid(es) => s"MyInvalid(${es.show})"
        case MyValid(a) => s"MyValid(${a.show})"

  given nothingInvalid[A: Showable]: Showable[MyValidated[Nothing, A]] with
    extension (v: MyValidated[Nothing, A]) def show: String = v match
      case MyValid(a) => s"MyValid(${a.show})"
      case _ => ""

  given nothingValid[E: Showable]: Showable[MyValidated[E, Nothing]] with
    extension (v: MyValidated[E, Nothing]) def show: String = v match
      case MyInvalid(es) => s"MyInvalid(${es.show})"
      case _ => ""

  def map2[EA, A, EB, B, EC >: EA | EB, C](v1: MyValidated[EA, A],
                                           v2: MyValidated[EB, B])
                                          (f: (A, B) => C)
                                          (combineErrors: (EC, EC) => EC): MyValidated[EC, C] =
    (v1, v2) match
      case (MyValid(a), MyValid(b)) => MyValid(f(a, b))
      case (MyInvalid(es), MyValid(_)) => MyInvalid(es)
      case (MyValid(_), MyInvalid(es)) => MyInvalid(es)
      case (MyInvalid(es1), MyInvalid(es2)) => MyInvalid(combineErrors(es1, es2))

  def fromEither[E, A](e: MyEither[E, A]): MyValidated[E, A] = e match
    case MyEither.MyRight(a) => MyValid(a)
    case MyEither.MyLeft(e) => MyInvalid(e)

  def sequence[E, A](vs: MyList[MyValidated[E, A]])(combineErrors: (E, E) => E): MyValidated[E, MyList[A]] =
    vs.traverse(identity)(combineErrors)

extension[A] (lst: MyList[A])
  // Iterate over a list of A, attempting to validate them via f into B.
  // Return a list of the valid entries.
  def traverse[E, B](f: A => MyValidated[E, B])(combineErrors: (E, E) => E): MyValidated[E, MyList[B]] =
    lst.foldRight[MyValidated[E, MyList[B]]](MyValidated.MyValid(MyList.MyNil)) { (a, acc) =>
      MyValidated.map2(f(a), acc)(MyList.MyCons.apply)(combineErrors)
    }


@main
def main_myvalidated(): Unit =
  import myeither.ValidationTesting.*

  val pairs = MyList((null, -11), ("Yagungles", 14), ("Too old", 200), ("", 11), ("Andrew", 33), ("Sebastian", 45))
  val people = pairs.map(Person.make.tupled).map(MyValidated.fromEither)

  println(s"people data: ${pairs.show}")
  println(s"people result: ${people.show}")

  // Now instead
  println(s"Errors: ${MyValidated.sequence(people)(_.append(_)).show}")
  println(s"Correct: ${MyValidated.sequence(people.filter(_.isValid))(_.append(_)).show}")
