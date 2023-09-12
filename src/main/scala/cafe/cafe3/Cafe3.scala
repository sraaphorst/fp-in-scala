package org.vorpal
package cafe.cafe3

import scala.annotation.targetName

// A functional solution.
// buyCoffee now returns a pair of a Coffee and Charge.
// The system that processes the payments in not involved here at all.
class Cafe:
  def buyCoffee(cc: CreditCard): (Coffee, Charge) =
    val cup = Coffee()
    // Separated the concern of creating a charge from the processing of the charge.
    (cup, Charge(cc, cup.price))

  // We can now reuse buyCoffee directly to define the buyCoffees
  // function, and both functions are trivially testable without having
  // to define complicated stub implementations of some Payments interface.
  // The Cafe is completely ignorant of how the Charge values will be processed.
  // We can still have a Payments class for actually processing charges,
  // but Cafe does not need to know about it.
  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) =
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    val reduced = charges.reduce(_ + _)
    (coffees, reduced)

class CreditCard(override val toString: String)

case class Charge(cc: CreditCard, amount: Double):
  @targetName("combine")
  def +(other: Charge): Charge =
    if cc == other.cc then
      Charge(cc, amount + other.amount)
    else
      throw Exception("Cannot combine charges across different credit cards.")

// Charge is first-class: we want to coalesce any same-card charges into a List[Charge].
// Given a bunch of charges, group them by their associated credit card and combine them.
// Then we end up with a list of charges per credit card.
def coalesce(charges: List[Charge]): List[Charge] =
//  shorthand: charges.groupBy(_.cc).values.map(_.reduce(_.combine(_))).toList
charges
  .groupBy(_.cc)
  .values // Iterable[List[Charge]], where they are grouped by credit card.
  .map(_.reduce(_ + _)) // Reduce the charges for a credit card down to a single charge per card.
  .toList // Finally, we have a list of charges, i.e. credit card and the total charge to each.

class Coffee:
  val price: Double = 2.0

@main
  def main(): Unit =
    val cafe = Cafe()
    val cc1 = CreditCard("Visa1")
    val cc2 = CreditCard("MC1")

    val orders = List(
      cafe.buyCoffees(cc1, 4),
      cafe.buyCoffees(cc2, 7),
      cafe.buyCoffee(cc1)
    )

    // Coalesce the orders together.
    coalesce(orders.map(_._2)).foreach { charge =>
      println(f"Total charge for ${charge.cc} is $$${charge.amount}%.2f.")
    }
