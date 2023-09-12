package org.vorpal
package cafe.cafe2

// CreditCard no longer has any methods: ignorant of concerns.
//
class Cafe:
  // By passing Payments here, we have achieved dependency injection.
  def buyCoffee(cc: CreditCard, p: Payments): Coffee =
    val cup = Coffee()
    p.charge(cc, cup.price)
    cup

class CreditCard(val description: String)

// Extracted logic to charge into the Payments interface: dependency injection.
// Side effects still occur when we call p.charge.
trait Payments:
  def charge(cc: CreditCard, price: Double): Unit

class SimulatedPayments extends Payments:
  def charge(cc: CreditCard, price: Double): Unit =
    println(f"Charging $$$price%.2f to ${cc.description}.")

class Coffee:
  val price: Double = 2.0

@main
def main(): Unit =
  val cafe = Cafe()
  val cc = CreditCard("Visa")
  val p = SimulatedPayments()
  val c = cafe.buyCoffee(cc, p)



