package org.vorpal
package cafe.cafe1

// Initial attempt with side effects.
// Testing is difficult: we don't want to actually contact the credit card company,
// nor should we have knowledge of how to persist a record of charge in our system.
class Cafe:
  def buyCoffee(cc: CreditCard): Coffee =
    val cup = Coffee()

    /** SIDE EFFECT: requires interaction with outside world. **/
    cc.charge(cup.price)

    cup

class CreditCard:
  // SIDE EFFECT: involves interaction with the outside world.
  def charge(price: Double): Unit =
    println(f"Charging $$$price%.2f.")

class Coffee:
  val price: Double = 2.0

@main
def main(): Unit =
  val cafe = Cafe()
  val cc = CreditCard()
  val cup = cafe.buyCoffee(cc)
