package techtask

import scala.annotation.tailrec


case class Item(tag: Char, price: Int)

case class Checkout(total: Int, itemsLeft: Seq[Item])

object Checkout {

  @tailrec
  def checkout(co: Checkout, offers: Set[Offer]): Checkout = (co, offers) match {
    case (Checkout(_, Nil), _) => co
    case (Checkout(total, items), os) if os.isEmpty =>
      val finalTotal = items.foldLeft(total)(_ + _.price)
      Checkout(finalTotal, Nil)
    case (_, os) => checkout(os.head.func(os.head.tag)(co), os.tail)
  }

}
