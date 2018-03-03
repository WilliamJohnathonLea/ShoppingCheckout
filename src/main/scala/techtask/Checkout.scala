package techtask

import scala.annotation.tailrec


case class Item(tag: String, price: Int)
case class Offer(numItems: Int, forAmount: Int, tag: String)

case class Checkout(total: Int, itemsLeft: Seq[Item])

object Checkout {

  @tailrec
  def checkout(co: Checkout, offers: Set[Offer]): Checkout = (co, offers) match {
    case (Checkout(_, Nil), _) => co
    case (Checkout(total, items), os) if os.isEmpty =>
      val finalTotal = items.foldLeft(total)(_ + _.price)
      Checkout(finalTotal, Nil)
    case (_, os) => checkout(applyOffer(os.head, co), os.tail)
  }

  private def applyOffer(offer: Offer, co: Checkout): Checkout = {
    val matchedList = co.itemsLeft.filter(_.tag == offer.tag)
    val spareCount = matchedList.length % offer.numItems
    val spareSum = matchedList.take(spareCount).foldLeft(0)(_ + _.price)
    val offerSum = (matchedList.drop(spareCount).length / offer.numItems) * offer.forAmount

    co.copy(co.total + offerSum + spareSum, co.itemsLeft.filterNot(_.tag == offer.tag))
  }

}
