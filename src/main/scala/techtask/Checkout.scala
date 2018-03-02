package techtask

object Checkout {

  def getTotal(items: Seq[Item], offers: Set[Offer]): Int = {
    val tags = items.map(_.tag).toSet

    val offerTotals: Set[Int] = for {
      offer <- offers
      tag <- tags
    } yield offer.on(tag, items)

    offerTotals.sum
  }

}
