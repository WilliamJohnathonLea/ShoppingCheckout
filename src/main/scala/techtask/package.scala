package object techtask {

  type OfferF = Option[Char] => Checkout => Checkout

  private def inOffer(tag: Option[Char], items: Seq[Item]): Seq[Item] = {
    tag.fold(items)(t => items.filter(_.tag == t))
  }

  private def notInOffer(tag: Option[Char], items: Seq[Item]): Seq[Item] = {
    tag.fold(items)(t => items.filter(_.tag != t))
  }

  def xForY(n: Int, p: Int)(tag: Option[Char])(co: Checkout): Checkout = {
    val offerItems = inOffer(tag, co.itemsLeft)
    val rem = offerItems.length % n
    val spareSum = offerItems.take(rem).foldLeft(0)(_ + _.price)
    val offerSum = (offerItems.drop(rem).length / n) * p

    tag match {
      case None => co
      case _ => Checkout(co.total + offerSum + spareSum, notInOffer(tag, co.itemsLeft))
    }
  }

}
