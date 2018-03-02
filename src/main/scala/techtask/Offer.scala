package techtask

case class Offer(numItems: Int, forAmount: Int) {

  def on(tag: String, items: Seq[Item]): Int = {
    this(items.filter(_.tag == tag))
  }

  def apply(items: Seq[Item]): Int = {
    val mod = items.length % numItems
    val spareItemsSum = items.take(mod).foldLeft(0)(_ + _.price)
    val offerSum = (items.drop(mod).length / numItems) * forAmount
    offerSum + spareItemsSum
  }

}
