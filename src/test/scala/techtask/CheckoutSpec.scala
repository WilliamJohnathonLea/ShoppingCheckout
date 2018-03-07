package techtask

import org.scalatest.{Matchers, WordSpec}

class CheckoutSpec extends WordSpec with Matchers {

  "The checkout" when {

    "there no offers" should {

      "contain the sum cost of all the items" in {

        val items = Seq(
          Item('A', 50), Item('A', 50), Item('A', 50),
          Item('B', 30), Item('B', 30)
        )

        val checkout = Checkout(0, items)

        Checkout.checkout(checkout, Set.empty).total shouldBe 210

      }

    }

    "there is one offer" should {

      "contain the offer price of the items which qualify" in {
        val items = Seq(
          Item('A', 50), Item('A', 50), Item('A', 50)
        )

        val offers = Set(
          Offer(xForY(3, 130), Some('A'))
        )

        val checkout = Checkout(0, items)

        Checkout.checkout(checkout, offers).total shouldBe 130
      }

      "contain the offer price of the items which qualify plus the sum cost of the spare items" in {
        val items = Seq(
          Item('A', 50), Item('A', 50), Item('A', 50),
          Item('B', 30), Item('B', 30)
        )

        val offers = Set(
          Offer(xForY(3, 130), Some('A'))
        )

        val checkout = Checkout(0, items)

        Checkout.checkout(checkout, offers).total shouldBe 190
      }

    }

    "there are multiple offers" should {

      "contain the offer prices of the items which qualify" in {
        val items = Seq(
          Item('A', 50), Item('A', 50), Item('A', 50),
          Item('B', 30), Item('B', 30)
        )

        val offers = Set(
          Offer(xForY(3, 130), Some('A')),
          Offer(xForY(2, 45), Some('B'))
        )

        val checkout = Checkout(0, items)

        Checkout.checkout(checkout, offers).total shouldBe 175
      }

      "contain the offer prices of the items which qualify plus the sum cost of the spare items" in {
        val items = Seq(
          Item('A', 50), Item('A', 50), Item('A', 50),
          Item('B', 30), Item('B', 30),
          Item('C', 10)
        )

        val offers = Set(
          Offer(xForY(3, 130), Some('A')),
          Offer(xForY(2, 45), Some('B'))
        )

        val checkout = Checkout(0, items)

        Checkout.checkout(checkout, offers).total shouldBe 185
      }

    }

  }

}
