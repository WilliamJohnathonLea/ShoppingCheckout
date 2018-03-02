package techtask

import org.scalatest.{Matchers, WordSpec}

class CheckoutSpec extends WordSpec with Matchers {

  "The checkout" when {

    "no offers apply" should {

      "return the sum total of the items" in {

        val items: Seq[Item] = Seq(Item("A", 50), Item("B", 30))
        val offer = Offer(3 , 130)

        Checkout.getTotal(items, Set(offer)) shouldBe 80
      }

    }

    "there is an applicable offer with no spare items" should {

      "return the sum total adjusted for the offer" in {

        val items: Seq[Item] = Seq(Item("A", 50), Item("A", 50), Item("A", 50))
        val offer = Offer(3 , 130)

        Checkout.getTotal(items, Set(offer)) shouldBe 130
      }

    }

    "there is an applicable offer with spare items" should {

      "return the sum total adjusted for the offer" in {

        val items: Seq[Item] = Seq(Item("A", 50), Item("A", 50), Item("A", 50), Item("B", 30))
        val offer = Offer(3 , 130)

        Checkout.getTotal(items, Set(offer)) shouldBe 160
      }

    }

  }

}
