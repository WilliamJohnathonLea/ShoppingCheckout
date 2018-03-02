package techtask

import org.scalatest.{Matchers, WordSpec}

class OfferSpec extends WordSpec with Matchers {

  "An offer" when {

    "applied to a list with no spare items" should {

      "return the offer amount" in {

        val offer = new Offer(3 , 130)

        offer(Seq(Item("A", 50), Item("A", 50), Item("A", 50))) shouldBe 130
      }

    }

    "applied to a list of item with one spare" should {

      "return the offer amount plus tbe normal cost of the spare item" in {

        val offer = new Offer(3 , 130)

        offer(Seq(Item("A", 50), Item("A", 50), Item("A", 50), Item("A", 50))) shouldBe 180
      }

    }

  }

}
