package techtask

import org.scalatest.{Matchers, WordSpec}

class OfferSpec extends WordSpec with Matchers {

  val startingCheckout = Checkout(0, Seq(Item('A', 50), Item('A', 50), Item('A', 50)))

  "The xForY offer function" when {

    "applied without a tag" should {

      "return the checkout as is" in {

        val result = xForY(3, 130)(None)(startingCheckout)

        result shouldEqual startingCheckout
      }

    }

    "applied with a tag matching item in the checkout" should {

      "return a checkout with the offer applied" in {

        val result = xForY(3, 130)(Some('A'))(startingCheckout)
        val expected = Checkout(130, Seq.empty)

        result shouldEqual expected
      }

    }

    "applied with a tag not matching item in the checkout" should {

      "return a checkout without applying the offer" in {

        val result = xForY(3, 130)(Some('B'))(startingCheckout)
        val expected = Checkout(0, Seq(Item('A', 50), Item('A', 50), Item('A', 50)))

        result shouldEqual expected
      }

    }

  }

}
