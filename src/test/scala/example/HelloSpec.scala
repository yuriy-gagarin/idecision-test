package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HelloSpec extends AnyFlatSpec with Matchers {
  "Retry" should "_" in {
    
    def throws(calls: Int) = {
      var callNo = 0

      () => {
        if (callNo <= calls) {
          callNo = callNo + 1
          throw new Exception
        } else ()
      }
    }

    val throwsTwice1 = throws(2)

    assertThrows[Exception] {
      RetryFunction.retry(throwsTwice1(), 1) 
    }

    val throwsTwice2 = throws(2)

    assertThrows[Exception] {
      RetryFunction.retry(throwsTwice2(), 2)  
    }

    val throwsTwice3 = throws(2)

    RetryFunction.retry(throwsTwice3(), 3) shouldEqual ()
  }

  "Peeking iterator" should "_" in {
    val iter = ExtendedIterator.peeking(List(1,2,3,4,5).iterator)

    iter.peek shouldEqual Some(1)
    iter.hasNext shouldEqual true

    val el1 = iter.next()

    el1 shouldEqual 1
    iter.peek shouldEqual Some(2)
    iter.hasNext shouldEqual true

    val el2 = iter.next()

    el2 shouldEqual 2
    iter.hasNext shouldEqual true

    val el3 = iter.next()

    el3 shouldEqual 3
    iter.peek shouldEqual Some(4)
    iter.hasNext shouldEqual true

    val el4 = iter.next()
    val el5 = iter.next()

    iter.peek shouldEqual None
    iter.hasNext shouldEqual false

    el4 shouldEqual 4
    el5 shouldEqual 5
  }
}
