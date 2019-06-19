import org.scalatest.{FunSpec, Matchers}

class QueueSpec extends FunSpec with Matchers {
  val emptyQueue: Queue[Int] = Queue.empty[Int]
  describe("An empty Queue:") {
    it("isEmpty should return true") {
      emptyQueue.isEmpty shouldBe true
    }
    it("enQueue(element) should return a non-empty Queue") {
      emptyQueue.enQueue(1).isEmpty shouldBe false
    }
    it("deQueue should return another empty Queue") {
      emptyQueue.deQueue().isEmpty shouldBe true
    }
    it("head should return None") {
      emptyQueue.head shouldBe None
    }
  }

  describe("An non-empty Queue:") {
    val originalHeadElement: Int = 1
    val otherElement: Int = 2
    val nonEmptyQueue = emptyQueue.enQueue(originalHeadElement)
    it("isEmpty should return false") {
      nonEmptyQueue.isEmpty shouldBe false
    }
    it("enQueue(otherElement) to a non-empty Queue, will not have otherElement as head") {
      nonEmptyQueue.enQueue(otherElement).head.contains(otherElement) shouldBe false
      nonEmptyQueue.enQueue(otherElement).head.contains(originalHeadElement) shouldBe true
    }
    it("deQueue will return a Queue without the head element") {
      nonEmptyQueue.head.contains(originalHeadElement) shouldBe true
      nonEmptyQueue.deQueue().head.contains(originalHeadElement) shouldBe false
    }
    it("head should return the element at the beginning of the queue") {
      nonEmptyQueue.head shouldBe Some(originalHeadElement)
    }
  }

  describe("Queue.head") {
    val firstElement: Int = 1
    val secondElement: Int = 2
    val nonEmptyQueue = Queue.empty[Int].enQueue(firstElement).enQueue(secondElement)
    it("should not remove element from Queue") {
      nonEmptyQueue.head shouldBe Some(firstElement)
      nonEmptyQueue.head shouldBe Some(firstElement)
      nonEmptyQueue.isEmpty shouldBe false
    }
    it("should return next element if the original head got deQueued") {
      nonEmptyQueue.head shouldBe Some(firstElement)
      val dequeuedQueue = nonEmptyQueue.deQueue()
      dequeuedQueue.head shouldBe Some(secondElement)
      dequeuedQueue.isEmpty shouldBe false
    }
    it("should return none if all existing elements have been deQueued") {
      nonEmptyQueue.head shouldBe Some(firstElement)
      val dequeuedQueue = nonEmptyQueue.deQueue()
      dequeuedQueue.head shouldBe Some(secondElement)
      val emptiedQueue = dequeuedQueue.deQueue()
      emptiedQueue.isEmpty shouldBe true
    }
  }

  describe("A Queue should behave as expected") {
    val testElements = (1 to 10000).toVector
    it("enQueuing and deQueuing items should maintain FIFO order for many items") {
      val filledUpTestQueue = testElements.foldLeft(Queue.empty[Int]){
        (queue, element) => queue.enQueue(element)
      }
      var testQueue = filledUpTestQueue
      testQueue.isEmpty shouldBe false
      for (element <- testElements) {
        testQueue.head shouldBe Some(element)
        testQueue = testQueue.deQueue()
      }
      testQueue.isEmpty shouldBe true
    }

    it("enQueuing and deQueuing items should maintain FIFO order for enQueues interleaved with deQueues") {
      var testQueue = Queue.empty[Int]
      testQueue.isEmpty shouldBe true
      testQueue = testQueue.enQueue(1)
      testQueue = testQueue.enQueue(2)
      testQueue = testQueue.enQueue(3)

      testQueue.head shouldBe Some(1)
      testQueue = testQueue.deQueue()
      testQueue.head shouldBe Some(2)
      testQueue = testQueue.deQueue()

      testQueue = testQueue.enQueue(4)
      testQueue = testQueue.enQueue(5)
      testQueue.isEmpty shouldBe false

      testQueue.head shouldBe Some(3)
      testQueue = testQueue.deQueue()
      testQueue.head shouldBe Some(4)
      testQueue = testQueue.deQueue()
      testQueue.head shouldBe Some(5)
      testQueue = testQueue.deQueue()
      testQueue.isEmpty shouldBe true
    }
  }

  describe("A Queue should also work for non-integer types") {
    it("enQueuing and deQueuing items should maintain FIFO order for enQueues interleaved with deQueues") {
      var testQueue = Queue.empty[String]
      testQueue.isEmpty shouldBe true
      testQueue = testQueue.enQueue("a")
      testQueue = testQueue.enQueue("b")
      testQueue = testQueue.enQueue("c")

      testQueue.head shouldBe Some("a")
      testQueue = testQueue.deQueue()
      testQueue.head shouldBe Some("b")
      testQueue = testQueue.deQueue()

      testQueue = testQueue.enQueue("d")
      testQueue = testQueue.enQueue("e")
      testQueue.isEmpty shouldBe false

      testQueue.head shouldBe Some("c")
      testQueue = testQueue.deQueue()
      testQueue.head shouldBe Some("d")
      testQueue = testQueue.deQueue()
      testQueue.head shouldBe Some("e")
      testQueue = testQueue.deQueue()
      testQueue.isEmpty shouldBe true
    }
  }
}
