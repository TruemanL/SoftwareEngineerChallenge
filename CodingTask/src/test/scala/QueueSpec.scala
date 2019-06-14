import org.scalatest.{FunSpec, Matchers}

class QueueSpec extends FunSpec with Matchers {
  describe("An empty Queue:") {
    it("isEmpty should return true") {
      Queue.empty[Int].isEmpty shouldBe true
    }
    it("head should return None") {
      Queue.empty[Int].head shouldBe None
    }
  }

  describe("An non-empty Queue:") {
    val onlyElement: Int = 1
    val nonEmptyQueue = Queue.empty[Int].enQueue(onlyElement)
    it("isEmpty should return false") {
      nonEmptyQueue.isEmpty shouldBe false
    }
    it("head should return the element at the beginning of the queue") {
      nonEmptyQueue.head shouldBe Some(onlyElement)
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
    val testElements = Vector(1, 2, 3, 4, 5)
    val testQueue = Queue.empty[Int]
    it("enQueuing and deQueuing items should maintain FIFO order") {
    }
  }
}
