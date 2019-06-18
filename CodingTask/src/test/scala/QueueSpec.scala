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

//  describe("A Queue should behave as expected") {
//    val testElements = Vector(1, 2, 3, 4, 5)
//    val testQueue = Queue.empty[Int]
//    it("enQueuing and deQueuing items should maintain FIFO order") {
//    }
//  }
}
