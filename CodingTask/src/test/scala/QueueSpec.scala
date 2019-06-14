import org.scalatest.{FunSpec, Matchers}

class QueueSpec extends FunSpec with Matchers {
  describe("An empty Queue") {
    it("isEmpty should return true") {
      Queue.empty[Int].isEmpty shouldBe true
    }
    it("head should return None") {
      Queue.empty[Int].head shouldBe None
    }
  }
}
