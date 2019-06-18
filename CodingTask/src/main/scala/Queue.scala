trait Queue[T] {
  // Returns true if the queue is empty, false otherwise
  def isEmpty: Boolean

  // Returns a new Queue with the element t added to the end of the queue
  def enQueue(t: T): Queue[T]

  // Removes the element at the beginning of the immutable queue, and returns the new queue.
  // Note: calling deQueue on an empty queue will return a new empty queue.
  def deQueue(): Queue[T]

  // Optionally returns the element at the start of the queue if the queue is not empty
  // or else returns None if the queue is empty
  def head: Option[T]
}

object Queue {
  def empty[T]: Queue[T] = MyQueue[T](Vector.empty[T])
}

case class MyQueue[T](list: Vector[T]) extends Queue[T] {
  override def isEmpty: Boolean = list.isEmpty

  override def enQueue(t: T): Queue[T] = MyQueue[T](list :+ t)

  override def deQueue(): Queue[T] = if (list.isEmpty) {
    MyQueue[T](Vector.empty[T])
  } else {
    MyQueue[T](list.tail)
  }

  override def head: Option[T] = list.headOption
}