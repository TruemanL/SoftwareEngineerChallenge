trait Queue[T] {
  def isEmpty: Boolean
  def enQueue(t: T): Queue[T]
  // Removes the element at the beginning of the immutable queue, and returns the new queue.
  def deQueue(): Queue[T]
  def head: Option[T]
}

object Queue {
  def empty[T]: Queue[T] = MyQueue[T](Vector.empty[T])
}

case class MyQueue[T](list: Vector[T]) extends Queue[T] {
  override def isEmpty: Boolean = list.isEmpty

  override def enQueue(t: T): Queue[T] = MyQueue[T](list :+ t)

  override def deQueue(): Queue[T] = MyQueue[T](list.tail)

  override def head: Option[T] = list.headOption
}