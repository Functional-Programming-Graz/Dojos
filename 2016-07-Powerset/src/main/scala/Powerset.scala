object Powerset {
  def apply[A](s: Stream[A]): Stream[Stream[A]] = {
    def diagnal(previous: Stream[Stream[A]], elements: Stream[A]): Stream[Stream[A]] = {
      if (elements.isEmpty) {
        Stream.empty
      } else {
        val head = elements.head
        val next: Stream[Stream[A]] = previous.map(head #:: _)

        next #::: diagnal(previous #::: next, elements.tail)
      }
    }
    Stream.empty #:: diagnal(Stream(Stream.empty), s)
  }
}
