package dojo

trait SimpleList {
  def add(s: String): Unit
  def find(s: String): Option[Node]
  def delete(s: Node): Unit
  def toList: List[String]
}

trait Node {
  def value: String
}

case class SinglyLinkedListNode(value: String) extends Node {
  def delete(s: SinglyLinkedListNode): Unit = next match {
    case Some(n) => if (n == s) {
      next = n.next
    } else {
      n.delete(s)
    }
    case None => ()
  }

  var next: Option[SinglyLinkedListNode] = None

  def toList: List[String] = value :: (next match {
    case None => Nil
    case Some(n) => n.toList
  })

  def add(s: String): Unit = next match {
    case Some(n) => n.add(s)
    case None => next = Some(SinglyLinkedListNode(s))
  }

  def find(s: String): Option[SinglyLinkedListNode] =
    if (value == s) {
      Some(this)
    } else {
      next match {
        case Some(n) => n.find(s)
        case None => None
      }
    }

  override def toString = s"Node($value, $next)"
}

class SinglyLinkedList extends SimpleList {
  private var first: Option[SinglyLinkedListNode] = None
  override def toString = first.toString

  override def add(s: String): Unit = first match {
    case None => first = Some(SinglyLinkedListNode(s))
    case Some(head) => head.add(s)
  }

  override def toList: List[String] = first match {
    case None => List()
    case Some(head) => head.toList
  }

  override def delete(s: Node): Unit =  first match {
    case Some(n) if n == s => first = n.next
    case Some(n) => n.delete(s.asInstanceOf[SinglyLinkedListNode])
    case None => ()
  }

  override def find(s: String): Option[SinglyLinkedListNode] = first match {
    case Some(n)  => n.find(s)
    case _ => None
  }
}

class DoublyLinkedList extends SimpleList {
  var first: Option[DoublyLinkedListNode] = None
  var last: Option[DoublyLinkedListNode] = None

  override def toString = first.toString

  override def add(s: String): Unit = first match {
    case None =>
      first = Some(DoublyLinkedListNode(s))
      last = first
    case Some(head) =>
      last = Some(head.add(s))
  }

  override def toList: List[String] = first match {
    case None => List()
    case Some(head) => head.toList
  }

  override def delete(s: Node): Unit = first match {
    case Some(n) if n == s =>
      first = n.next
      if (first == None) {
        last = None
      }
      s.asInstanceOf[DoublyLinkedListNode].delete()
    case Some(n) =>
      s.asInstanceOf[DoublyLinkedListNode].delete()
      if (last == Some(n)) {
        last = n.prev
      }
    case None => ()
  }

  override def find(s: String): Option[DoublyLinkedListNode] = first match {
    case Some(n)  => n.find(s)
    case _ => None
  }
}

case class DoublyLinkedListNode(value: String) extends Node {
  def delete(): Unit = {
    next.foreach(_.prev = prev)
    prev.foreach(_.next = next)
  }

  var next: Option[DoublyLinkedListNode] = None
  var prev: Option[DoublyLinkedListNode] = None

  def toList: List[String] = value :: (next match {
    case None => Nil
    case Some(n) => n.toList
  })

  def add(s: String): DoublyLinkedListNode = next match {
    case Some(n) => n.add(s)
    case None =>
      val newNode = DoublyLinkedListNode(s)
      newNode.prev = Some(this)
      next = Some(newNode)
      newNode
  }

  def find(s: String): Option[DoublyLinkedListNode] =
    if (value == s) {
      Some(this)
    } else {
      next match {
        case Some(n) => n.find(s)
        case None => None
      }
    }

  override def toString = s"Node($value, $next)"
}
