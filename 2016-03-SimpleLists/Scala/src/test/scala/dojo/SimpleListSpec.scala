package dojo

import org.scalatest.{DiagrammedAssertions, FunSpec}

class SimpleListSpec extends FunSpec with DiagrammedAssertions {
  def aSimpleList(newList: () => SimpleList) = {
    it("cannot find value in empty list") {
      val l = newList()
      assert(l.find("fred") == None)
    }

    it("can find value after add to list") {
      val l = newList()
      l.add("fred")
      assert(l.find("fred").get.value == "fred")
    }

    it("can find multiple values") {
      val l = newList()
      l.add("fred")
      l.add("wilma")
      assert(l.find("fred").get.value == "fred")
      assert(l.find("wilma").get.value == "wilma")
    }

    it("has toList") {
      val l = newList()
      l.add("fred")
      l.add("wilma")
      l.add("betty")
      l.add("barney")

      assert(l.toList sameElements List("fred", "wilma", "betty", "barney"))
    }

    it("deletes second element") {
      val l = newList()
      l.add("wilma")
      l.add("fred")
      l.delete(l.find("fred").get)

      assert(l.find("fred") == None)
    }
    it("deletes first element") {
      val l = newList()
      l.add("fred")
      l.delete(l.find("fred").get)

      assert(l.find("fred") == None)
    }
    it("deletes first element and toLists") {
      val l = newList()
      l.add("fred")
      l.add("wilma")
      l.delete(l.find("fred").get)

      assert(l.toList == List("wilma"))
    }

    it("deletes second element and toLists") {
      val l = newList()
      l.add("fred")
      l.add("wilma")
      l.delete(l.find("wilma").get)

      assert(l.toList == List("fred"))
    }

    it("deletes middle element and toLists") {
      val l = newList()
      l.add("fred")
      l.add("wilma")
      l.add("barney")
      l.add("betty")
      l.delete(l.find("wilma").get)

      assert(l.toList == List("fred", "barney", "betty"))
    }
  }

  describe("Singly linked list") {
    it should behave like aSimpleList(() => new SinglyLinkedList)
  }

  describe("Doubly linked list") {
    it should behave like aSimpleList(() => new DoublyLinkedList)
  }
}
