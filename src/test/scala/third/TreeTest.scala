package third

import org.scalatest.FunSpec

class TreeTest extends FunSpec {

  describe("3.25 size") {
    it("root") {
      assert(Tree.size(Leaf("a")) == 1)
    }

    it("balanced tree") {
      assert(Tree.size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) == 7)
    }
  }

  describe("3.26 maximum") {
    it("root") {
      assert(Tree.maximum(Leaf(1)) == 1)
    }

    it("balanced tree") {
      assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(5)), Branch(Leaf(3), Leaf(2)))) == 5)
    }
  }

  describe("3.27 depth") {
    it("root") {
      assert(Tree.depth(Leaf("A")) == 1)
    }

    it("balanced tree") {
      assert(Tree.depth(Branch(Branch(Leaf(1), Leaf(5)), Branch(Leaf(3), Leaf(2)))) == 3)
    }

    it("unbalanced tree") {
      assert(Tree.depth(Branch(Leaf(6), Branch(Leaf(3), Leaf(2)))) == 3)
    }
  }

  describe("3.28 map") {

    def parse(s: String): Int = s.toInt

    it("root") {
      assert(Tree.map(Leaf("1"))(parse) == Leaf(1))
    }

    it("balanced tree") {
      assert(Tree.map(Branch(Branch(Leaf("1"), Leaf("5")), Branch(Leaf("3"), Leaf("2"))))(parse)
        == Branch(Branch(Leaf(1), Leaf(5)), Branch(Leaf(3), Leaf(2))))
    }
  }
}
