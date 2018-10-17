package fintech.homework04
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {
  object TreeSamples {
    val singleton: Tree[Int] = Leaf(5)
    val simple: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val unbalanced: Tree[Int] = Branch(Branch(Leaf(1), Branch(Leaf(4), Leaf(3))), Leaf(2))
    val complex: Tree[Any] = Branch(Branch(Leaf(1), Branch(Leaf("4"), Branch(Leaf(3), Leaf("5")))), Leaf("2"))

    val treeList = List(singleton, simple, unbalanced, complex)
  }

  "fold" should "work correctly" in {
    val results = List("5", "1234", "1432", "14352")
    TreeSamples.treeList.zip(results).foreach { testPair =>
      Tree.fold(testPair._1)(_.toString)(_ + _) should be(testPair._2)
    }
  }

  "tailrecFold" should "work correctly" in {
    val results = List("5", "1234", "1432", "14352")
    TreeSamples.treeList.zip(results).foreach { testPair =>
      Tree.tailrecFold(testPair._1)(_.toString)(_ + _) should be(testPair._2)
    }
  }

  "size" should "work correctly" in {
    val results = List(1, 7, 7, 9)
    TreeSamples.treeList.zip(results).foreach { testPair =>
      Tree.size(testPair._1) should be(testPair._2)
    }
  }

  "dataSize" should "work correctly" in {
    val results = List(1, 4, 4, 5)
    TreeSamples.treeList.zip(results).foreach { testPair =>
      Tree.dataSize(testPair._1) should be(testPair._2)
    }
  }

  "max" should "work correctly" in {
    val results = List(5, 4, 4)
    TreeSamples.treeList.take(3).zip(results).foreach { testPair =>
      Tree.max(testPair._1.asInstanceOf[Tree[Int]]) should be(testPair._2)
    }
  }

  "depth" should "work correctly" in {
    val results = List(0, 2, 3, 4)
    TreeSamples.treeList.zip(results).foreach { testPair =>
      Tree.depth(testPair._1) should be(testPair._2)
    }
  }

  "map" should "work correctly" in {
    val singletonMap: Tree[String] = Leaf("555")
    val simpleMap: Tree[String] = Branch(Branch(Leaf("111"), Leaf("222")), Branch(Leaf("333"), Leaf("444")))
    val unbalancedMap: Tree[String] = Branch(Branch(Leaf("111"), Branch(Leaf("444"), Leaf("333"))), Leaf("222"))
    val complexMap: Tree[String] = Branch(Branch(Leaf("111"), Branch(Leaf("444"), Branch(Leaf("333"), Leaf("555")))), Leaf("222"))
    val resultList = List(singletonMap, simpleMap, unbalancedMap, complexMap)

    TreeSamples.treeList.zip(resultList).foreach { testPair =>
      Tree.map(testPair._1)(_.toString * 3) should be(testPair._2)
    }
  }
}