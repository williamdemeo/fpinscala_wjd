package fpinscala.datastructures
import List._

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.scalatest.exceptions.TestFailedException

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._


@RunWith(classOf[JUnitRunner])
class ListSuite extends FunSuite with Checkers {
  test("tail length") {
    check(ListTests)
  }
}

object ListTests extends Properties("List") {

  // generate random lists (to be used when testing the properties below)
  lazy val genList: Gen[List[Int]] = for {
    n <- arbitrary[Int]               // Generate random n:Int to be inserted into list l
    l <- oneOf(const(Nil), genList)   // Generate random list l:List[Int] to which n will be added
  } yield Cons(n,l)                   // return the list l updated with element n

  def choose(lo: Int, hi: Int): Gen[Int] =
    for (x <- arbitrary[Int]) yield lo + x % (hi-lo)
    
  lazy val genListInt: Gen[(List[Int], Int)] = for {
    l <- genList
    n <- choose(0, length(l))
    if (length(l)>0 && n > 0)
  } yield (l, n)

  property("random tail length") = forAll(genList){ (l: List[Int]) =>
    length(l) == length(tail(l))+1
  }

  property("simple drop length") = {
    val mylist = List(1,2,3)
    length(mylist) - 1 == length(drop(mylist,1))
  }
  property("random drop length") = forAll(genListInt) { pair =>
    if (length(pair._1)> pair._2) {
      println("length(l) = " + length(pair._1) + "    n = " + pair._2)
      length(pair._1) - pair._2 == length(drop(pair._1,pair._2))
    }
    else true
  }
  
  
  
}
  
