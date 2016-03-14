package fpinscala.errorhandling
import Option._

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
class OptionSuite extends FunSuite with Checkers {
  test("tail length") {
    check(OptionTests)
  }
}

// I plan to implement tests of Option similar to the tests
// I developed for Lists, but I think I'll hold off on this
// until after I've reached Chapter 8 which coveres testing. 
object OptionTests extends Properties("Option") {

  // generate random lists (to be used when testing the properties below)
  lazy val genOption: Gen[Option[Int]] = ???

  def choose(lo: Int, hi: Int): Gen[Int] = ???

  property("test that Option satisfies ...") = ???
  
  
}
  
