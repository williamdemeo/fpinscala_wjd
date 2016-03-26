/* Ch08_testing.sc for scratch work and testing of methods in Gen.scala
 * Author: williamdemeo@gmail.com
 * Date: 25 March 2016
 */
package fpinscala.testing
import fpinscala.state._
import fpinscala.parallelism._
import Gen._
import Prop._
 
object Ch08_testing {

  //--- A first batch of tests (cf. p. 137) ---
  // For this first run, we break it down into components for clarity.
  // Of course, this isn't necessary, and we'll reformulate the test(s) below more compactly.

  // Component 1. the primitive generator
  // smallInt: Gen[Int]
  val smallInt = Gen.choose(-10,10)               //> smallInt  : fpinscala.testing.Gen[Int] = Gen(State(<function1>))
  // generates random ints in [-10,10)

  // Component 2. the list generator
  // intListGen: SGen[List[Int]]
  val intListGen = nonEmptyListOf(smallInt)       //> intListGen  : fpinscala.testing.SGen[List[Int]] = SGen(<function1>)
  // This one generates lists of int in [-10,10) by repeatedly calling on the generator smallInt.

  // Component 3. a predicate
  // P1: List[Int] => Boolean
  def P1(ns: List[Int]): Boolean = {
    val maxVal = ns.max
    !ns.exists(_ > maxVal)
  }                                               //> P1: (ns: List[Int])Boolean
  // We expect P1 to hold for all generated lists of ints.
  
  // Component 4. a property tester.
  // propertyTester: Prop
  val propertyTester = forAll(intListGen)(P1)     //> propertyTester  : fpinscala.testing.Prop = Prop(<function3>)
  // When we invoke this Prop's run method, it will checks that all examples generated
  // by intListGen satisfy the predicate P1.
    
  run(propertyTester,
      50,            // maximum size
      50,            // number of test cases
      RNG.Simple(System.currentTimeMillis))       //> + OK, passed 50 tests.

  // Components 1 thru 4 can be rolled up into a very compact form:
  val propertyTest_compact = forAll(nonEmptyListOf(Gen.choose(-10,10))) { ns =>
    val maxVal = ns.max
    !ns.exists(_ > maxVal)
    }                                             //> propertyTest_compact  : fpinscala.testing.Prop = Prop(<function3>)
   
   run(propertyTest_compact)                      //> + OK, passed 100 tests.
   // Calling run with just one argument invokes the helper method with
   // default values 100, 100, RNG.Simple(System.currentTimeMillis))

  // Ex 8.14 Write a property to verify the behavior of List.sorted.
  def testIntListSort(ls: List[Int]): Boolean = {
    val sortedList = ls.sorted
    isSorted(sortedList)
  }                                               //> testIntListSort: (ls: List[Int])Boolean
  def isSorted(ls: List[Int]): Boolean = ls match {
    case List() => true
    case List(x) => true
    case h::t => (h <= t.min) && isSorted(t)
  }                                               //> isSorted: (ls: List[Int])Boolean
  val sortPropertyTester = forAll(listOf(smallInt)) { ns => testIntListSort(ns) }
                                                  //> sortPropertyTester  : fpinscala.testing.Prop = Prop(<function3>)
  run(sortPropertyTester)                         //> + OK, passed 100 tests.
  
  //---------- Par tests ---------------------
  // The old way:
  val parProp_first_try = check {
    val p1 = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p1(ES).get == p2(ES).get
  }                                               //> parProp_first_try  : fpinscala.testing.Prop = Prop(<function3>)
  run(parProp_first_try)                          //> + OK, proved property.

  // The new way:
  val parProp_second_try = provePar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }                                               //> parProp_second_try  : fpinscala.testing.Prop = Prop(<function3>)
  run(parProp_second_try)                         //> + OK, proved property.

}