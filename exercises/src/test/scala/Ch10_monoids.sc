/* Ch10_monoids.sc for scratch work and testing of methods in Monoids.scala
 * Author: williamdemeo@gmail.com
 * Date: 26 March 2016
 */
package fpinscala.monoids
import fpinscala.testing._
import Gen._
import Prop._
import Monoid._

object Ch10_monoids {

  // Ex 10.4 Use the property-based testing framework developed in Part 2 to implement a
  // property for the monoid laws. Use your property to test the monoids we've written.

  //----  intAdditive monoid TESTS --------------------------------------------------------
  run( monoidLaws( intAdditive, Gen.integer ) )   //> + OK, passed 100 tests.

  //----  intMultiplicative monoid TESTS --------------------------------------------------------
  run( monoidLaws( intMultiplicative, Gen.integer ) )
                                                  //> + OK, passed 100 tests.

  // Note: the next two should be exhaustively proved.  Fix this.
  //----  booleanOr monoid TESTS --------------------------------------------------------
  run( monoidLaws( booleanOr, Gen.boolean ) )     //> + OK, passed 100 tests.

  //----  booleanAnd monoid TESTS --------------------------------------------------------
  run( monoidLaws( booleanAnd, Gen.boolean ) )    //> + OK, passed 100 tests.

  //---- optionMonoid TESTS ----------------------------------------
  // ...for Option[Int]
  run( monoidLaws( optionMonoid[Int], genToOpt( Gen.integer ) ) )
                                                  //> + OK, passed 100 tests.
  // ...for Option[Boolean]
  run( monoidLaws( optionMonoid[Boolean], genToOpt( Gen.boolean ) ) )
                                                  //> + OK, passed 100 tests.

  //---- endoMonoid TESTS ----------------------------------------
  // Earlier we left off here with the Monoid tests because we didn't know how
  // to randomly generate endofunctions.  Now we know...
  // run( monoidLaws( endoMonoid[Int], g ))  // need to supply g: Gen[endoMonoid] ...how?

  // Some tests are based on this arbitrarily chosen number:
  val N = 10  // Fix this (i.e., make it less ad hoc)
                                                  //> N  : Int = 10
  //----  stringMonoid TESTS --------------------------------------------------------
  val stringOfLengthAtMostN: Gen[String] =
    Gen.stringGenN( choose(0, N) )                //> stringOfLengthAtMostN  : fpinscala.testing.Gen[String] = Gen(State(<functio
                                                  //| n1>))
  run( monoidLaws( stringMonoid, stringOfLengthAtMostN ) )
                                                  //> + OK, passed 100 tests.

  //---- listMonoid TESTS --------------------------------------------------------
  // ...for lists of booleans:
  val listOfBooleans: Gen[List[Boolean]] =
    boolean listOfGeneratedLength( choose(0, N) ) //> listOfBooleans  : fpinscala.testing.Gen[List[Boolean]] = Gen(State(<functio
                                                  //| n1>))
  run( monoidLaws( listMonoid[Boolean], listOfBooleans ) )
                                                  //> + OK, passed 100 tests.

  // ...for lists of doubles:
  val listOfDoubles: Gen[List[Double]] =
    double listOfGeneratedLength( choose(0, N) )  //> listOfDoubles  : fpinscala.testing.Gen[List[Double]] = Gen(State(<function1
                                                  //| >))
  run( monoidLaws( listMonoid[Double], listOfDoubles ) )
                                                  //> + OK, passed 100 tests.

  // ...for lists of strings:
  val listOfStrings_generator: Gen[List[String]] =
    string(N) listOfGeneratedLength( choose(0, N) )
                                                  //> listOfStrings_generator  : fpinscala.testing.Gen[List[String]] = Gen(State(
                                                  //| <function1>))
  run( monoidLaws( listMonoid[String], listOfStrings_generator ) )
                                                  //> + OK, passed 100 tests.

  /* Ex 10.10b make sure the monoid instance for WC meets the monoid laws
   * To test monoid laws for wcMonoid, we will use our monoidLaws function, as above.
   * To do so, we need wc_generator to pass to the second argument of monoidLaws.
   * This is a pattern: if we instantiate a new Monoid[A], and we want to test the
   * monoid laws using our moindLaws function, we need an ag: Gen[A].  In the present
   * case, we need genWC: Gen[WC].  (This genWC is now implemented in Monoid.scala.)
   */
  run( monoidLaws( wcMonoid, genWC ) )            //> + OK, passed 100 tests.
   
  
}