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
  val stringsOfLengthAtMostN_generator: Gen[String] =
    Gen.stringGenN( choose(0, N) )                //> stringsOfLengthAtMostN_generator  : fpinscala.testing.Gen[String] = Gen(Sta
                                                  //| te(<function1>))
  run( monoidLaws( stringMonoid, stringsOfLengthAtMostN_generator ) )
                                                  //> + OK, passed 100 tests.

  //---- listMonoid TESTS --------------------------------------------------------
  // ...for lists of booleans:
  val listOfBooleans_generator: Gen[List[Boolean]] =
    boolean listOfGeneratedLength( choose(0, N) ) //> listOfBooleans_generator  : fpinscala.testing.Gen[List[Boolean]] = Gen(Stat
                                                  //| e(<function1>))
  run( monoidLaws( listMonoid[Boolean], listOfBooleans_generator ) )
                                                  //> + OK, passed 100 tests.

  // ...for lists of doubles:
  val listOfDoubles_generator: Gen[List[Double]] =
    double listOfGeneratedLength( choose(0, N) )  //> listOfDoubles_generator  : fpinscala.testing.Gen[List[Double]] = Gen(State(
                                                  //| <function1>))
  run( monoidLaws( listMonoid[Double], listOfDoubles_generator ) )
                                                  //> + OK, passed 100 tests.

  // ...for lists of strings:
  val listOfStrings_generator: Gen[List[String]] =
    string(N) listOfGeneratedLength( choose(0, N) )
                                                  //> listOfStrings_generator  : fpinscala.testing.Gen[List[String]] = Gen(State(
                                                  //| <function1>))
  run( monoidLaws( listMonoid[String], listOfStrings_generator ) )



  // Ex 10.10b make sure the monoid instance for WC meets the monoid laws
  
}