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

	concatenate(List(1,2,3), intAdditive )  // this works but...
                                                  //> res0: Int = 6
	// concatenate(List(1,2,3))   // ...this doesn't work

	// With implicits we needn't pass in intAdditive, as long as there's only one
	// monoid based on Int type that uses the implicit declaration.

	reduce(List(1,2,3))                       //> res1: Int = 6

	// Without implicits `reduce` would require an m: Monoid[Int] argument,
	// m.op and m.zero are needed to perform the fold.  However, since we made the
	// IntAdditive monoid implicit, `reduce` uses that monoid by default, so we don't
	// even have to mention the monoid! Let's try this with a conjunctive clause.
	
	// If we use our monoid over booleans, `booleanAnd`, to define an implicit
	// monoid type, `implicit val BooleanAnd = booleanAnd`, then we can do
	
	val valid_conjuncts: List[Boolean] = List((1<3), (3==3), (2*8 > 10))
                                                  //> valid_conjuncts  : List[Boolean] = List(true, true, true)
	val invalid_conjuncts: List[Boolean] = List( (1<3), (3==3), (2*8 < 10) )
                                                  //> invalid_conjuncts  : List[Boolean] = List(true, true, false)
	reduce(valid_conjuncts)                   //> res2: Boolean = true
	reduce(invalid_conjuncts)                 //> res3: Boolean = false
}
	

object Ch10_monoids_2 {

  // Ex 10.4 Use the property-based testing framework developed in Part 2 to implement a
  // property for the monoid laws. Use your property to test the monoids we've written.

  //----  intAdditive monoid TESTS --------------------------------------------------------
  run( monoidLaws( intAdditive, Gen.integer ) )

  //----  intMultiplicative monoid TESTS --------------------------------------------------------
  run( monoidLaws( intMultiplicative, Gen.integer ) )

  // Note: the next two should be exhaustively proved.  Fix this.
  //----  booleanOr monoid TESTS --------------------------------------------------------
  run( monoidLaws( booleanOr, Gen.boolean ) )

  //----  booleanAnd monoid TESTS --------------------------------------------------------
  run( monoidLaws( booleanAnd, Gen.boolean ) )

  //---- optionMonoid TESTS ----------------------------------------
  // ...for Option[Int]
  run( monoidLaws( optionMonoid[Int], genToOpt( Gen.integer ) ) )
  // ...for Option[Boolean]
  run( monoidLaws( optionMonoid[Boolean], genToOpt( Gen.boolean ) ) )

  //---- endoMonoid TESTS ----------------------------------------
  // Earlier we left off here with the Monoid tests because we didn't know how
  // to randomly generate endofunctions.  Now we know...
  // run( monoidLaws( endoMonoid[Int], g ))  // need to supply g: Gen[endoMonoid] ...how?

  // Some tests are based on this arbitrarily chosen number:
  val N = 10  // Fix this (i.e., make it less ad hoc)
  //----  stringMonoid TESTS --------------------------------------------------------
  val stringOfLengthAtMostN: Gen[String] =
    Gen.stringGenN( choose(0, N) )
  run( monoidLaws( stringMonoid, stringOfLengthAtMostN ) )

  //---- listMonoid TESTS --------------------------------------------------------
  // ...for lists of booleans:
  val listOfBooleans: Gen[List[Boolean]] =
    boolean listOfGeneratedLength( choose(0, N) )
  run( monoidLaws( listMonoid[Boolean], listOfBooleans ) )

  // ...for lists of doubles:
  val listOfDoubles: Gen[List[Double]] =
    double listOfGeneratedLength( choose(0, N) )
  run( monoidLaws( listMonoid[Double], listOfDoubles ) )

  // ...for lists of strings:
  val listOfStrings_generator: Gen[List[String]] =
    string(N) listOfGeneratedLength( choose(0, N) )
  run( monoidLaws( listMonoid[String], listOfStrings_generator ) )

  /* Ex 10.10b make sure the monoid instance for WC meets the monoid laws
   * To test monoid laws for wcMonoid, we will use our monoidLaws function, as above.
   * To do so, we need wc_generator to pass to the second argument of monoidLaws.
   * This is a pattern: if we instantiate a new Monoid[A], and we want to test the
   * monoid laws using our moindLaws function, we need an ag: Gen[A].  In the present
   * case, we need genWC: Gen[WC].  (This genWC is now implemented in Monoid.scala.)
   */
  //run( monoidLaws( wcMonoid, genWC ) )
  run( monoidLaws( wcMonoid, genWC_weighted ) )
   
  
}