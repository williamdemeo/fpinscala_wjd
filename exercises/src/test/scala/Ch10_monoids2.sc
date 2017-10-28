//package fpinscala.monoids
import fpinscala.testing._
import Gen._
import Prop._
import fpinscala.monoids.Monoid._

object Ch10_monoids2 {
  println("Ch10_monoids_2")

  // Ex 10.4 Use the property-based testing framework developed in Part 2 to implement a
  // property for the monoid laws. Use your property to test the monoids we've written.

  println("Test: intAdditive monoid")
  run( monoidLaws( intAdditive, Gen.integer ) )

  println("Test: intMultiplicative monoid")
  run( monoidLaws( intMultiplicative, Gen.integer ) )

  // Note: the next two should be exhaustively proved.  Fix this.
  println("Test: booleanOr monoid")
  run( monoidLaws( booleanOr, Gen.boolean ) )

  println("Test: booleanAnd monoid")
  run( monoidLaws( booleanAnd, Gen.boolean ) )

  println("Test: optionMonoid ...for Option[Int]")
  run( monoidLaws( optionMonoid[Int], genToOpt( Gen.integer ) ) )
  println("Test: optionMonoid ...for Option[Boolean]")
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