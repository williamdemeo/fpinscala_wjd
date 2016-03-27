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

	// Ex 10.4 Use the property-based testing framework we developed in part 2 to implement a
	// property for the monoid laws. Use your property to test the monoids we've written.

	// stringMonoid tests
  // run(monoidLaws(stringMonoid,g))  // need to supply g: Gen[String]

  // listMonoid tests
  // run(monoidLaws(listMonoid,g))  // need to supply g: Gen[List[A]] for some A

  // intAdditive tests
  // run(monoidLaws(intAdditive,g))  // need to supply g: Gen[Int]

  // intMultiplicative tests
  // run(monoidLaws(intMultiplicative,g))  // need to supply g: Gen[Int]

  // booleanOr tests
  // run(monoidLaws(booleanOr,g))  // need to supply g: Gen[Boolean]

  // booleanAnd tests
  // run(monoidLaws(booleanAnd,g))  // need to supply g: Gen[Boolean]

  // optionMonoid tests
  // run(monoidLaws(optionMonoid,g))  // need to supply g: Gen[Option[A]] for some A

	// endoMonoid tests
  // run(monoidLaws(endoMonoid,g))  // need to supply g: Gen[endoMonoid] ...how?

  
}