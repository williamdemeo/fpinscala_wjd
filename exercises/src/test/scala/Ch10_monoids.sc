/* Ch10_monoids.sc for scratch work and testing of methods in Monoids.scala
 * Author: williamdemeo@gmail.com
 * Date: 26 March 2016
 */
//package fpinscala.monoids
import fpinscala.monoids.Monoid._

object Ch10_monoids_1 {
  println("Ch10_monoids_1")
  concatenate(List(1,2,3), intAdditive )  // this works but...
                                                  //> res0: Int = 6
	// concatenate(List(1,2,3))   // ...this doesn't work

	// With implicits we needn't pass in intAdditive, as long as there's only one
	// monoid based on Int type that uses the implicit declaration.


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

