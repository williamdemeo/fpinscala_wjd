/* Ch06_state.sc for scratch work and testing of methods in State.scala
 * Author: williamdemeo@gmail.com
 * Date: 16 March 2016
 */
package fpinscala.state
import RNG._

object Ch06_state {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val rng = Simple(42)                            //> rng  : fpinscala.state.RNG.Simple = Simple(42)

  //---- nonNegativeInt() tests ---------------------------------------------------------------
  // signature: nonNegativeInt(rng: RNG): (Int, RNG)
  nonNegativeInt(rng)                             //> res0: (Int, fpinscala.state.RNG) = (16159453,Simple(1059025964525))

  //---- double() tests ---------------------------------------------------------------
  // signature: double(rng: RNG): (Double, RNG)
  double(rng)                                     //> res1: (Double, fpinscala.state.RNG) = (0.007524831689672932,Simple(105902596
                                                  //| 4525))

  /* Ex 6.3 Write functions to generate an (Int, Double) pair, a (Double, Int) pair, */
  //---- intDouble() tests -----------------------------------------------------------------
  // signature: intDouble(rng: RNG): ((Int,Double), RNG)
  intDouble(rng)                                  //> res2: ((Int, Double), fpinscala.state.RNG) = ((16159453,0.596735485175967),S
                                                  //| imple(197491923327988))

  //---- doubleInt() tests -----------------------------------------------------------------
  // signature: doubleInt(rng: RNG): ((Double,Int), RNG)
  doubleInt(rng)                                  //> res3: ((Double, Int), fpinscala.state.RNG) = ((0.596735485175967,16159453),
                                                  //| Simple(197491923327988))
  
  //---- double3() tests -----------------------------------------------------------------
  // signature: double3(rng: RNG): ((Double,Double,Double), RNG)
  double3(rng)                                    //> res4: ((Double, Double, Double), fpinscala.state.RNG) = ((0.007524831689672
                                                  //| 932,0.596735485175967,0.15846728401187216),Simple(197491923327988))
  
  //---- ints() tests -----------------------------------------------------------------
  // signature: ints(count: Int)(rng: RNG): (List[Int], RNG)
  ints(5)(rng)                                    //> res5: (List[Int], fpinscala.state.RNG) = (List(1770001318, -2015756020, -34
                                                  //| 0305902, -1281479697, 16159453),Simple(42))

  //---- double_with_map() tests ---------------------------------------------------------------
  double_with_map(rng)                            //> res6: (Double, fpinscala.state.RNG) = (0.007524831689672932,Simple(10590259
                                                  //| 64525))
}