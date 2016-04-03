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
  double(rng)                                     //> res1: (Double, fpinscala.state.RNG) = (0.007524831686168909,Simple(105902596
                                                  //| 4525))

  /* Ex 6.3 Write functions to generate an (Int, Double) pair, a (Double, Int) pair, */
  //---- intDouble() tests -----------------------------------------------------------------
  // signature: intDouble(rng: RNG): ((Int,Double), RNG)
  intDouble(rng)                                  //> res2: ((Int, Double), fpinscala.state.RNG) = ((16159453,0.5967354848980904),
                                                  //| Simple(197491923327988))

  //---- doubleInt() tests -----------------------------------------------------------------
  // signature: doubleInt(rng: RNG): ((Double,Int), RNG)
  doubleInt(rng)                                  //> res3: ((Double, Int), fpinscala.state.RNG) = ((0.5967354848980904,16159453)
                                                  //| ,Simple(197491923327988))
  
  //---- double3() tests -----------------------------------------------------------------
  // signature: double3(rng: RNG): ((Double,Double,Double), RNG)
  double3(rng)                                    //> res4: ((Double, Double, Double), fpinscala.state.RNG) = ((0.007524831686168
                                                  //| 909,0.5967354848980904,0.15846728393808007),Simple(197491923327988))
  
  //---- ints() tests -----------------------------------------------------------------
  // signature: ints(count: Int)(rng: RNG): (List[Int], RNG)
  ints(5)(rng)                                    //> res5: fpinscala.state.RNG.Rand[List[Int]] = <function1>

  //---- double_with_map() tests ---------------------------------------------------------------
  double_with_map(rng)                            //> res6: (Double, fpinscala.state.RNG) = (0.007524831686168909,Simple(10590259
                                                  //| 64525))
	//---- die rolling ---------------------
	rollDie_first_try(Simple(1))._1           //> res7: Int = 4
	rollDie_first_try(Simple(2))._1           //> res8: Int = 3
	rollDie_first_try(Simple(3))._1           //> res9: Int = 2
	rollDie_first_try(Simple(4))._1           //> res10: Int = 1
	rollDie_first_try(Simple(5))._1           //> res11: Int = 0
	rollDie_first_try(Simple(6))._1           //> res12: Int = 5
	rollDie_first_try(Simple(7))._1           //> res13: Int = 4
	rollDie_first_try(Simple(8))._1           //> res14: Int = 3
	rollDie_first_try(Simple(5))._1           //> res15: Int = 0
	rollDie_first_try(Simple(5))._1           //> res16: Int = 0
	rollDie(Simple(5))._1                     //> res17: Int = 1
	
	
	
	
	
	
	
	
	
}