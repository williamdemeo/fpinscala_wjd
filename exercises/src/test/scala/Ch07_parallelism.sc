/* Ch07_parallelism.sc for scratch work and testing of methods in Par.scala
 * Author: williamdemeo@gmail.com
 * Date: 18 March 2016
 */
package fpinscala.parallelism
import Par._
import java.util.concurrent._

object Ch07_parallelism {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

	map(unit(1))(_ + 1) == unit(2)            //> res0: Boolean = false
	
	val a = lazyUnit(42 + 1)                  //> a  : fpinscala.parallelism.Par.Par[Int] = <function1>
	// val S = Executors.newFixedThreadPool(1)
	// println(Par.equal(S)(a, fork(a)))
}