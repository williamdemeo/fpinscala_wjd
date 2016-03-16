package fpinscala.laziness
import Stream._

object Chapter05_scratch {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  // Let's make sure we're using our stream library
  // (not the standard one):
  Stream(1,2,3)                                   //> res0: fpinscala.laziness.Stream[Int] = Cons(<function0>,<function0>)

  //---- toList() tests ------------------------------------------------------------------
  //   NB: toList forces each element of the stream to be evaluated
  Stream(1,2,3).toList                            //> res1: List[Int] = List(1, 2, 3)


  //---- take() tests --------------------------------------------------------------------
  Stream(1,2,3).take(2).toList                    //> res2: List[Int] = List(1, 2)
  Stream(1,2,3).take(0).toList                    //> res3: List[Int] = List()
  Stream(1,2,3).take(4).toList                    //> res4: List[Int] = List(1, 2, 3)
	// Note: ones is defined in Stream.scala
  ones.take(5).toList                             //> res5: List[Int] = List(1, 1, 1, 1, 1)


  //---- drop() tests --------------------------------------------------------------------
  Stream(1,2,3).drop(2).toList                    //> res6: List[Int] = List(3)
  Stream(1,2,3).drop(0).toList                    //> res7: List[Int] = List(1, 2, 3)
  Stream(1,2,3).drop(4).toList                    //> res8: List[Int] = List()
	// `ones` is equivalent to
	ones.drop(100)                            //> res9: fpinscala.laziness.Stream[Int] = Cons(<function0>,<function0>)
	// of course, applying toList wouldn't terminate, unless we take a few first
	ones.drop(100).take(5).toList             //> res10: List[Int] = List(1, 1, 1, 1, 1)


  //---- takeWhile() tests ----------------------------------------------------------------
  Stream(4,2,3,12).takeWhile( _ % 2 == 0).toList  //> res11: List[Int] = List(4, 2)
  Stream(1,2,3,4,3,2,1).takeWhile( _ < 4).toList  //> res12: List[Int] = List(1, 2, 3)
	// `ones` is equivalent to
	ones.takeWhile(_ == 1)                    //> res13: fpinscala.laziness.Stream[Int] = Cons(<function0>,<function0>)
	ones.takeWhile(_ == 1).take(5).toList     //> res14: List[Int] = List(1, 1, 1, 1, 1)


	//---- takeWhile_with_foldRight() tests -------------------------------------------------
	Stream(4,2,3,12).takeWhile_with_foldRight( _ % 2 == 0).toList
                                                  //> res15: List[Int] = List(4, 2)
  Stream(1,2,3,4,3,2,1).takeWhile_with_foldRight( _ < 4).toList
                                                  //> res16: List[Int] = List(1, 2, 3)
	// `ones` is equivalent to
	ones.takeWhile_with_foldRight(_ == 1)     //> res17: fpinscala.laziness.Stream[Int] = Cons(<function0>,<function0>)
	ones.takeWhile_with_foldRight(_ == 1).take(5).toList
                                                  //> res18: List[Int] = List(1, 1, 1, 1, 1)


  //---- takeIf() tests ------------------------------------------------------------------
  Stream(4,2,3,12).takeIf( _ % 2 == 0).toList     //> res19: List[Int] = List(4, 2, 12)
  Stream(1,2,3,4,3,2,1).takeIf( _ < 4).toList     //> res20: List[Int] = List(1, 2, 3, 3, 2, 1)
	// `ones` is equivalent to
	ones.takeIf(_ == 1)                       //> res21: fpinscala.laziness.Stream[Int] = Cons(<function0>,<function0>)

  
  //---- forAll() tests ------------------------------------------------------------------
  Stream(1,2,3).forAll( _ < 4)                    //> res22: Boolean = true
  Stream(1,2,3).forAll( _ < 3)                    //> res23: Boolean = false
  Stream(1,2,3).forAll( _ > 0)                    //> res24: Boolean = true
  Stream(1,2,3).forAll( _ > 4)                    //> res25: Boolean = false
	ones.forAll(_ != 1)                       //> res26: Boolean = false
	//	ones.forAll(_ == 1)  // results in  java.lang.StackOverflowError

	
  //---- exists() tests ------------------------------------------------------------------
  Stream(1,2,3).exists( _ < 4)                    //> res27: Boolean = true
  Stream(1,2,3).exists( _ < 3)                    //> res28: Boolean = true
  Stream(1,2,3).exists( _ > 0)                    //> res29: Boolean = true
  Stream(1,2,3).exists( _ > 4)                    //> res30: Boolean = false
	ones.exists(_ != 0)                       //> res31: Boolean = true
	//	ones.exists(_ == 0)  // results in  StackOverflowError
  
  
  //---- map and filter tests -------------------------------------------------------------
  Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList
                                                  //> res32: List[Int] = List(12, 14)
 
  //---- startsWith() tests -------------------------------------------------------------------
  Stream(1,2,3).startsWith_first_try(Stream(1,2)) //> res33: Boolean = true
  Stream(1,2,3).startsWith_first_try(Stream(1,2,3,4))
                                                  //> res34: Boolean = true
  Stream(1,2,3).startsWith_second_try(Stream(1,2))//> res35: Boolean = true
  Stream(1,2,3).startsWith_second_try(Stream(1,2,3,4))
                                                  //> res36: Boolean = false
 
  //---- hasSubsequence() tests ---------------------------------------------------------
  Stream(1,2,3).hasSubsequence(Stream(2,3))       //> res37: Boolean = true
  Stream(1,2,3).hasSubsequence(Stream(1,3))       //> res38: Boolean = false
  // The last example results in false which shows that
  // `hasSubsequence` is not a good name for this function.

  //---- tails() tests ------------------------------------------------------------------------
  Stream(1,2,3).tails.map(_.toList).toList        //> res39: List[List[Int]] = List(List(1, 2, 3), List(2, 3), List(3), List())
  
  //---- scanRight() tests ----------------------------------------------------------------
  Stream(1,2,3).scanRight(0)(_ + _).toList        //> res40: List[Int] = List(6, 5, 3, 0)
  
 	//---- miscellaneous examples -----------------------------------------------------------
 	val zeros = ones.map(_ - 1)               //> zeros  : fpinscala.laziness.Stream[Int] = Cons(<function0>,<function0>)
 	// three zeros followed by 7 ones:
	zeros.take(3).append(ones).take(10).toList//> res41: List[Int] = List(0, 0, 0, 1, 1, 1, 1, 1, 1, 1)

	fibs.take(7).toList                       //> res42: List[Int] = List(0, 1, 1, 2, 3, 5, 8)
	 	
  fibs_with_unfold.take(7).toList                 //> res43: List[Int] = List(0, 1, 1, 2, 3, 5, 8)
 
  from_with_unfold(7).take(3).toList              //> res44: List[Int] = List(7, 8, 9)
 
 	constant_with_unfold(2).take(5).toList    //> res45: List[Int] = List(2, 2, 2, 2, 2)
 
 
 	ones_with_unfold.take(5).toList           //> res46: List[Int] = List(1, 1, 1, 1, 1)
 
 
 
 
 
 
  
}