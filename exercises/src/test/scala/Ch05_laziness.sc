package fpinscala.laziness
import Stream._

object Chapter05_scratch {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  // Let's make sure we're using our stream library
  // (not the standard one):
  Stream(1,2,3)                                   //> res0: fpinscala.laziness.Stream[Int] = Cons(<function0>,<function0>)

  // toList() tests
  //   NB: toList forces each element of the stream to be evaluated
  Stream(1,2,3).toList                            //> res1: List[Int] = List(1, 2, 3)

  // take() tests
  Stream(1,2,3).take(2).toList                    //> res2: List[Int] = List(1, 2)
  Stream(1,2,3).take(0).toList                    //> res3: List[Int] = List()
  Stream(1,2,3).take(4).toList                    //> res4: List[Int] = List(1, 2, 3)

  // drop() tests
  Stream(1,2,3).drop(2).toList                    //> res5: List[Int] = List(3)
  Stream(1,2,3).drop(0).toList                    //> res6: List[Int] = List(1, 2, 3)
  Stream(1,2,3).drop(4).toList                    //> res7: List[Int] = List()

  // takeWhile() tests
  Stream(4,2,3,12).takeWhile( _ % 2 == 0).toList  //> res8: List[Int] = List(4, 2)
  Stream(1,2,3,4,3,2,1).takeWhile( _ < 4).toList  //> res9: List[Int] = List(1, 2, 3)

  // takeIf() tests
  Stream(4,2,3,12).takeIf( _ % 2 == 0).toList     //> res10: List[Int] = List(4, 2, 12)
  Stream(1,2,3,4,3,2,1).takeIf( _ < 4).toList     //> res11: List[Int] = List(1, 2, 3, 3, 2, 1)
  
  // forAll() tests
  Stream(1,2,3).forAll( _ < 4)                    //> res12: Boolean = true
  Stream(1,2,3).forAll( _ < 3)                    //> res13: Boolean = false
  Stream(1,2,3).forAll( _ > 0)                    //> res14: Boolean = true
  Stream(1,2,3).forAll( _ > 4)                    //> res15: Boolean = false

  // exists() tests
  Stream(1,2,3).exists( _ < 4)                    //> res16: Boolean = true
  Stream(1,2,3).exists( _ < 3)                    //> res17: Boolean = true
  Stream(1,2,3).exists( _ > 0)                    //> res18: Boolean = true
  Stream(1,2,3).exists( _ > 4)                    //> res19: Boolean = false
  
}