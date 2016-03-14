package fpinscala.laziness
import Stream._

object Chapter05_scratch {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  // toList() tests
  Stream(1,2,3).take(2).toList                    //> res0: List[Int] = List(1, 2)
  Stream(1,2,3).take(0).toList                    //> res1: List[Int] = List()
  Stream(1,2,3).take(4).toList                    //> res2: List[Int] = List(1, 2, 3)

  // takeIf() tests
  Stream(4,2,3,12).takeIf( _ % 2 == 0).toList     //> res3: List[Int] = List(4, 2, 12)
  
}