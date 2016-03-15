package fpinscala.laziness
import Stream._

object Chapter05_scratch {
  println("Welcome to the Scala worksheet")
  
  
  // Let's make sure we're using our stream library
  // (not the standard one):
  Stream(1,2,3)

	// some streams we'll use for testing:
	val abc = Stream(1,2,3)
	val ones: Stream[Int] = Stream.cons(1, ones)
  ones.take(5).toList
  
  // toList() tests
  //   NB: toList forces each element of the stream to be evaluated
  Stream(1,2,3).toList

  // take() tests
  Stream(1,2,3).take(2).toList
  Stream(1,2,3).take(0).toList
  Stream(1,2,3).take(4).toList

  // drop() tests
  Stream(1,2,3).drop(2).toList
  Stream(1,2,3).drop(0).toList
  Stream(1,2,3).drop(4).toList

  // takeWhile() tests
  Stream(4,2,3,12).takeWhile( _ % 2 == 0).toList
  Stream(1,2,3,4,3,2,1).takeWhile( _ < 4).toList

  // takeIf() tests
  Stream(4,2,3,12).takeIf( _ % 2 == 0).toList
  Stream(1,2,3,4,3,2,1).takeIf( _ < 4).toList
  
  // forAll() tests
  Stream(1,2,3).forAll( _ < 4)
  Stream(1,2,3).forAll( _ < 3)
  Stream(1,2,3).forAll( _ > 0)
  Stream(1,2,3).forAll( _ > 4)

  // exists() tests
  Stream(1,2,3).exists( _ < 4)
  Stream(1,2,3).exists( _ < 3)
  Stream(1,2,3).exists( _ > 0)
  Stream(1,2,3).exists( _ > 4)
  
}