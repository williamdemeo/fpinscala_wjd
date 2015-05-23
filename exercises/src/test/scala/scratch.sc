package fpinscala.datastructures
import List._

object scratch {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  
  // Test for Exercise 3.8:
  //     What happens when you pass Nil and Cons to foldRight?
  //     What does this say about the relationship between foldRight and the constructors of List?
  foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))//> res0: fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
  //     Answer: You just get the original list back because the constructors of List are
  //             precisely the cases matched in the foldRight definition.  In other words, the cases are
  //             handled as follows (resulting in the identity map):
  //                 case Nil => Nil
  //                 case Cons(x,xs) => Cons(x,xs)

  // Test for Exercise 3.9:
	def length_fr[A](l: List[A]): Int =
		foldRight(l, 0)((x,y) => 1+y)     //> length_fr: [A](l: fpinscala.datastructures.List[A])Int
		
	length_fr(List(1,2,1,-1))                 //> res1: Int = 4
	length_fr(List())                         //> res2: Int = 0
	length_fr(List('a','b','c'))              //> res3: Int = 3
	
	sum_fl(List(1,2,3,-1))                    //> res4: Int = 5
	product_fl(List(1,2,3,0.5))               //> res5: Double = 3.0
	
	// Practice with foldLeft and foldRight:
	
	// foldLeft(List(1,2,3), 0)(_+_) is (((0+1)+2)+3) = 6
	foldLeft(List(1,2,3), 0)(_+_)             //> res6: Int = 6
	// foldRight(List(1,2,3), 0)(_+_) is (1+(2+(3+0))) = 6
	foldRight(List(1,2,3), 0)(_+_)            //> res7: Int = 6
	// No difference because + is both commutative and associative.
	// However, - is associative but not commutative. So,
	// foldLeft(List(1,2,3), 0)(_-_) is (((0-1)-2)-3) = -6
	foldLeft(List(1,2,3), 0)(_-_)             //> res8: Int = -6
	// foldRight(List(1,2,3), 0)(_-_) is (1-(2-(3-0))) = (1-(2-3)) = (1-(-1)) = 2
	foldRight(List(1,2,3), 0)(_-_)            //> res9: Int = 2

	// foldLeft(List(1,2,3), 1)(_*_) is (((1*1)*2)*3) = 6
	foldLeft(List(1,2,3), 1)(_*_)             //> res10: Int = 6
	// foldRight(List(1,2,3), 1)(_*_) is (1*(2*(3*1))) = 6
	foldRight(List(1,2,3), 1)(_*_)            //> res11: Int = 6
	// No difference because * is both commutative and associative.
	// However, / is neither associative nor commutative. So,
	// foldLeft(List(1,2,3), 1)(_/_) is (((1/1)/2)/3) = 1/6
	foldLeft(List(1.0,2.0,3.0), 1.0)(_/_)     //> res12: Double = 0.16666666666666666
	// foldRight(List(1,2,3), 1)(_/_) is (1/(2/(3/1))) = 1.5
	foldRight(List(1.0,2.0,3.0), 1.0)(_/_)    //> res13: Double = 1.5
	
	// Test for Exercise 3.12: test reverse of a list
	reverse_first_try(List(1,2,3))            //> res14: fpinscala.datastructures.List[Int] = Cons(3,Cons(2,Cons(1,Nil)))
	reverse(List(1,2,3))                      //> res15: fpinscala.datastructures.List[Int] = Cons(3,Cons(2,Cons(1,Nil)))
	
	// Test for Exercise 3.14: append in terms of foldRight
	append(List(1,2),List(3,4))               //> res16: fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Nil
                                                  //| ))))
                                             
  // Test for Excercise 3.15: flatten a list of lists into a single list
	flatten(List(List(1,2),List(3,4),List(5,6)))
                                                  //> res17: fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Con
                                                  //| s(5,Cons(6,Nil))))))
	// TODO: check runtime of flatten (should be linear in the total length of all lists)
  // Test for Exercise 3.18: map (tail recursive version defined with foldRight)
  map(List(-1,2,-3))(x => x*x*x)                  //> res18: fpinscala.datastructures.List[Int] = Cons(-1,Cons(8,Cons(-27,Nil)))
  
	// Test for Exercise 3.19: filter (tail recursive version defined with foldRight)
	// use filter to remove all odd numbers from a List[Int]
	filter(List(1,2,3,4,5))( _ % 2 == 0)      //> res19: fpinscala.datastructures.List[Int] = Cons(2,Cons(4,Nil))

  // Test for Exercise 3.20: flatMap (tail recursive version defined with foldRight)
  // The following example should return List(1,1,2,2,3,3)
  flatMap(List(1,2,3))(x => List(x,x))            //> res20: fpinscala.datastructures.List[Int] = Cons(1,Cons(1,Cons(2,Cons(2,Con
                                                  //| s(3,Cons(3,Nil))))))

	// Test for Exercise 3.21: filter defined with flatMap
	filter_with_flatMap(List(1,2,3,4,5))( _ % 2 == 0)
                                                  //> res21: fpinscala.datastructures.List[Int] = Cons(2,Cons(4,Nil))
	// Test for Exercise 3.23: zipWith_first_try
	zipWith_first_try(List(1,2,3), List(4,5,6))(_+_)
                                                  //> res22: fpinscala.datastructures.List[Int] = Cons(5,Cons(7,Cons(9,Nil)))
	
	// Test for Exercise 3.23: zipWith_first_try
	zipWith(List(1,2,3), List(4,5,6))(_+_)    //> res23: fpinscala.datastructures.List[Int] = Cons(5,Cons(7,Cons(9,Nil)))
	
}