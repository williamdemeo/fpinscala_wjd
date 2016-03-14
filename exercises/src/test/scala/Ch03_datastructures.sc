package fpinscala.datastructures

object my_trees {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import Tree._
  
  // Binary Tree tests
  val my_tree = Branch[Int](Leaf(1), Leaf(2))     //> my_tree  : fpinscala.datastructures.Branch[Int] = Branch(Leaf(1),Leaf(2))
  val my_tree2 = Branch(my_tree, Leaf(3))         //> my_tree2  : fpinscala.datastructures.Branch[Int] = Branch(Branch(Leaf(1),Lea
                                                  //| f(2)),Leaf(3))
  val my_tree3 = Branch(Leaf(0), my_tree2)        //> my_tree3  : fpinscala.datastructures.Branch[Int] = Branch(Leaf(0),Branch(Bra
                                                  //| nch(Leaf(1),Leaf(2)),Leaf(3)))

  depth(my_tree)                                  //> res0: Int = 2
  depth_alternative(my_tree)                      //> res1: Int = 2
  depth(my_tree2)                                 //> res2: Int = 3
  depth_alternative(my_tree2)                     //> res3: Int = 3
  maximum(my_tree)                                //> res4: Int = 2
  maximum(my_tree2)                               //> res5: Int = 3
  size(my_tree2)                                  //> res6: Int = 5
  size_with_foldLeft(my_tree2)                    //> res7: Int = 5
  size(my_tree3)                                  //> res8: Int = 7
  size_with_foldLeft(my_tree3)                    //> res9: Int = 7
}

object my_lists {
  import List._
  
  // Test for Exercise 3.8:
  //     What happens when you pass Nil and Cons to foldRight?
  //     What does this say about the relationship between foldRight and the constructors of List?
  foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  //     Answer: You just get the original list back because the constructors of List are
  //             precisely the cases matched in the foldRight definition.  In other words, the cases are
  //             handled as follows (resulting in the identity map):
  //                 case Nil => Nil
  //                 case Cons(x,xs) => Cons(x,xs)

  // Test for Exercise 3.9
	def length_fr[A](l: List[A]): Int =
		foldRight(l, 0)((x,y) => 1+y)
		
	length_fr(List(1,2,1,-1))
	length_fr(List())
	length_fr(List('a','b','c'))
	
	sum_fl(List(1,2,3,-1))
	product_fl(List(1,2,3,0.5))
	
	// Practice with foldLeft and foldRight:
	
	// foldLeft(List(1,2,3), 0)(_+_) is (((0+1)+2)+3) = 6
	foldLeft(List(1,2,3), 0)(_+_)
	// foldRight(List(1,2,3), 0)(_+_) is (1+(2+(3+0))) = 6
	foldRight(List(1,2,3), 0)(_+_)
	// No difference because + is both commutative and associative.
	// However, - is associative but not commutative. So,
	// foldLeft(List(1,2,3), 0)(_-_) is (((0-1)-2)-3) = -6
	foldLeft(List(1,2,3), 0)(_-_)
	// foldRight(List(1,2,3), 0)(_-_) is (1-(2-(3-0))) = (1-(2-3)) = (1-(-1)) = 2
	foldRight(List(1,2,3), 0)(_-_)

	// foldLeft(List(1,2,3), 1)(_*_) is (((1*1)*2)*3) = 6
	foldLeft(List(1,2,3), 1)(_*_)
	// foldRight(List(1,2,3), 1)(_*_) is (1*(2*(3*1))) = 6
	foldRight(List(1,2,3), 1)(_*_)
	// No difference because * is both commutative and associative.
	// However, / is neither associative nor commutative. So,
	// foldLeft(List(1,2,3), 1)(_/_) is (((1/1)/2)/3) = 1/6
	foldLeft(List(1.0,2.0,3.0), 1.0)(_/_)
	// foldRight(List(1,2,3), 1)(_/_) is (1/(2/(3/1))) = 1.5
	foldRight(List(1.0,2.0,3.0), 1.0)(_/_)
	
	// Test for Exercise 3.12: test reverse of a list
	reverse_first_try(List(1,2,3))
	reverse(List(1,2,3))
	
	// Test for Exercise 3.14: append in terms of foldRight
	append(List(1,2),List(3,4))
                                             
  // Test for Excercise 3.15: flatten a list of lists into a single list
	flatten(List(List(1,2),List(3,4),List(5,6)))
	// TODO: check runtime of flatten (should be linear in the total length of all lists)
  // Test for Exercise 3.18: map (tail recursive version defined with foldRight)
  map(List(-1,2,-3))(x => x*x*x)
  
	// Test for Exercise 3.19: filter (tail recursive version defined with foldRight)
	// use filter to remove all odd numbers from a List[Int]
	filter(List(1,2,3,4,5))( _ % 2 == 0)

  // Test for Exercise 3.20: flatMap (tail recursive version defined with foldRight)
  // The following example should return List(1,1,2,2,3,3)
  flatMap(List(1,2,3))(x => List(x,x))

	// Test for Exercise 3.21: filter defined with flatMap
	filter_with_flatMap(List(1,2,3,4,5))( _ % 2 == 0)
	// Test for Exercise 3.23: zipWith_first_try
	zipWith_first_try(List(1,2,3), List(4,5,6))(_+_)
	
	// Test for Exercise 3.23: zipWith_first_try
	zipWith(List(1,2,3), List(4,5,6))(_+_)
	
}