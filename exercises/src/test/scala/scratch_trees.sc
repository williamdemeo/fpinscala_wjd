package fpinscala.datastructures
import Tree._


object scratch_trees {
	
	// Binary Tree tests
	val my_tree = Branch[Int](Leaf(1), Leaf(2))
                                                  //> my_tree  : fpinscala.datastructures.Branch[Int] = Branch(Leaf(1),Leaf(2))
	val my_tree2 = Branch(my_tree, Leaf(3))   //> my_tree2  : fpinscala.datastructures.Branch[Int] = Branch(Branch(Leaf(1),Lea
                                                  //| f(2)),Leaf(3))
	val my_tree3 = Branch(Leaf(0), my_tree2)  //> my_tree3  : fpinscala.datastructures.Branch[Int] = Branch(Leaf(0),Branch(Bra
                                                  //| nch(Leaf(1),Leaf(2)),Leaf(3)))

	depth(my_tree)                            //> res0: Int = 2
	depth_alternative(my_tree)                //> res1: Int = 2
	depth(my_tree2)                           //> res2: Int = 3
	depth_alternative(my_tree2)               //> res3: Int = 3
	maximum(my_tree)                          //> res4: Int = 2
	maximum(my_tree2)                         //> res5: Int = 3
	size(my_tree2)                            //> res6: Int = 5
	size_with_foldLeft(my_tree2)              //> res7: Int = 5
	size(my_tree3)                            //> res8: Int = 7
	size_with_foldLeft(my_tree3)              //> res9: Int = 7
}