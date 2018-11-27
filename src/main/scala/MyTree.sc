sealed trait MyTree[+A]

case class MyLeaf[A](value: A) extends MyTree[A]

case class MyBranch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {

  def size[A](mt: MyTree[A]): Int = mt match {
    case MyLeaf(_) => 1
    case MyBranch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(mt: MyTree[Int]): Int = mt match {
    case MyLeaf(v) => v
    case MyBranch(l, r) =>  maximum(r) max maximum(l)
  }

  def depth[A](ml: MyTree[A]): Int = ml match {
    case MyLeaf(_) => 0
    case MyBranch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map [A, B](mt: MyTree[A])(f: A => B): MyTree[B] = mt match {
    case MyLeaf(v) => MyLeaf(f(v))
    case MyBranch(l, r) => MyBranch(map(l)(f), map(r)(f))
  }

  def fold[A, B](mt: MyTree[A])(f: A => B)(g: (B, B) => B): B = mt match {
    case MyLeaf(v) => f(v)
    case MyBranch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A] (mt: MyTree[A]): Int =
    fold(mt)(_ => 1)(1 + _ + _)

  def maxViaFold(mt: MyTree[Int]): Int =
    fold(mt)((v: Int) => v)(_ max _)

  def depthViaFold[A](mt: MyTree[A]): Int =
    fold(mt)(_ => 0)((l: Int, r: Int) => 1 + (l max r))

  def mapViaFold[A, B](mt: MyTree[A])(f: A => B): MyTree[B] =
    fold(mt)((v: A) => MyLeaf(f(v)): MyTree[B])(MyBranch(_, _))

}

val myLeaf1 = MyLeaf(1)
val myLeaf2 = MyLeaf(2)
val myLeaf3 = MyLeaf(3)

val branch = MyBranch(myLeaf1, myLeaf3)
val branch1 = MyBranch(myLeaf1, myLeaf3)
val branch3 = MyBranch(branch1, myLeaf3)
val branch2 = MyBranch(branch, branch3)

val res = MyTree.size(branch)
println("sad")

val res1 = MyTree.depth(branch2)
val res2 = MyTree.map(branch2)(_ + 1)
val res3 = MyTree.fold(branch1)(x => x * 2)(_ + _)



