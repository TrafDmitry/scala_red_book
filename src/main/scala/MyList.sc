sealed trait MyList[+A]

case object MyNill extends MyList[Nothing]

case class MyCons [+A] (head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def sum (ints: MyList[Int]): Int = ints match {
    case MyNill => 0
    case MyCons(x, xs) => x + sum(xs)
  }

  def product (ds: MyList[Double]): Double = ds match {
    case MyNill => 1.0
    case MyCons(0.0, _) => 0.0
    case MyCons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNill
    else MyCons(as.head, apply(as.tail: _*))


  def foldRight[A, B] (ml: MyList[A], z: B)(f: (A, B) => B): B = ml match {
    case MyNill => z
    case MyCons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2 (ml: MyList[Int]) = foldRight(ml, 0) ((x, y) => x + y)

  def product2 (ml: MyList[Double]) = foldRight(ml, 1.0) (_ * _)

  def length[A](ml: MyList[A]): Int = foldRight(ml, 0)((x, y) => y + 1 )

  def foldLeft[A, B] (ml: MyList[A], z: B) (f: (B, A) => B): B = {
    def iter(ml: MyList[A], acc: B): B = ml match {
      case MyNill => acc
      case MyCons(h, t) => iter(t, f(acc, h))
    }
    iter(ml, z)
  }

  def sum3 (ml: MyList[Int]): Int = foldLeft(ml, 0) (_ + _)
  def product3 (ml: MyList[Int]): Int = foldLeft(ml, 1) (_ * _)
  def length2 (ml: MyList[Int]): Int = foldLeft(ml, 0) ((x,  _) => x + 1)

  def reverse[A](ml: MyList[A]): MyList[A] = {
    def go(ml: MyList[A], acc: MyList[A]): MyList[A] = ml match {
      case MyNill => acc
      case MyCons(h, t) => go(t, MyCons(h, acc))
    }
    go(ml, MyList())
  }

  def foldLeft2[A, B] (ml: MyList[A], z: B)(f: (B, A) => B): B = {
    foldRight(ml, z) ((a: A, b: B) => f(b, a))
  }

 }

val foo = MyList()

val test = MyList(1, 1, 3, 4 )

//val res1 = MyList.foldRight(MyList(1,2,3), MyNill:MyList[Int])(MyCons(_,_))

//val res2 = MyList.sum3(test)
//val res4 = MyList.product3(test)
//val res5 = MyList.length2(test)
val res6 = MyList.reverse(test)