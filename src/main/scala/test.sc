//val int = 12

def fib(n: Int): Int = {
  if (n <= 1) n
  else fib(n - 1) + fib(n - 2)
}

def fibt(n: Int): Int = {
  def go(n:Int, a: Int, b: Int): Int =
    if (n <= 0) b
    else
      go(n - 1, a + b, a )
  go(n, 1, 0)
}

val result = fibt(9)

//def getFibonacci(index: Int): Int = {
//  @annotation.tailrec
//  def getTailRec(index: Int, prev: Int, current: Int): Int = {
//    if (index <= 0) {
//      current
//    } else {
//      getTailRec(index - 1, prev = prev + current, current = prev)
//    }
//  }
//
//  getTailRec(index, prev = 1, current = 0)
//}