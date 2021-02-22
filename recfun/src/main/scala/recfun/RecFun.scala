package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r || r == 0 || r == 1) 1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def score(chars: List[Char], acc: Int = 0): Int =
      if (chars.isEmpty || acc < 0)
        acc
      else {
        val c = chars.head
        score(chars.tail, if (c == '(') acc + 1 else if (c == ')') acc - 1 else acc)
      }
    score(chars) == 0
  }



  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???

  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)

  def sumCubes(a: Int, b: Int) = sum(x => x * x * x, a, b)
}
