package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println(countChange(4,List(1,2)))

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if r <= 0 || c <= 0 || c >= r then 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def check(chars: List[Char], stack: List[Char]): Boolean =
      if (chars.isEmpty) {
        stack.isEmpty
      } else {
        val c = chars.head
        val opening = c == '('
        if (opening || c == ')') {
          val oposite = if opening then ')' else '('
          if opening || stack.isEmpty || stack.head != oposite 
          then check(chars.drop(1), c :: stack)
          else check(chars.drop(1), stack.drop(1)) 
        } else { check(chars.drop(1), stack) }
      }

    check(chars, "".toList)
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    def loop(money: Int, coins: List[Int]): Int = 
      if (money < 0 || coins.isEmpty ) 0
      else if (money == 0 ) 1
      else loop(money, coins.tail) + loop(money - coins.head, coins)

    loop(money, coins)