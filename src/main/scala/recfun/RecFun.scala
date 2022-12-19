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
    def change(money: Int, coins: List[Int]): Int =
      // println(s"money: $money - coins: $coins")
      if(coins.isEmpty || money <= 0) { 0 } 
      else {
        val amount = coins.head
        val r = money % amount
        lazy val q = money / amount
        // println(s"amount: $amount - r: $r - q: $q")
        if(coins.size == 1) { if r == 0 then 1 else 0 }
        else { 
          lazy val rCount = change(r, coins.drop(1))
          val qCount = if amount <= money && (r == 0 || rCount > 0) then 1 + q * change(amount, coins.drop(1)) + rCount else 0
          qCount + change(money, coins.filter(c => amount%c != 0 ))
        }
      }
    
    val sortedCoins = coins.sorted.reverse
    change(money, sortedCoins)