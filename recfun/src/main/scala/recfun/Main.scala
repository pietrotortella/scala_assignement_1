package recfun


object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (r == 0) 1
      else if (c == 0) 1
      else if (c == r) 1
      else pascal(c, r-1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def locBalance(chars: List[Char], opens: Int): Boolean = {
        if (chars.isEmpty) 
          if (opens == 0) true
          else false
        else if (chars.head == ')')
          if (opens == 0) false
          else locBalance(chars.tail, opens - 1)
        else if (chars.head == '(') locBalance(chars.tail, opens + 1)
        else locBalance(chars.tail, opens)
      }
      
      locBalance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty) 0
      else if (money - coins.head >= 0) countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else countChange(money, coins.tail)
    }
  }
