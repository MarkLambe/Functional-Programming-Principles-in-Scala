package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }
  
  def testBal(){
	   print(balance("())(".toList)) 
	   println()
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c > r || c < 0 || r < 0) 0//Bounds checking
    else{
      if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1);
    }
  }

  /**
   * Exercise 2
   */  
  def balance(chars: List[Char]): Boolean = {
	def iter(sub: List[Char], bal: Int): Int = {
	  if (sub.isEmpty) bal
	  else{
	    if(sub.head == '(') iter(sub.tail, bal + 1)
	    else if(sub.head == ')'){
	    	if(bal > 0) iter(sub.tail,bal - 1) else bal - 1
	    }
	    else iter(sub.tail, bal)
	  }
	}
	iter(chars, 0) == 0
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {   
    def iter(num: Int, denoms: List[Int], count: Int): Int = {
      if(num < 0 || denoms.isEmpty) 0
      else{
        if(num == 0) count + 1
        else iter(num - denoms.head,denoms, count) + iter(num,denoms.tail, count)
      }//else
    }//iter

    iter(money, coins.sortWith(_ > _), 0)
  }
}
