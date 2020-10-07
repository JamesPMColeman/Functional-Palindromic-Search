/*
 * James Coleman
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Programming Assignment 02
 * Monday October 5th 2020
 */
import scala.collection.mutable.ArrayBuffer

object FunctionalPalindromesSearch {
	
	// Sum
	def listSum(l: Seq[Int]): Int = {
		var sum = 0
		for (i <- l) sum += i
		sum
	}
	// Combinations
	def sumToN(n: Int, acc: ArrayBuffer[Int], combos: ArrayBuffer[List[Int]]): Seq[Int] = {
		
		val sum = listSum(acc)
		val value = if (acc.last < n - sum) acc.last else n - sum
		
		if (sum == n) combos.append(acc.toList)
		
		else {
			for (j <- 1 to value) {
				ArrayBuffer[Int] rec = acc.clone()
				rec + j
				combos ++ sumToN(n, rec, combos)
			}
		combos
		}
	}
	// Permutations

	//

	def main(args: Array[String]): Unit = {
		println("Welcome to the palindromic sequence project!")
		if (args.length < 2 || args.length > 3)
			println("Use: java PalindromesSearch$ n m [y]\n[y]: when informed, all palindromic sequences must be saved to a file")
		val l = Array(1,2,3)
		println("Sum of list: " + listSum(l))
		// print sum list
		for (num <- sumToN(4)) println(num)
	}
}
