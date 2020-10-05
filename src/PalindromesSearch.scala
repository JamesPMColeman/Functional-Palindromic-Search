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
	def sumToN(n: Int): Seq[Int] = {
		val temp = new ArrayBuffer[Int]
		temp.append(n)
		temp.toList
	}
	// Permutations

	//

	def main(args: Array[String]): Unit = {
		println("Welcome to the palindromic sequence project!")
		if (args.length < 2 || args.length > 3)
			println("Use: java PalindromesSearch$ n m [y]\n[y]: when informed, all palindromic sequences must be saved to a file")
		val l = Array(1,2,3)
		println("Sum of list: " + listSum(l))
	}
}
