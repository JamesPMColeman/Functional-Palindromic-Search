/*
 * James Coleman
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Programming Assignment 02
 * Monday October 5th 2020
 *
 */
import scala.collection.mutable.ArrayBuffer

object FunctionalPalindromesSearch {
	
	var combinations = new ArrayBuffer[List[Int]]
	
	// Print list
 	def printList(l: List[Int]): Unit = { 
        print("(")
        for (i <- 0 to l.length - 2) print(l(i) + ", ")
        print(l.last)
        println(")")    
    } 
	
	// Combinations
	def sumCombinations(n: Int, m: Int, acc: ArrayBuffer[Int]): Unit = {
		
		val sum = acc.sum
		// TODO: if the statement below could account for m we could safe some time.
		val value = if (acc.last < n - sum) acc.last else n - sum
		
		if (sum == n && acc.contains(m)) {
			combinations.append(acc.toList)
			printList(acc.toList)
		}
		else {
			for (j <- m to value) {
				var rec = acc.clone()
				rec.append(j)
				sumCombinations(n, m, rec)
			}
		}
	}
	// Permutations

	//

	def main(args: Array[String]): Unit = {
		println("\n\nWelcome to the palindromic sequence project!\n\n")
		if (args.length < 2 || args.length > 3) {
			println("Use: java PalindromesSearch$ n m [y]")
			println("[y]: when informed, all palindromic sequences must be saved to a file")
			System.exit(0)
		}
		var n, m = 0
		n = args(0).toInt
		m = args(1).toInt
		if (m > n || n < 1 || m < 1 || n > 127) {
			println("Gross, I don't like these numbers.")
			System.exit(0)
		}
		var accumulator = new ArrayBuffer[Int]
		accumulator.append(n)
		for (i <- m to n / 2) {
			accumulator.update(0, i)
			sumCombinations(n, m, accumulator)		
		}
	}
}
