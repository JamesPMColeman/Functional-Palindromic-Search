/*
 * James Coleman
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Programming Assignment 02
 * Monday October 5th 2020
 *
 */
import scala.collection.mutable.ArrayBuffer

object FunctionalPalindromesSearch {
	
	var combinations = new ArrayBuffer[List[Byte]]
	var count = 0
	
	// Print list
 	def printList(l: List[Byte]): Unit = { 
        print("(")
        for (i <- 0 to l.length - 2) print(l(i) + ", ")
        print(l.last)
        println(")")    
    } 
	
	// Combinations
	def sumCombinations(n: Byte, m: Byte, acc: ArrayBuffer[Byte]): Unit = {
		
		val sum = acc.sum
		// TODO: if the statement below could account for m we could safe some time.
		val value = if (acc.last < n - sum) acc.last else n - sum
		
		if (sum > n / 2) {}
		else if (acc.contains(m) || sum == (n - m) / 2) {
			combinations.append(acc.toList)
	//		println("\nSum: " + sum)
			permutations(acc.toList)
			for (j <- 1 to value) {
				var rec = acc.clone()
				rec.append(j.toByte)
				sumCombinations(n, m, rec)
			}
		}
		else {
			for (j <- 1 to value) {
				var rec = acc.clone()
				rec.append(j.toByte)
				sumCombinations(n, m, rec)
			}
		}
	}
	// Permutations
	def permutations(l: List[Byte]): Unit = {
		var perms = l.permutations
		for (i <- perms) {
	//		printList(i)
			count += 1
		}
	}
	//
	def main(args: Array[String]): Unit = {
		println("\n\nWelcome to the palindromic sequence project!\n\n")
		if (args.length < 2 || args.length > 3) {
			println("Use: java PalindromesSearch$ n m [y]")
			println("[y]: when informed, all palindromic sequences must be saved to a file")
			System.exit(0)
		}
		val start = System.currentTimeMillis()
		var n, m = 0
		n = args(0).toByte
		m = args(1).toByte
		if (m > n || n < 1 || m < 1 || n > 127) {
			println("I don't like these numbers. Please try again.")
			System.exit(0)
		}
		println("Parameter n = " + n)
		println("Parameter m = " + m)
		var accumulator = new ArrayBuffer[Byte]
		accumulator.append(0)
		for (i <- 1 to n / 2) {
			accumulator.update(0, i.toByte)
			sumCombinations(n.toByte, m.toByte, accumulator)		
		}
		val end = System.currentTimeMillis()
		
		println("\nNumber of palendromic sequences found: " + count)
		println("It took me " + ((end - start) / 1000).toInt + "s")
	}
}
