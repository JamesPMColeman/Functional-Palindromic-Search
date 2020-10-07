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
		
		if (acc.contains(m)) {
			combinations.append(acc.toList)
			permutations(acc.toList)
		}
		else {
			for (j <- m to value) {
				var rec = acc.clone()
				rec.append(j.toByte)
				sumCombinations(n, m, rec)
			}
		}
	}
	// Permutations
	def permutations(l: List[Byte]): Unit = {
		var perms = l.permutations
		println("")
		for (i <- perms) printList(i)
	}
	//
	def main(args: Array[String]): Unit = {
		println("\n\nWelcome to the palindromic sequence project!\n\n")
		if (args.length < 2 || args.length > 3) {
			println("Use: java PalindromesSearch$ n m [y]")
			println("[y]: when informed, all palindromic sequences must be saved to a file")
			System.exit(0)
		}
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
		for (i <- m to n / 2) {
			accumulator.update(0, i.toByte)
			sumCombinations(n.toByte, m.toByte, accumulator)		
		}
	}
}
