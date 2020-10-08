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
	def sumCombinations(n: Byte, m: Byte, acc: ArrayBuffer[Byte], boo: (Int, ArrayBuffer[Byte]) => Boolean): Unit = {
		
		val sum = acc.sum
		// TODO: if the statement below could account for m we could safe some time.
		val value = if (acc.last < n - sum) acc.last else n - sum
		
		if (sum > n / 2) {}
		else if (boo(sum, acc)) {
			combinations.append(acc.toList)
			println("\nSum: " + sum)
			permutations(acc.toList)
			for (j <- 1 to value) {
				var rec = acc.clone()
				rec.append(j.toByte)
				sumCombinations(n, m, rec, boo)
			}
		}
		else {
			for (j <- 1 to value) {
				var rec = acc.clone()
				rec.append(j.toByte)
				sumCombinations(n, m, rec, boo)
			}
		}
	}
	// Permutations
	def permutations(l: List[Byte]): Unit = {
		var perms = l.permutations
		for (i <- perms) {
			printList(i)
			count += 1
		}
	}
	// Build Palindromes
	def palindromeAssemly(n: Byte, m: Byte): Unit = {
		// I may be able to save time by passing sum into combinations instead of recalculating it
		// TODO finish
		for (l <- combinations) {
			val s = l.sum
			if (s * 2 == n) 			printList(l.reverse ++ l)
			else if (s * 2 == n - m) 	printList(l.reverse ++ (m +: l))
			else 						printList(l.reverse ++ ((n - s * 2).toByte +: l))
		}
	}
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
		if (args.length == 2) {
			println("Parameter n = " + n)
			println("Parameter m = " + m)
		}
		else println("Generating palindromic sequences...")

		var accumulator = new ArrayBuffer[Byte]
		def evens(i: Int, ab: ArrayBuffer[Byte]): Boolean = ab.contains(m) || i == (n - m) / 2
		def evenOdd(i: Int, ab: ArrayBuffer[Byte]): Boolean = ab.contains(m)
		def oddEven(i: Int, ab: ArrayBuffer[Byte]): Boolean = ab.contains(m) || n - 2 * i % 2 == 1
		def odds(i: Int, ab: ArrayBuffer[Byte]): Boolean = ab.contains(m) || i == (n - 1) / 2
		def lambda = if (n % 2 == 0 && m % 2 == 0) evens(_,_) 
				else if (n % 2 == 0 && m % 2 == 1) evenOdd(_,_)
				else if (n % 2 == 1 && m % 2 == 0) oddEven(_,_) 
				else odds(_,_)
		accumulator.append(0)
		for (i <- 1 to n / 2) {
			accumulator.update(0, i.toByte)
			sumCombinations(n.toByte, m.toByte, accumulator, lambda)		
		}
		val end = System.currentTimeMillis()
		if (args.length == 2) {	
			println("\nNumber of palendromic sequences found: " + count)
			println("It took me " + ((end - start) / 1000).toInt + "s")
		}
		else {
			println("\n=====================================\n")
			palindromeAssemly(n.toByte, m.toByte)
			println("Done: " + count)			
		}
	}
}
