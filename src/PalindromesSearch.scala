/*
 * James Coleman
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Programming Assignment 02
 * Monday October 5th 2020
 *
 */
import scala.collection.mutable.ArrayBuffer
import java.io.File
import java.io.PrintWriter

object FunctionalPalindromesSearch {

	val OUTPUT_FILE_NAME = "palindromes.txt"
	val file = new File(OUTPUT_FILE_NAME)
	val fileWriter = new PrintWriter(file)	
	var permutations = new ArrayBuffer[List[Byte]]
	var count = 0
	
	// Print list
 	def printList(l: List[Byte]): Unit = { 
        fileWriter.write("(")
        for (i <- 0 to l.length - 2) fileWriter.write(l(i) + ", ")
        fileWriter.write(l(l.length - 1) + ")\n")    
    } 
	
	// Combinations
	def sumCombinations(n: Byte, 
						m: Byte, 
						b: Boolean,
						acc: ArrayBuffer[Byte], 
						boo: (Int, ArrayBuffer[Byte]) => Boolean): Unit = {
		
		val sum = acc.sum
		val value = if (acc.last < n - sum) acc.last else n - sum
		
		if (sum > n / 2) {}
		else if (boo(sum, acc)) {
			permutation(acc.toList, b)
			for (j <- 1 to value) {
				var rec = acc.clone()
				rec.append(j.toByte)
				sumCombinations(n, m, b, rec, boo)
			}
		}
		else {
			for (j <- 1 to value) {
				var rec = acc.clone()
				rec.append(j.toByte)
				sumCombinations(n, m, b, rec, boo)
			}
		}
	}
	// Permutations
	def permutation(l: List[Byte], b: Boolean): Unit = {
		var perms = l.permutations
		for (i <- perms) {
			if (b) permutations.append(i.toList)
			count += 1
		}
	}
	// Build Palindromes
	def palindromeAssemly(n: Byte, m: Byte): Unit = {
		for (l <- permutations) {
			val s = l.sum
			if (s * 2 == n) 			printList(l.reverse ++ l)
			else if (s * 2 == n - m) 	printList(l.reverse ++ (m +: l))
			else 						printList(l.reverse ++ ((n - s * 2).toByte +: l))
		}
	}
	// Duplicate detection
	def duplicates(): Boolean = {
		var b = false
		for (i <- 0 to permutations.length - 1)
			for (j <- i + 1 to permutations.length - 1)
				if (permutations(i) == permutations(j)) b = true
		b	
	}
	def main(args: Array[String]): Unit = {
		println("\n\nWelcome to the palindromic sequence project!\n\n")
		if (args.length < 2 || args.length > 3) {
			println("Use: java PalindromesSearch n m [y]")
			println("[y]: when informed, all palindromic sequences must be saved to a file")
			System.exit(0)
		}
		val start = System.currentTimeMillis()
		var n, m = 0
		n = args(0).toInt
		m = args(1).toInt
		if (n < 1 || m < 1 || n > 127) {
			println("I don't like these numbers. Please try again.")
			System.exit(0)
		}
		println("Parameter n = " + n)
		println("Parameter m = " + m)
		if (args.length == 3) println("\nGenerating palindromic sequences...")
		val save = if (args.length == 3) true else false
		var accumulator = new ArrayBuffer[Byte]
		def evens(i: Int, ab: ArrayBuffer[Byte]): Boolean = ab.contains(m) || i == (n - m) / 2
		def evenOdd(i: Int, ab: ArrayBuffer[Byte]): Boolean = ab.contains(m)
		def oddEven(i: Int, ab: ArrayBuffer[Byte]): Boolean = ab.contains(m) || n - 2 * i % 2 == 1
		def odds(i: Int, ab: ArrayBuffer[Byte]): Boolean = ab.contains(m) || i == (n - m) / 2
		def lambda = if (n % 2 == 0 && m % 2 == 0) evens(_,_) 
				else if (n % 2 == 0 && m % 2 == 1) evenOdd(_,_)
				else if (n % 2 == 1 && m % 2 == 0) oddEven(_,_) 
				else odds(_,_)
		accumulator.append(0)
		for (i <- 1 to n / 2) {
			accumulator.update(0, i.toByte)
			sumCombinations(n.toByte, m.toByte, save, accumulator, lambda)		
		}
		val end = System.currentTimeMillis()
		if (args.length == 2) {	
			println("\nNumber of palendromic sequences found: " + count)
			println("It took me " + ((end - start) / 1000).toInt + "s\n")
		}
		else {
			palindromeAssemly(n.toByte, m.toByte)
			val end2 = System.currentTimeMillis()
			println("Done!\n")
		}
	//	println(duplicates)
		fileWriter.close()
	}
}
