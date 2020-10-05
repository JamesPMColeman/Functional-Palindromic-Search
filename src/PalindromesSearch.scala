/*
 * James Coleman
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Programming Assignment 02
 * Monday October 5th 2020
 */


object FunctionalPalindromesSearch {
	
	def main(args: Array[String]) {
		println("Welcome to the palindromic sequence project!")
		if (args.length < 2 || args.length > 3)
			println("Use: java PalindromesSearch$ n m [y]\n[y]: when informed, all palindromic sequences must be saved to a file")
	}
}
