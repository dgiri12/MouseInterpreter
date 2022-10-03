/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Prg 01 - Tree (each tree node has a lexeme and branches)
 * Student: Diptanshu Giri
 */

import scala.collection.mutable.ArrayBuffer

class Node(var lexeme: Lexeme) {

  private val branches = new ArrayBuffer[Node]()

  def add(branch: Node): Unit = {
    branches += branch
  }

  def getBranches() = branches

  private def print(current: Node, tabs: String): String = {
    var out = ""
    if (current == null)
      out
    else {
      out += tabs + current.lexeme + "\n"
      for (branch <- current.branches)
        out += print(branch, tabs + "\t")
      out
    }
  }

  override def toString = print(this, "")
}

// example code
object Node {
  
}
