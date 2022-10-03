/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Prg 01 - SyntaxAnalyzer (an iterable syntax analyzer)
 * Student(s) Name(s): Diptanshu Giri
 */

class SyntaxAnalyzer(private var source: String) {

  private val it = new LexicalAnalyzer(source).iterator
  private var current: Lexeme = null
  private var noOpenBrackets: Int = 0
  private var noOpenParen: Int = 0

  // returns the current lexeme
  private def getLexeme: Lexeme = {
    if (current == null)
      nextLexeme
    current
  }

  // advances the input one lexeme
  private def nextLexeme = {
    current = it.next
  }

  // returns true if the given token identifies a statement (or the beginning of a statement)
  private def isStatement(token: Token.Value): Boolean = {
    token == Token.IDENTIFIER     ||
    token == Token.LITERAL        ||
    token == Token.STRING         ||
    token == Token.INPUT          ||
    token == Token.OUTPUT         ||
    token == Token.ASSIGNMENT     ||
    token == Token.ADDITION       ||
    token == Token.SUBTRACTION    ||
    token == Token.MULTIPLICATION ||
    token == Token.DIVISION       ||
    token == Token.MODULUS        ||
    token == Token.LESS           ||
    token == Token.LESS_EQUAL     ||
    token == Token.GREATER        ||
    token == Token.GREATER_EQUAL  ||
    token == Token.EQUAL          ||
    token == Token.DIFFERENT      ||
    token == Token.BREAK          ||
    token == Token.DOT            ||
    token == Token.OPEN_BRACKET   ||
    token == Token.OPEN_PAR
  }

  // returns true if the given token identifies a line (or the beginning of a line)
  // a line can be a statement or a comma
  private def isLine(token: Token.Value): Boolean = {
    isStatement(token) || token == Token.COMMENT
  }

  private def isComment(token: Token.Value): Boolean = {
    token == Token.COMMENT
  }

  // parses the program, returning its corresponding parse tree
  def parse: Node = {
    parseMouse
  }

  // TODO: mouse = { line } ´$$´
  private def parseMouse: Node = {
    val node = new Node(new Lexeme("mouse"))
    var loop: Boolean = true
    var failCounter: Int = 0
    while(loop){
      if (isLine(getLexeme.token)){
      node.add(parseLine)
      }
      if (getLexeme.token == Token.EO_PRG){
        loop = false
        node.add(new Node(getLexeme))
      }
    }
    node
  }

  // TODO: line = statement | comment
  private def parseLine: Node = {
    val node = new Node(new Lexeme("line"))
    if (isComment(getLexeme.token)){
      node.add(new Node(getLexeme))
      nextLexeme //only use this at end nodes. like leaf nodes
      return node
    }
    if (isStatement(getLexeme.token)){
      node.add(parseStatement)
    }
    node
  }

  // TODO: statement = ´?´ | ´!´ | string | identifier | ´=´ | literal | ´+´ | ´-´ | ´*´ | ´/´ | ´%´ | ´<´ | ´<=´ | ´>´ | ´>=´ | ´==´ | ´!=´ | ´^´ | ´.´ |  | if | while
  private def parseStatement: Node = {
    val node = new Node(new Lexeme("statement"))
    if (getLexeme.token == Token.OPEN_BRACKET){
      noOpenBrackets += 1
      node.add(parseIf)
      return node
    }
    if (getLexeme.token == Token.CLOSE_BRACKET){
      if (noOpenBrackets - 1 < 0){
        throw new Exception ("Syntax Analyzer Error: Wrong syntax! Wrong ] placement")
    }
    }
    if (getLexeme.token == Token.CLOSE_PAR){
      if (noOpenParen - 1 < 0){
        throw new Exception ("Syntax Analyzer Error: Wrong syntax! ) not expected")
      }
    }
    if (getLexeme.token == Token.OPEN_PAR){
      noOpenParen += 1
      node.add(parseWhile)
      return node
    } 
    if (isStatement(getLexeme.token)){
      node.add(new Node(getLexeme))
      nextLexeme
    }
    node
  }

  // TODO: if = ´[´ { line } ´]´
  def parseIf: Node = {
    val node = new Node(new Lexeme("if"))
    node.add(new Node(getLexeme)) //add the opend bracket lexeme to tree
    nextLexeme
    var loop: Boolean = true
    while(loop){
      if (isLine(getLexeme.token)){
      node.add(parseLine)
      }
    if (getLexeme.token == Token.EO_PRG){
      throw new Exception ("Syntax Analyzer Error: Wrong syntax! where is closing bracket??")
    }
      if (getLexeme.token == Token.CLOSE_BRACKET){
        noOpenBrackets -= 1
        loop = false
        node.add(new Node(getLexeme))
        nextLexeme
      }
    }
    node
  }

  // TODO: while = ´(´ { line } ´)´
  def parseWhile: Node = {
    val node = new Node(new Lexeme("while"))
    node.add(new Node(getLexeme)) //add the opend paren lexeme to tree
    nextLexeme
    var loop: Boolean = true
    while(loop){
      if (isLine(getLexeme.token)){
      node.add(parseLine)
      }

      if (getLexeme.token == Token.CLOSE_PAR){
        noOpenParen -= 1
        loop = false
        node.add(new Node(getLexeme))
        nextLexeme
      }
    }
    node
  }
}

object SyntaxAnalyzer {
  /*
  def main(args: Array[String]): Unit = {

    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse
    print(parseTree)
  }*/
}
