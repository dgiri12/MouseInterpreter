/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Homework 04 - LexicalAnalyzer (an iterable lexical analyzer)
 * Student Name: Diptanshu Giri
 */

import LexicalAnalyzer.{BLANKS, DIGITS, LETTERS, NEW_LINE, PUNCTUATIONS, SPECIALS}
import scala.io.Source

class LexicalAnalyzer(private var source: String) extends Iterable[Lexeme]{

  var commentLine: Boolean = true

  var input = ""
  var thereisadot: Boolean = false
  for (line <- Source.fromFile(source).getLines)
    input += line + LexicalAnalyzer.NEW_LINE //adds a new line character at the end of each line in 'input'
  input = input.trim //removes whitespaces

  // checks if reached eof
  private def eof: Boolean = input.length == 0

  var currentChar: Char = 0

  // returns the current char
  private def getChar = {
    if (!eof)
      currentChar = input(0)
    currentChar
  }

  // advances the input one character
  private def nextChar: Unit = {
    if (!eof)
      input = input.substring(1)
  }

  // checks if input has a blank character ahead
  private def hasBlank: Boolean = {
    LexicalAnalyzer.BLANKS.contains(getChar)
  }

  // reads the input until a non-blank character is found, updating the input
  def readBlanks: Unit = {
    while (!eof && hasBlank) {
      nextChar
    }
  }

  // checks if input has a letter ahead
  private def hasLetter: Boolean = {
    LexicalAnalyzer.LETTERS.contains(getChar)
  }

  // checks if input has a digit ahead
  private def hasDigit: Boolean = {
    LexicalAnalyzer.DIGITS.contains(getChar)
  }

  // checks if input has a special character ahead
  private def hasSpecial: Boolean = {
    LexicalAnalyzer.SPECIALS.contains(getChar)
  }

  // checks if input has a punctuation character ahead
  private def hasPunctuation: Boolean = {
    LexicalAnalyzer.PUNCTUATIONS.contains(getChar)
  }

  // checks if input has a punctuation character ahead
  private def hasNewLine: Boolean = {
    LexicalAnalyzer.NEW_LINE.contains(getChar)
  }

  // returns an iterator for the lexical analyzer
  override def iterator: Iterator[Lexeme] = {

    new Iterator[Lexeme] {

      // returns true/false depending whether there is a lexeme to be read from the input
      override def hasNext: Boolean = {
        readBlanks
        !eof
      }

      // returns the next lexeme (or Token.EOF if there isn't any lexeme left to be read)
      override def next(): Lexeme = {

        if (!hasNext)
          return new Lexeme("eof", Token.EOF)

        // TODO: finish the implementation
        //readBlanks
        if (thereisadot){
          thereisadot=false //turn it off
          return new Lexeme(".",Token.DOT)
        }
        var myStr=""
        while(!hasBlank && !eof){
          if ("\"".contains(getChar)){
            nextChar
            while(!"\"".contains(getChar)){
              myStr = myStr + getChar
              nextChar
            }
            nextChar //to skip the last ' " '
            return new Lexeme(myStr, Token.STRING)
          }
          myStr = myStr + getChar
          nextChar
      
        }//now myStr contains a full word

        if(";".contains(myStr(0))){
          nextChar
          myStr=myStr.substring(1)
            while(!hasNewLine){
              myStr = myStr + getChar
              nextChar //this loop ignores words until the end of line
            }
            return new Lexeme(myStr, Token.COMMENT)
          }

        
if (myStr == "?"){
            return new Lexeme(myStr, Token.INPUT)
        }

        if (myStr == "!"){
            return new Lexeme(myStr, Token.OUTPUT)
        }

        if (myStr == "="){
            return new Lexeme(myStr, Token.ASSIGNMENT)
        }

        if (myStr == "+"){
            return new Lexeme(myStr, Token.ADDITION)
        }

        if (myStr == "-"){
            return new Lexeme(myStr, Token.SUBTRACTION)
        }

        if (myStr == "*"){
            return new Lexeme(myStr, Token.MULTIPLICATION)
        }

        if (myStr == "/"){
            return new Lexeme(myStr, Token.DIVISION)
        }

        if (myStr == "%"){
            return new Lexeme(myStr, Token.MODULUS)
        }

        if (myStr == "<"){
            return new Lexeme(myStr, Token.LESS)
        }
        if (myStr == ">"){
            return new Lexeme(myStr, Token.GREATER)
        }
        if (myStr == "^"){
            return new Lexeme(myStr, Token.BREAK)
        }

        if (myStr == "."){
            return new Lexeme(myStr, Token.DOT)
        }

        if (myStr == "("){
            return new Lexeme(myStr, Token.OPEN_PAR)
        }

        if (myStr == ")"){
            return new Lexeme(myStr, Token.CLOSE_PAR)
        }

        if (myStr == "["){
            return new Lexeme(myStr, Token.OPEN_BRACKET)
        }

        if (myStr == "]"){
            return new Lexeme(myStr, Token.CLOSE_BRACKET)
        }
        if (myStr == "$$"){
            return new Lexeme(myStr, Token.EO_PRG)
        }

        if (myStr == "<="){
            return new Lexeme(myStr, Token.LESS_EQUAL)
        }
        if (myStr == ">="){
            return new Lexeme(myStr, Token.GREATER_EQUAL)
        }

        if (myStr == "=="){
            return new Lexeme(myStr, Token.EQUAL)
        }

        if (myStr == "!="){
            return new Lexeme(myStr, Token.DIFFERENT)
        }

        //'identifier' identifier
        if (LexicalAnalyzer.LETTERS.contains(myStr(0))){
          if (".".contains(myStr.charAt(myStr.length()-1))){
            thereisadot = true
            //remove that dot from myStr
            myStr = myStr.substring(0, myStr.length - 1)
          }
          return new Lexeme(myStr,Token.IDENTIFIER)
        }

        //' identifier
        if (LexicalAnalyzer.DIGITS.contains(myStr(0))){
          if(LexicalAnalyzer.DIGITS.contains(myStr)){ //make sure the whole string is digits
          return new Lexeme(myStr,Token.LITERAL)
          }
        }

        // throw an exception if an unrecognizable symbol is found
        throw new Exception("Lexical Analyzer Error: unrecognizable symbol found!")
      }
    }
  }
}

object LexicalAnalyzer {
  val BLANKS       = " \n\t"
  val NEW_LINE     = "\n"
  val LETTERS      = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS       = "0123456789"
  val PUNCTUATIONS = ".,;:?!"
  val SPECIALS     = "<_@#$%^&()-+='/\\[]{}|"
}

