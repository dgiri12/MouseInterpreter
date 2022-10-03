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
        if(";".contains(getChar)){
          var myStr: String = ""
          nextChar
            while(!hasNewLine){
              myStr = myStr + getChar
              nextChar //this loop ignores words until the end of line
            }
            nextChar
            return new Lexeme(myStr, Token.COMMENT)
          }

        
        if ("?".contains(getChar)){
          nextChar
          return new Lexeme("?", Token.INPUT)
        }

        if ("!".contains(getChar)){
          nextChar
          return new Lexeme("!", Token.OUTPUT)
        }

        if ("=".contains(getChar)){
          nextChar
            return new Lexeme("=", Token.ASSIGNMENT)
        }

        if ("+".contains(getChar)){
          nextChar
            return new Lexeme("+", Token.ADDITION)
        }

        if ("-".contains(getChar)){
          nextChar
            return new Lexeme("-", Token.SUBTRACTION)
        }

        if ("*".contains(getChar)){
          nextChar
            return new Lexeme("*", Token.MULTIPLICATION)
        }

        if ("/".contains(getChar)){
          nextChar
          return new Lexeme("/", Token.DIVISION)
        }

        if ("%".contains(getChar)){
          nextChar
          return new Lexeme("%", Token.MODULUS)
        }

        if ("<".contains(getChar)){
          nextChar
            return new Lexeme("<", Token.LESS)
        }
        if (">".contains(getChar)){
          nextChar
          return new Lexeme(">", Token.GREATER)
        }
        if ("^".contains(getChar)){
          nextChar
          return new Lexeme("^", Token.BREAK)
        }

        if (".".contains(getChar)){
          nextChar
          return new Lexeme(".", Token.DOT)
        }

        if ("(".contains(getChar)){
          nextChar
          return new Lexeme("(", Token.OPEN_PAR)
        }

        if (")".contains(getChar)){
          nextChar
          return new Lexeme(")", Token.CLOSE_PAR)
        }

        if ("[".contains(getChar)){
          nextChar
          return new Lexeme("[", Token.OPEN_BRACKET)
        }

        if ("]".contains(getChar)){
          nextChar
          return new Lexeme("]", Token.CLOSE_BRACKET)
        }        

        //now check for double occurences of lexemes
        //to check double occurences you need a place to store the chars
        // dont 'nextChar' carelessly

        var storeChar: String = ""
        storeChar = storeChar + getChar // store the current char so that you can get
        // the next char, then check double occurences
        nextChar
        var doubleChars: String = storeChar + getChar
        // also you will be needing storeChar later in this code block, you gotta move a step back
        // to check for identifiers now
        
        if ("$$".contains(doubleChars)){
          nextChar //nextChar here, because you need to consume these characters,
          // then char is at a fresh position for the next loop iteration
          return new Lexeme("$$", Token.EO_PRG)
        }

        if ("<=".contains(doubleChars)){
          nextChar
          return new Lexeme("<=", Token.LESS_EQUAL)
        }
        if (">=".contains(doubleChars)){
          nextChar
          return new Lexeme(">=", Token.GREATER_EQUAL)
        }

        if ("==".contains(doubleChars)){
          nextChar
          return new Lexeme("==", Token.EQUAL)
        }

        if ("!=".contains(doubleChars)){
          nextChar
          return new Lexeme("!=", Token.DIFFERENT)
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

