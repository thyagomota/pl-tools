package pltools

import pltools.Grammar.EPSILON_SYMBOL

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Project pltools
 * @author Thyago Mota
 * @date 2020-09-17
 * Models context-free grammars from a source file. It stores the grammar productions using an array of tuples. The first element is of type string and it represents the left-hand side symbol. The second element is an array of strings with all of the right-hand side symbols.
 */
class Grammar(private var source: Source) {

  private val productions = new ArrayBuffer[(String, ArrayBuffer[String])]
  for (line <- source.getLines()) {
    val fields = line.split(Grammar.DEFINING_SYMBOL)
    val lhs = fields(0).strip()
    val rhs = new ArrayBuffer[String]
    for (symbol <- fields(1).strip().split(Grammar.CONCATENATE_SYMBOL))
      rhs += symbol
    // epsilon (when in the rhs of a production) must appear solo;
    // epsilon should not be allowed to appear in the lhs
    if ( !lhs.equals(EPSILON_SYMBOL) &&
         ( !rhs.contains(Grammar.EPSILON_SYMBOL) ||
           (rhs.contains(Grammar.EPSILON_SYMBOL) && rhs.length == 1)) )
      productions += ((lhs, rhs))
  }

  /**
   * Returns the number of productions of the grammar
   * @return the number of productions
   */
  def size() = productions.length

  /**
   * Returns the left-hand side symbol of the production identified by index
   * @return a symbol
   * @throws java.lang.IndexOutOfBoundsException
   */
  @throws(classOf[IndexOutOfBoundsException])
  def getLHS(index: Integer) = {
    val production = productions(index)
    production._1
  }

  /**
   * Returns the right-hand side symbol(s) of the production identified by index
   * @return an array of symbols
   * @throws java.lang.IndexOutOfBoundsException
   */
  @throws(classOf[IndexOutOfBoundsException])
  def getRHS(index: Integer) = {
    val production = productions(index)
    production._2
  }

  /**
   * Returns all variable symbols identified in the grammar
   * @return an array of variable symbols (strings)
   */
  def getVariables = {
    val variables = new ArrayBuffer[String]()
    for (production <- productions) {
      if (!variables.contains(production._1))
        variables += production._1
    }
    variables
  }

  /**
   * Returns all terminal symbols (non-variables) identified in the grammar
   * @return an array of terminal symbols (strings)
   */
  def getTerminals = {
    val terminals = new ArrayBuffer[String]()
    val variables = getVariables
    for (production <- productions) {
      for (symbol <- production._2)
        if ( !variables.contains(symbol) &&
             !terminals.contains(symbol) &&
             !symbol.equals(Grammar.EPSILON_SYMBOL) )
          terminals += symbol
    }
    terminals
  }

  /**
   * Returns true/false whether the given symbol is a terminal or not
   * @param symbol a string
   * @return true/false
   */
  def isTerminal(symbol: String) = {
    getTerminals.contains(symbol)
  }

  /**
   * Returns all of the productions of the grammar as an array of strings
   * @return an array of strings
   */
  def getProductions = productions

  /**
   * Returns the production identified by the given index
   * @throws java.lang.IndexOutOfBoundsException
   * @return
   */
  @throws(classOf[IndexOutOfBoundsException])
  def getProduction(index: Integer) =
    productions(index)

  /**
   * Returns all productions that have the given symbol on their lhs
   * @param symbol a string
   * @return an array with the productions
   */
  def getProductions(symbol: String) = {
    val productions_ = new ArrayBuffer[String]
    for (production <- productions) {
      val lhs = production._1
      if (lhs.equals(symbol))
        productions_ += lhs
    }
    productions_
  }


//  def getFirst(symbol: String) = {
//    if (isTerminal(symbol))
//      true
//    else if
//  }

  /**
   * Returns the start symbol of the grammar
   * @return the start symbol (a string)
   */
  @throws(classOf[IndexOutOfBoundsException])
  def getStart = getLHS(0)

  /**
   * Returns true/false depending whether there is an epsilon production in the grammar
   * @return true/false
   */
  def hasEpsilonProduction: Boolean = {
      for (production <- productions)
        for (symbol <- production._2)
          if (symbol.equals(Grammar.EPSILON_SYMBOL))
            return true
    false
  }

  /**
   * Returns a string representation of the grammar, with one production per line; productions are numbered starting at 0
   * @return a string
   */
  override def toString: String = {
    var out = ""
    for (i <- 0 until productions.length)
      out += i + ". " + getLHS(i) + " " + Grammar.DEFINING_SYMBOL + " " + getRHS(i).mkString(Grammar.CONCATENATE_SYMBOL) + "\n"
    out = out.strip()
    out
  }
}

object Grammar {

  val EPSILON_SYMBOL     = "ε"
  var DEFINING_SYMBOL    = "->"
  var CONCATENATE_SYMBOL = " "
  val EOF_SYMBOL         = "$"
  var GRAMMARS_FOLDER    = "grammars"

  def main(args: Array[String]): Unit = {
    val grammar = new Grammar(Source.fromFile("grammars/grammar_a.txt"))
    println(grammar.getLHS(10))
  }
}
