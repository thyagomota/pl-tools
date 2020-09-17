package pltools

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Project pltools
 *
 * @author Thyago Mota
 * @date 2020-09-17
 * Models context-free grammars from a source file.
 */
class Grammar(private var source: String) {

  private val productions = new ArrayBuffer[(String, String)]
  for (line <- Source.fromFile(source).getLines()) {
    val fields = line.split("->")
    val production = (fields(0).strip(), fields(1).strip())
    productions += production
  }

  def getLHS(index: Integer) = {
    val production = productions(index)
    production._1
  }

  def getRHS(index: Integer) = {
    val production = productions(index)
    production._2.split(" ")
  }

  def size() = productions.length

  def getVariables() = {
    val variables = new ArrayBuffer[String]()
    for (production <- productions) {
      if (!variables.contains(production._1))
        variables += production._1
    }
    variables
  }

  def getTerminals() = {
    val terminals = new ArrayBuffer[String]()
    val variables = getVariables()
    for (production <- productions) {
      for (symbol <- production._2.split(" "))
        if (!variables.contains(symbol) && !terminals.contains(symbol))
          terminals += symbol
    }
    terminals
  }

  def getStartSymbol = getLHS(0)

  override def toString: String = {
    var out = ""
    for (i <- 0 until productions.length)
      out += i + ". " + getLHS(i) + " -> " + getRHS(i).mkString(" ") + "\n"
    out
  }
}

object Grammar {
  def main(args: Array[String]): Unit = {
    val grammar = new Grammar("grammar.txt")
    print(grammar.getStartSymbol)
  }
}
