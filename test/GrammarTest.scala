import java.io.File

import org.scalatest.FunSuite
import pltools.Grammar

import scala.io.Source

/**
 * Project pltools
 * @author Thyago Mota
 * @date 2020-09-18
 * Unit tests for the Grammar class.
 */
class GrammarTest extends FunSuite {

  test("simple grammar with epsilon") {
    val grammar = new Grammar(Source.fromString("A -> a A\nA -> ε"))
    assert(grammar.size() == 2)
    assert(grammar.getLHS(0).equals("A"))
    var rhs = grammar.getRHS(0)
    assert(rhs.length == 2)
    assert(rhs(0).equals("a"))
    assert(rhs(1).equals("A"))
    assert(grammar.getLHS(1).equals("A"))
    rhs = grammar.getRHS(1)
    assert(rhs.length == 1)
    assert(rhs(0).equals("ε"))
    val variables = grammar.getVariables
    assert(variables.length == 1)
    assert(variables(0).equals("A"))
    val terminals = grammar.getTerminals
    assert(terminals.length == 1)
    assert(terminals(0).equals("a"))
    assertThrows[IndexOutOfBoundsException] {
      grammar.getLHS(2)
    }
    assertThrows[IndexOutOfBoundsException] {
      grammar.getRHS(2)
    }
    assert(grammar.getStart.equals("A"))
    assert(grammar.hasEpsilonProduction)
  }
}
