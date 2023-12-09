package labs.task4

import org.scalatest.funsuite.AnyFunSuiteLike

class Task4Test extends AnyFunSuiteLike {
  private val A = Variable("A")
  private val B = Variable("B")
  private val variableToValue = Map("A" -> true, "B" -> false)

  test("testBeautifyExpr") {
    assert(Task4.beautifyExpr(Conjunction(A, B)) == Conjunction(A, B))
    assert(Task4.beautifyExpr(Implication(A, B)) == Disjunction(Negation(A), B))
    assert(Task4.beautifyExpr(Disjunction(A, Constant(true))) == Constant(true))
    assert(Task4.beautifyExpr(Disjunction(Constant(true), Constant(false))) == Constant(true))
    assert(Task4.beautifyExpr(Conjunction(A, Implication(Disjunction(A, B), B))) ==
      Conjunction(A, Disjunction(Conjunction(Negation(A), Negation(B)), B)))
    assert(Task4.beautifyExpr(Negation(Negation(Negation(A)))) == Negation(A))
    assert(Task4.beautifyExpr(Negation(Negation(Negation(Negation(A))))) == A)
    assert(Task4.beautifyExpr(Negation(Constant(true))) == Constant(false))


    println(ExprUtils.exprAsString(Conjunction(Negation(Conjunction(Negation(A), Conjunction(B, Constant(true)))),
      Negation(Implication(A, Disjunction(B, Constant(false)))))))
    println(ExprUtils.exprAsString(Task4.beautifyExpr(Conjunction(Negation(Conjunction(Negation(A), Conjunction(B, Constant(true)))),
      Negation(Implication(A, Disjunction(B, Constant(false))))))))
    assert(Task4.beautifyExpr(Conjunction(Negation(Conjunction(Negation(A), Conjunction(B, Constant(true)))),
      Negation(Implication(A, Disjunction(B, Constant(false)))))) == Conjunction(Disjunction(A, Negation(B)), Conjunction(A, Negation(B))))


  }

  test("testEvalExpr") {
    assert(!Task4.evalExpr(Conjunction(A, B), variableToValue))
    assert(Task4.evalExpr(Disjunction(A, B), variableToValue))
    assert(!Task4.evalExpr(Negation(A), variableToValue))
    assert(Task4.evalExpr(Constant(true), variableToValue))

    assert(!Task4.evalExpr(Conjunction(Disjunction(Constant(true), B), Constant(false)), variableToValue))
  }

  test("testBeautifyAndEvalExpr") {
    println(ExprUtils.exprAsString(Conjunction(Negation(Conjunction(Negation(A), Conjunction(B, Constant(true)))),
      Negation(Implication(A, Disjunction(B, Constant(false)))))))
    println(ExprUtils.exprAsString(Task4.beautifyExpr(Conjunction(Negation(Conjunction(Negation(A), Conjunction(B, Constant(true)))),
      Negation(Implication(A, Disjunction(B, Constant(false))))))))
    assert(Task4.beatifyAndEvalExpr(Conjunction(Negation(Conjunction(Negation(A), Conjunction(B, Constant(true)))),
      Negation(Implication(A, Disjunction(B, Constant(false))))), variableToValue))
  }
}
