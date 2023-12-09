package labs.task4

object ExprUtils {

  def exprAsString(expr: Expr): String = expr match {
    case Variable(value) => value
    case Constant(value) => value.toString
    case Negation(value) => "~(" + exprAsString(value) + ")"
    case Conjunction(lhs, rhs) => "(" + exprAsString(lhs) + " & " + exprAsString(rhs) + ")"
    case Disjunction(lhs, rhs) => "(" + exprAsString(lhs) + " | " + exprAsString(rhs) + ")"
    case Implication(lhs, rhs) => "(" + exprAsString(lhs) + " -> " + exprAsString(rhs) + ")"
  }

}
