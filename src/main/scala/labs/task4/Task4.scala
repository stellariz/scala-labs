package labs.task4

sealed trait Expr

final case class Constant(value: Boolean) extends Expr

final case class Variable(value: String) extends Expr

final case class Negation(value: Expr) extends Expr

final case class Conjunction(lhs: Expr, rhs: Expr) extends Expr

final case class Disjunction(lhs: Expr, rhs: Expr) extends Expr

final case class Implication(lhs: Expr, rhs: Expr) extends Expr

object Task4 {

  // 1-st step - remove implication
  private def removeImplication(expr: Expr): Expr = expr match {
    case Constant(_) => expr
    case Variable(_) => expr
    case Negation(value) => Negation(removeImplication(value))
    case Conjunction(lhs, rhs) => Conjunction(removeImplication(lhs), removeImplication(rhs))
    case Disjunction(lhs, rhs) => Disjunction(removeImplication(lhs), removeImplication(rhs))
    case Implication(lhs, rhs) => Disjunction(Negation(removeImplication(lhs)), removeImplication(rhs))
    case _ => throw new IllegalArgumentException("Illegal boolean expression")
  }

  // 2-nd and 3-rd step put negation inside in expr and remove double negation
  private def applyNegation(expr: Expr): Expr = expr match {
    case Constant(_) => expr
    case Variable(_) => expr
    case Negation(value) => value match {
      case Constant(_) => expr
      case Variable(_) => expr
      case Conjunction(lhs, rhs) => Disjunction(applyNegation(Negation(lhs)), applyNegation(Negation(rhs)))
      case Disjunction(lhs, rhs) => Conjunction(applyNegation(Negation(lhs)), applyNegation(Negation(rhs)))
      // here is removing double negation
      case Negation(value2) => applyNegation(value2)
      case _ => throw new IllegalArgumentException("Illegal boolean expression")
    }
    case Conjunction(lhs, rhs) => Conjunction(applyNegation(lhs), applyNegation(rhs))
    case Disjunction(lhs, rhs) => Disjunction(applyNegation(lhs), applyNegation(rhs))
    case _ => throw new IllegalArgumentException("Illegal boolean expression")
  }


  // 4-th step - simplify expression
  private def simplifyExpr(expr: Expr): Expr = expr match {
    case Constant(_) => expr
    case Variable(_) => expr
    case Negation(value) => value match {
      case Constant(value) => Constant(!value)
      case Variable(_) => expr
      case Negation(expr) => simplifyExpr(expr)
      case Conjunction(_, _) => Negation(simplifyExpr(value))
      case Disjunction(_, _) => Negation(simplifyExpr(value))
      case _ => throw new IllegalArgumentException("Illegal boolean expression")
    }
    case Conjunction(lhs, rhs) => (simplifyExpr(lhs), simplifyExpr(rhs)) match {
      case (simplifiedLhs, Constant(value)) => if (value) simplifiedLhs else Constant(value)
      case (Constant(value), simplifiedRhs) => if (value) simplifiedRhs else Constant(value)
      case (simplifiedLhs, simplifiedRhs) => Conjunction(simplifiedLhs, simplifiedRhs)
    }
    case Disjunction(lhs, rhs) => (simplifyExpr(lhs), simplifyExpr(rhs)) match {
      case (simplifiedLhs, Constant(value)) => if (value) Constant(value) else simplifiedLhs
      case (Constant(value), simplifiedRhs) => if (value) Constant(value) else simplifiedRhs
      case (simplifiedLhs, simplifiedRhs) => Disjunction(simplifiedLhs, simplifiedRhs)
    }
    case _ => throw new IllegalArgumentException("Illegal boolean expression")
  }

  private def eval(expr: Expr, variableToValue: Map[String, Boolean]): Boolean = expr match {
    case Constant(value) => value
    case Variable(varName) => variableToValue.apply(varName)
    case Negation(value) => !eval(value, variableToValue)
    case Conjunction(lhs, rhs) => eval(lhs, variableToValue) && eval(rhs, variableToValue)
    case Disjunction(lhs, rhs) => eval(lhs, variableToValue) || eval(rhs, variableToValue)
    case _ => throw new IllegalArgumentException("Illegal boolean expression")
  }

  def beautifyExpr(expr: Expr): Expr = {
    val exprWithoutImplication = removeImplication(expr);
    val exprWithInnerNegation = applyNegation(exprWithoutImplication)
    simplifyExpr(exprWithInnerNegation)
  }

  def evalExpr(expr: Expr, variableToValue: Map[String, Boolean]): Boolean = {
    eval(expr, variableToValue)
  }

  def beatifyAndEvalExpr(expr: Expr, variableToValue: Map[String, Boolean]): Boolean = {
    evalExpr(beautifyExpr(expr), variableToValue)
  }

  /*
    1. Get expression
    2. Transform expression (same as compile) to form which easy to eval
    3. Transform expression to DNF
    4. Eval
   */
  def main(args: Array[String]): Unit = {
  }
}
