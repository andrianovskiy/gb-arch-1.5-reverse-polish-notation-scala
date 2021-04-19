package ru.andri
package calculator.mathlex

import calculator.mathlex.TupleFunctionMathLex.{SYMBOL_ADD, SYMBOL_DIVIDE, SYMBOL_MULTIPLY, SYMBOL_SUBTRACT, TUPLE_FUNC_TYPE}


case class TupleFunctionMathLex(lex: String) extends MathLex {
  private val lexType = MathLex.TUPLE_FUNC
  private val tupleFuncType = parse(lex)


  override def getType: MathLex.Value = lexType

  private def parse(lex: String): TUPLE_FUNC_TYPE.Value = {
    lex match {
      case SYMBOL_ADD =>
        TUPLE_FUNC_TYPE.ADD
      case SYMBOL_SUBTRACT =>
        TUPLE_FUNC_TYPE.SUBTRACT
      case SYMBOL_MULTIPLY =>
        TUPLE_FUNC_TYPE.MULTIPLY
      case SYMBOL_DIVIDE =>
        TUPLE_FUNC_TYPE.DIVIDE
      case _ =>
        throw new IllegalArgumentException("Ошибка! Некорректный оператор " + lex)
    }
  }

  def calc(dig1: Double, dig2: Double): Double = tupleFuncType match {
    case TUPLE_FUNC_TYPE.ADD =>
      dig2 + dig1
    case TUPLE_FUNC_TYPE.SUBTRACT =>
      dig2 - dig1
    case TUPLE_FUNC_TYPE.MULTIPLY =>
      dig2 * dig1
    case TUPLE_FUNC_TYPE.DIVIDE =>
      dig2 / dig1
    case _ =>
      throw new RuntimeException
  }

  def isCanBeUnaryOperator: Boolean = (tupleFuncType eq TUPLE_FUNC_TYPE.ADD) || (tupleFuncType eq TUPLE_FUNC_TYPE.SUBTRACT)

  def compareTo(tupleFunc: TupleFunctionMathLex): Int = this.tupleFuncType.priority.compareTo(tupleFunc.tupleFuncType.priority)

  override def toString: String = tupleFuncType.symbol

}

object TupleFunctionMathLex {
  val SYMBOL_ADD = "+"
  val SYMBOL_SUBTRACT = "-"
  val SYMBOL_MULTIPLY = "*"
  val SYMBOL_DIVIDE = "/"
  val tupleFuncRegExp = "[-+*/]"

  object TUPLE_FUNC_TYPE extends Enumeration {
    protected case class TupleFuncVal(symbol: String, priority: Int) extends super.Val {
    }

    import scala.language.implicitConversions

    implicit def valueToTupleFuncVal(x: Value): TupleFuncVal = x.asInstanceOf[TupleFuncVal]

    val ADD: TupleFuncVal = TupleFuncVal(SYMBOL_ADD, 1)
    val SUBTRACT: TupleFuncVal = TupleFuncVal(SYMBOL_SUBTRACT, 1)
    val MULTIPLY: TupleFuncVal = TupleFuncVal(SYMBOL_MULTIPLY, 2)
    val DIVIDE: TupleFuncVal = TupleFuncVal(SYMBOL_DIVIDE, 2)
  }
}