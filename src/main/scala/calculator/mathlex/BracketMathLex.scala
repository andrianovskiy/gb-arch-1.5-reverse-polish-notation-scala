package ru.andri
package calculator.mathlex

import calculator.mathlex.BracketMathLex.{BRACKET_TYPE, SYMBOL_CLOSE_BRACKET, SYMBOL_OPEN_BRACKET}


case class BracketMathLex(lex: String) extends MathLex {
  private val lexType = MathLex.BRACKET
  private val bracketType = parse(lex)


  override def getType: MathLex.Value = lexType

  private def parse(lex: String): BRACKET_TYPE.Value = {
    lex match {
      case SYMBOL_OPEN_BRACKET =>
        BRACKET_TYPE.OPEN_BRACKET
      case SYMBOL_CLOSE_BRACKET =>
        BRACKET_TYPE.CLOSE_BRACKET
      case _ =>
        throw new IllegalArgumentException("Ошибка! Некорректный оператор " + lex)
    }
  }

  def isBracketType(bracketType: BRACKET_TYPE.Value): Boolean = this.bracketType eq bracketType

  def isOpenBracket: Boolean = isBracketType(BRACKET_TYPE.OPEN_BRACKET)

  override def toString: String = bracketType.symbol

}

object BracketMathLex {
  val SYMBOL_OPEN_BRACKET = "("
  val SYMBOL_CLOSE_BRACKET = ")"
  val bracketRegExp = "[()]"

  object BRACKET_TYPE extends Enumeration {
    protected case class BracketVal(symbol: String) extends super.Val {
    }

    import scala.language.implicitConversions

    implicit def valueToBracketVal(x: Value): BracketVal = x.asInstanceOf[BracketVal]

    val OPEN_BRACKET: BracketVal = BracketVal(SYMBOL_OPEN_BRACKET)
    val CLOSE_BRACKET: BracketVal = BracketVal(SYMBOL_CLOSE_BRACKET)
  }
}