package ru.andri
package calculator.mathlex

import java.math.BigDecimal


case class NumberMathLex(lex: String) extends MathLex {
  private val lexType = MathLex.NUMBER

  val value: Double = parse(lex)

  override def getType: MathLex.Value = lexType

  private def parse(lex: String) = lex.toDouble

  override def toString: String = BigDecimal.valueOf(value).stripTrailingZeros.toString

}
