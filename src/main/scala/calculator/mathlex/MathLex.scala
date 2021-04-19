package ru.andri
package calculator.mathlex


trait MathLex {

  def getType: MathLex.Value

  def isType(lexType: MathLex.Value): Boolean = getType.compareTo(lexType) == 0

}

object MathLex extends Enumeration {
  val TUPLE_FUNC, BRACKET, NUMBER = Value
}