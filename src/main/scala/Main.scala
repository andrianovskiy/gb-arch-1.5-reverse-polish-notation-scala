package ru.andri

import calculator.ReversePolishNotation

import java.math.BigDecimal


object Main extends App {

  private val testExpression = "(1+  2)*4+3"

  val rpn = new ReversePolishNotation(getExpression)
  println(rpn)
  println(BigDecimal.valueOf(rpn.calculate).stripTrailingZeros)


  def getExpression: String = {
    var expression = ""
    if (args.length < 1) {
      expression = testExpression
      println("Выражение не передано. Расчет будет произведен для тестового выражения")
    }
    else {
      println("Выражение прочитано из аргументов командной строки")
      expression = args(0)
    }

    println(expression)

    expression
  }

}
