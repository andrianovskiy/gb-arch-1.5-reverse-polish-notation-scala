package ru.andri
package calculator

import calculator.ReversePolishNotation.{REG_EXP, SPACE}
import calculator.mathlex.{BracketMathLex, MathLex, NumberMathLex, TupleFunctionMathLex}

import scala.collection.mutable


class ReversePolishNotation(lex: String) {

  val lexemes: mutable.Queue[MathLex] = convertToReversePolish(parse(lex))

  private def parse(expression: String): List[MathLex] = {
    if (expression.isBlank) throw new IllegalArgumentException("Ошибка! Пустая строка")

    expression.split(REG_EXP)
      .map(_.stripLeading)
      .map(stringToMathLex)
      .toList
  }

  private def stringToMathLex(lex: String): MathLex = {
    if (lex.matches(TupleFunctionMathLex.tupleFuncRegExp)) TupleFunctionMathLex(lex)
    else if (lex.matches(BracketMathLex.bracketRegExp)) BracketMathLex(lex)
    else NumberMathLex(lex)
  }

  private def convertToReversePolish(lexemes: List[MathLex]): mutable.Queue[MathLex] = {
    val (res, stack, _) = lexemes.foldLeft((new mutable.Queue[MathLex](), new mutable.Stack[MathLex](), lexemes.head)) {
      case ((res, stack, prevLex), mathLex) =>
        mathLex match {
          case _: NumberMathLex => (res.enqueue(mathLex), stack, mathLex)
          case bracketLex: BracketMathLex => bracketLex match {
            case _ if bracketLex.isOpenBracket => (res, stack.push(mathLex), mathLex)
            case _ => pushUntilNotOpenBracket(res, stack, mathLex, prevLex)
          }
          case _: TupleFunctionMathLex =>
            if (checkUnaryExpression(prevLex, mathLex)) res :+ NumberMathLex("0")
            pushUntilFunc(res, stack, mathLex, prevLex)
        }
    }
    pushStackToReversePolishLexemes(res, stack)
  }

  private def checkUnaryExpression(prevLex: MathLex, currLex: MathLex) = prevLex.isType(MathLex.BRACKET) && currLex.isType(MathLex.TUPLE_FUNC) && currLex.asInstanceOf[TupleFunctionMathLex].isCanBeUnaryOperator

  private def pushUntilNotOpenBracket(res: mutable.Queue[MathLex], stack: mutable.Stack[MathLex], currLex: MathLex, prevLex: MathLex) = {
    if (prevLex.isType(MathLex.TUPLE_FUNC)) {
      throw new RuntimeException("Ошибка! Некорректное выражение")
    }
    while (stack.isEmpty || !(stack.top.isType(MathLex.BRACKET) && stack.pop().asInstanceOf[BracketMathLex].isOpenBracket)) {
      if (stack.isEmpty) throw new RuntimeException("Ошибка! В выражении либо неверно поставлен разделитель, либо не согласованы скобки")

      res.enqueue(stack.pop())
    }

    (res, stack, currLex)
  }

  private def pushUntilFunc(res: mutable.Queue[MathLex], stack: mutable.Stack[MathLex], currLex: MathLex, prevLex: MathLex) = {
    if (prevLex.isType(MathLex.TUPLE_FUNC)) {
      throw new RuntimeException("Ошибка! Некорректное выражение")
    }

    res.enqueueAll(stack.popWhile(mathLex => !(mathLex.isType(MathLex.BRACKET) || mathLex.asInstanceOf[TupleFunctionMathLex].compareTo(currLex.asInstanceOf[TupleFunctionMathLex]) < 0)))

    (res, stack.push(currLex), currLex)
  }

  private def pushStackToReversePolishLexemes(res: mutable.Queue[MathLex], stack: mutable.Stack[MathLex]) = {
    while (stack.nonEmpty) {
      if (stack.top.isType(MathLex.BRACKET)) throw new RuntimeException("Ошибка! Некорректное выражение")
      res.enqueue(stack.pop())
    }
    res
  }

  def calculate: Double = {
    lexemes.foldLeft(new mutable.Stack[Double])((stack, mathLex) => {
      mathLex match {
        case numMathLex: NumberMathLex => stack.push(numMathLex.value)
        case _ if stack.size < 2 => throw new RuntimeException("Ошибка! Некорректное выражение")
        case tupleFuncLex: TupleFunctionMathLex => stack.push(tupleFuncLex.calc(stack.pop(), stack.pop()))

      }
    }).pop()
  }

  override def toString: String = {
    lexemes.map(_.toString).reduce((str, lex) => str.concat(SPACE).concat(lex))
  }

}


object ReversePolishNotation {
  val REG_EXP = "((?<=[-+*/()])|(?=[-+*/()]))"
  val SPACE = " "
}