package utils

import com.typesafe.scalalogging.LazyLogging
import scala.util.parsing.combinator._
import utils.ExpressionParser.Validator


/**
 * http://bitwalker.org/blog/2013/08/10/learn-by-example-scala-parser-combinators/
 * http://debasishg.blogspot.com/2008/04/external-dsls-made-easy-with-scala.html
 *
 * ca_elem_1 is notEmpty
 * ca_elem_1 is required
 *
 * ca_elem_1 is required and maxlength of 4
 * ca_elem_1 is notNull and minlength of 3
 * ca_elem_1 is numeric or empty
 * ca_elem_1 has maxlength of 3, minvalue of 2
 *
 *
 * Created by johnbush on 10/9/14.
 */

object ExpressionParser extends ExpressionParser {
  type Validator = String => Boolean
}

class ExpressionParser extends JavaTokenParsers with LazyLogging {

  def is_has = "is" | "has" ~> "a" | "has" | "should" ~> "have" ~> "a" | "should" ~> "be" | "must" ~> "be" | "must" | "should"

  def required = is_has ~> "required" | is_has ~> "notEmpty" | is_has ~> "notNull" | is_has ~> "defined" | "!=" ~> "null"

  def empty = is_has ~> "empty" | is_has ~> "null" | is_has ~> "undefined" | is_has ~> "notDefined" |  "=" ~> "null"

  def equal = is_has ~> "=" | is_has ~> "equal" <~ "to" | is_has ~> "equal" | "="

  def notEqual = is_has ~> "<>" | is_has ~> "!=" | is_has ~> "notEqual" | "!="

  def greaterThan = is_has ~> ">" | is_has ~> "greater" ~> "than" | ">"

  def lessThan = is_has ~> "<" | is_has ~> "less" ~> "than" | "<"

  def greaterThanOrEqual = is_has ~> ">=" | is_has ~> "greater" ~> "than" ~> "or" ~> "equal" ~> "to" | ">="

  def lessThanOrEqual = is_has ~> "<=" | is_has ~> "less" ~> "than" ~> "or" ~> "equal" ~> "to" | "<="

  def maxlength = is_has ~> "maxlength" <~ "of"

  def minlength = is_has ~> "minlength" <~ "of"


  def predicate = (
    ident ~ minlength ~ wholeNumber ^^ {
      case field ~ f ~ i => Expression(field, getValidator(f.toString, Some(i.toInt)), f.toString)
    }
      | ident ~ equal ~ stringLiteral ^^ {
      case field ~ f ~ s => Expression(field, equalVal(stripQuotes(s)), f.toString)
    }
      | ident ~ equal ~ floatingPointNumber ^^ {
      case field ~ f ~ i => Expression(field, equalNumberVal(i.toFloat), f.toString)
    }
      | ident ~ notEqual ~ stringLiteral ^^ {
      case field ~ f ~ s => Expression(field, notEqualStringVal(stripQuotes(s)), f.toString)
    }
      | ident ~ notEqual ~ floatingPointNumber ^^ {
      case field ~ f ~ i => Expression(field, notEqualVal(i.toFloat), f.toString)
    }
      | ident ~ greaterThan ~ floatingPointNumber ^^ {
      case field ~ f ~ i => Expression(field, greaterThanVal(i.toFloat), f.toString)
    }
      | ident ~ lessThan ~ floatingPointNumber ^^ {
      case field ~ f ~ i => Expression(field, lessThanVal(i.toFloat), f.toString)
    }
      | ident ~ greaterThanOrEqual ~ floatingPointNumber ^^ {
      case field ~ f ~ i => Expression(field, greaterThanOrEqualVal(i.toFloat), f.toString)
    }
      | ident ~ lessThanOrEqual ~ floatingPointNumber ^^ {
      case field ~ f ~ i => Expression(field, lessThanOrEqualVal(i.toFloat), f.toString)
    }
      | ident ~ maxlength ~ wholeNumber ^^ {
      case field ~ f ~ i => Expression(field, getValidator(f.toString, Some(i.toInt)), f.toString)
    }
      | ident ~ required ^^ {
      case field ~ f => Expression(field, getValidator(f.toString), f.toString)
    }
      | ident ~ empty ^^ {
      case field ~ f => Expression(field, getValidator(f.toString), f.toString)
    }
    )

  def stripQuotes(s:String) = s.substring(1, s.length-1)

  def validationExpression:Parser[Where] = rep(clause) ^^ (Where(_:_*))



  def clause:Parser[Clause] = (predicate|parens|ifClause) * (
    "and" ^^^ { (a:Clause, b:Clause) => And(a,b) } |
      "or" ^^^ { (a:Clause, b:Clause) => Or(a,b) }
    )

  def parens:Parser[Clause] = "(" ~> clause  <~ ")"

  def ifClause:Parser[Clause] = "if" ~ clause ~ "then" ~ clause ^^ {case "if" ~ a ~ "then" ~ b => new If(a,b)}

  def parse(expr: String): Option[Where] = {
    parseAll(validationExpression, expr) match {
      case Success(r, q) => Option(r)
      case x => println(x); None
    }
  }

  def parseSingleExpr(expr: String): Option[Expression] = {
    parseAll(predicate, expr) match {
      case Success(r, q) => Option(r)
      case x => println(x); None
    }
  }


  def evaluate(clause: Clause, fieldMap: Map[String, String]): Boolean = {
    clause match {
      case expr: Expression => {
        val fieldName = expr.fieldName
        val fieldValue = fieldMap.get(fieldName).get
        val funcName = expr.validatorName
        val result = expr.validator(fieldValue)
        logger.debug(s"validator [$funcName] evaluated [$fieldName] with value of [$fieldValue] to [$result]")
        result
      }
      case and: And => evaluate(and.lClause, fieldMap) match {
        // short circuit, if the left is false we are done
        case false => false
        case true => evaluate(and.rClause, fieldMap)
      }
      case or: Or => evaluate(or.lClause, fieldMap) match {
        // short circuit, if the left is true we are done
        case true => true
        case false => evaluate(or.rClause, fieldMap)
      }
      case clause: If => evaluate(clause.ifClause, fieldMap) match {
        // all if evaluate to true if the if doesn't pass, this avoids needing else statements
        case false => true
        case true => evaluate(clause.thenClause, fieldMap)
      }
    }
  }

  def getValidator(name: String, param: Option[Int] = None): Validator = {
    name match {
      case "required" | "notEmpty" | "notNull" | "defined" => requiredVal
      case "minlength" => minlengthVal(param.get)
      case "maxlength" => maxlengthVal(param.get)
      case "empty" | "null" | "undefined" | "notDefined" => emptyVal
      case _ => throw new RuntimeException("no function to bind with name: " + name)
    }
  }

  def minlengthVal(min: Int)(value: String): Boolean = value != null && value.length >= min

  def maxlengthVal(max: Int)(value: String): Boolean = value == null || value.length <= max


  def emptyVal(value: String): Boolean = value == null || value.isEmpty

  def equalNumberVal(exprValue: Float)(inputValue: String):   Boolean = {
    logger.debug(s"equalNumberVal($exprValue,$inputValue)")
    inputValue.toFloat == exprValue
  }

  def equalVal(exprValue: String)(inputValue: String): Boolean = {
    logger.debug(s"equalVal($exprValue,$inputValue)")
    inputValue.equals(exprValue)
  }

  def notEqualStringVal(exprValue: String)(inputValue: String): Boolean = !inputValue.equals(exprValue)

  def notEqualVal(exprValue: Float)(inputValue: String): Boolean = inputValue.toFloat != exprValue

  def greaterThanVal(exprValue: Float)(inputValue: String): Boolean = inputValue.toFloat > exprValue

  def lessThanVal(exprValue: Float)(inputValue: String): Boolean = inputValue.toFloat < exprValue

  def greaterThanOrEqualVal(exprValue: Float)(inputValue: String): Boolean = inputValue.toFloat >= exprValue

  def lessThanOrEqualVal(exprValue: Float)(inputValue: String): Boolean = inputValue.toFloat <= exprValue

  def requiredVal(value: String): Boolean = value != null && !value.isEmpty


}

case class If(val ifClause:Clause, val thenClause: Clause ) extends Clause

case class Expression(fieldName: String, validator: Validator, validatorName: String) extends Clause

