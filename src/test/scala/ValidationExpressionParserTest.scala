import utils.{Expression, ExpressionParser, Where}

/**
 * Created by johnbush on 10/10/14.
 */
class ValidationExpressionParserTest extends org.scalatest.WordSpec with org.scalatest.MustMatchers with org.scalatest.OptionValues {

  "parse expressions" in {

    val exprs = List(
      "ca_elem_1 has maxlength of 5",
      "ca_elem_1 should have a maxlength of 5",
      "ca_elem_1 has a maxlength of 5",
      "ca_elem_1 is maxlength of 5",
      "ca_elem_1 has minlength of 55",
      "ca_elem_1 should have a minlength of 56",
      "ca_elem_1 has a minlength of 57",
      "ca_elem_1 is minlength of 58",
      "ca_elem_1 is notEmpty",
      "ca_elem_1 is empty",
      "ca_elem_1 should be empty",
      "ca_elem_1 must be equal to 5.6",
      "ca_elem_1 must equal to 5",
      "ca_elem_1 should = 5",
      "ca_elem_1 should be > -1",
      "ca_elem_1 should be > 1.3",
      "ca_elem_1 should be <= -1.2",
      "ca_elem_1 <= -1.2",
      "ca_elem_1 >= -1.2",
      "ca_elem_1 < -1.2",
      "ca_elem_1 > -1.2",
      "ca_elem_1 should be <= 0",
      "ca_elem_1 should be >= -1.2",
      "ca_elem_1 should be >= 0",
      "ca_elem_1 should be greater than 5",
      "ca_elem_1 should be greater than or equal to 5",
      "ca_elem_1 should be less than or equal to 5",
      "ca_elem_1 should be less than 5",
      "ca_elem_1 must notEqual 5",
      "ca_elem_1 must != 5",
      "ca_elem_1 must <> 5",
      "ca_elem_1 is notDefined",
      "ca_elem_1 is defined",
      "ca_elem_1 is undefined",
      "ca_elem_1 is notNull",
      "ca_elem_1 must be null",
      "ca_elem_1 = null",
      "ca_elem_1 = \"X\"",
      "ca_elem_1 != null",
      "ca_elem_1 is null",
      "ca_elem_1 is required")

    exprs.foreach(e => validateExpr(e))
  }


  "evaluate required" in {
    val expr: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 is required").get
    assert(!expr.validator(null))
    assert(!expr.validator(""))
    assert(expr.validator("X"))
  }

  "evaluate equals" in {
    val expr1: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 should equal 5").get
    val expr2: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 should = 5").get
    val expr3: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 must be equal to 5").get
    val expr4: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 must = 5").get
    val expr5: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 must be equal to \"QWERTY\"").get

    assert(expr1.validator("5"))
    assert(expr2.validator("5"))
    assert(expr3.validator("5"))
    assert(expr4.validator("5"))
    assert(expr5.validator("QWERTY"))

  }

  "evaluate not equals" in {
    val expr1: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 should notEqual 5").get
    val expr2: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 should != 5").get
    val expr3: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 must <> 5").get
    val expr4: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 must != 5").get
    val expr5: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 != 5").get
    val expr6: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 != \"5\"").get

    assert(!expr1.validator("5"))
    assert(expr2.validator("6"))
    assert(expr3.validator("7"))
    assert(expr4.validator("8"))
    assert(expr5.validator("8"))
    assert(expr6.validator("8"))


  }

  "evaluate less than" in {
    val expr1: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 should be < 5").get
    val expr2: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 should be less than 5").get
    val expr3: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 must <= 5").get
    val expr4: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 must be less than or equal to 5").get
    val expr5: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 < 5").get
    val expr6: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 <= 5").get

    assert(!expr1.validator("5"))
    assert(expr2.validator("1"))
    assert(expr3.validator("5"))
    assert(expr4.validator("2"))
    assert(expr5.validator("2"))
    assert(expr6.validator("2"))

  }

  "evaluate greater than" in {
    val expr1: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 should be > 5").get
    val expr2: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 should be greater than 5").get
    val expr3: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 must >= 5").get
    val expr4: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 must be greater than or equal to 5").get
    val expr5: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 >= 5").get
    val expr6: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 > 5").get


    assert(!expr1.validator("5"))
    assert(expr2.validator("6"))
    assert(expr3.validator("5"))
    assert(expr4.validator("10.11"))
    assert(expr5.validator("10.11"))
    assert(expr6.validator("10.11"))

  }


  "evaluate notEmpty" in {
    val expr2: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 is notEmpty").get
    assert(!expr2.validator(null))
    assert(!expr2.validator(""))
    assert(expr2.validator("X"))
  }

  "evaluate empty" in {
    val expr: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 is empty").get
    assert(expr.validator(null))
    assert(expr.validator(""))
    assert(!expr.validator("X"))
  }

  "evaluate minlength for strings" in {
    val expr: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 has minlength of 2").get
    assert(expr.validator("XXX"))
    assert(expr.validator("XX"))
    assert(!expr.validator("X"))
  }



  "evaluate minlength" in {
    val expr: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 has minlength of 5").get
    assert(!expr.validator(null))
    assert(!expr.validator(""))
    assert(!expr.validator("X"))
    assert(expr.validator("XXXXX"))
    assert(expr.validator("XXXXXX"))
  }

  "evaluate maxlength" in {
    val expr: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 should have a maxlength of 5").get
    assert(expr.validator(null))
    assert(expr.validator(""))
    assert(expr.validator("X"))
    assert(expr.validator("XXXXX"))
    assert(!expr.validator("XXXXXX"))
  }

  "evaluate maxlength for strings" in {
    val expr: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 should have a maxlength of 2").get

    assert(expr.validator("X"))
    assert(expr.validator("XX"))
    assert(!expr.validator("XXXXX"))
  }


  "apply field name lookup to evaluation" in {
    val expr1: Expression = ExpressionParser.parseSingleExpr("ca_elem_1 should have a maxlength of 5").get
    val expr2: Expression = ExpressionParser.parseSingleExpr("ca_elem_2 should have a maxlength of 5").get
    val expr3: Expression = ExpressionParser.parseSingleExpr("ca_elem_3 should have a maxlength of 5").get
    val expr4: Expression = ExpressionParser.parseSingleExpr("ca_elem_4 should have a maxlength of 5").get
    val expr5: Expression = ExpressionParser.parseSingleExpr("ca_elem_5 should have a maxlength of 5").get

    val fieldMap = Map(
      "ca_elem_1" -> null,
      "ca_elem_2" -> "",
      "ca_elem_3" -> "X",
      "ca_elem_4" -> "XXXXX",
      "ca_elem_5" -> "XXXXXX")


    assert(expr1.validator(fieldMap.get(expr1.fieldName).get))
    assert(expr2.validator(fieldMap.get(expr2.fieldName).get))
    assert(expr3.validator(fieldMap.get(expr3.fieldName).get))
    assert(expr4.validator(fieldMap.get(expr4.fieldName).get))
    assert(!expr5.validator(fieldMap.get(expr5.fieldName).get))

  }

  "evaluate a bunch of added expressions" in {
    val expr: Where = ExpressionParser.parse("( ca_elem_1 should have a maxlength of 5 and ca_elem_2 should have a maxlength of 5 and ca_elem_3 should have a maxlength of 5 and ca_elem_4 should have a maxlength of 5 and ca_elem_5 should have a minlength of 5 )").get
    val fieldMap = Map(
      "ca_elem_1" -> null,
      "ca_elem_2" -> "",
      "ca_elem_3" -> "X",
      "ca_elem_4" -> "XXXXX",
      "ca_elem_5" -> "XXXXXX")
    assert(ExpressionParser.evaluate(expr.clauses(0), fieldMap))
  }

  "evaluate expressions using or" in {
    val expr: Where = ExpressionParser.parse("ca_elem_1 is required or ca_elem_2 should have a maxlength of 5 ").get
    val fieldMap = Map(
      "ca_elem_1" -> null,
      "ca_elem_2" -> "",
      "ca_elem_3" -> "X",
      "ca_elem_4" -> "XXXXX",
      "ca_elem_5" -> "XXXXXX")
    assert(ExpressionParser.evaluate(expr.clauses(0), fieldMap))
  }

  "evaluate expressions using or,and" in {
    val expr: Where = ExpressionParser.parse("( ca_elem_1 is empty or ca_elem_2 should have a maxlength of 5 ) and ca_elem_5 is required").get
    val fieldMap = Map(
      "ca_elem_1" -> null,
      "ca_elem_2" -> "",
      "ca_elem_3" -> "X",
      "ca_elem_4" -> "XXXXX",
      "ca_elem_5" -> "XXXXXX")
    assert(ExpressionParser.evaluate(expr.clauses(0), fieldMap))
  }


  def validateExpr(exprStr: String) = {
    try {
      val expr: Expression = ExpressionParser.parseSingleExpr(exprStr).get
      println(expr)

      assert(true)

      true
    } catch {
      case e: Exception => {
        assert(false, s"failed to parse: $exprStr " + e.getMessage)
        false
      }
    }
  }


  "parse if/then" in {

    val expr1: Where = ExpressionParser.parse("if ca_elem_1 = null then ca_elem_3 = \"X\"").get

    val expr2: Where = ExpressionParser.parse("if (ca_elem_1 = null) then ca_elem_2 is required").get

    val expr3: Where = ExpressionParser.parse("if (ca_elem_3 != null) then (ca_elem_4 is required)").get

    val expr4: Where = ExpressionParser.parse("if (ca_elem_1 != null or ca_elem_2 > 5) then (ca_elem_2 is required and ca_elem_1 < 5)").get

    val expr5: Where = ExpressionParser.parse("if (ca_elem_1 != null or if ca_elem_2 > 5 then ca_elem_4 > 3) then (ca_elem_2 is required and ca_elem_1 < 5)").get

    val fieldMap = Map(
      "ca_elem_1" -> null,
      "ca_elem_2" -> "",
      "ca_elem_3" -> "X",
      "ca_elem_4" -> "XXXXX",
      "ca_elem_5" -> "XXXXXX")

    assert(ExpressionParser.evaluate(expr1.clauses(0), fieldMap))
    assert(!ExpressionParser.evaluate(expr2.clauses(0), fieldMap))
    assert(ExpressionParser.evaluate(expr3.clauses(0), fieldMap))

  }
}
