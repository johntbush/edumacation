package platform.fabric

import utils.{Where, SQLParser}

/**
 * Created by johnbush on 7/31/14.
 */
class SqlParserTest extends org.scalatest.WordSpec with org.scalatest.MustMatchers with org.scalatest.OptionValues {


  "parse and/or with parens" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("(name = \"peter\" and field2 = 5) or field6 = 10").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse data with new lines in expression" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("name = \"peter\" and \n field2 \n= 5").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse data with new lines (Java style) in string literals" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("name = \"peter\\n and paul\" and field2 = 5").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse data with new lines in string literals" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("name = \"peter\n and paul\" and field2 = 5").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }


  "parse like clause" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("field1 like \"%yourmom%\"").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }


  "parse IN clauses with numbers" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("field1 in (1,2,345)").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse clause with % in column" in  {
    val p = new SQLParser
    val query: Where = p.parseWhere("frght_bl__%t010 = \"Parts Center\"").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)

  }

  "parse clause with % in column inclosed in brackets" in  {
    val p = new SQLParser
    val query: Where = p.parseWhere("frght_bl__[%t010] = \"Parts Center\"").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)

  }


  "parse IN clauses with numbers and white space" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("field1 in ( 1 , 2,   345  )").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse IN clauses with strings in double quotes" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("field1 in (\"123\",\"2\",\"3\")").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse IN clauses with strings in single quotes" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("field1 in ('a','b','cde123@#$!@#$')").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }


  "parse not equals for strings" in {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 != \"asdf\"").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse not equals for numbers" in {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 != 10").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse not equals for booleans" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 != false").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse greater than for strings" in {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 > \"asdf\"").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse greater than for numbers" in {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 > 10").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse greater than for booleans" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 > false").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse less than for strings" in {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 < \"asdf\"").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse less than for numbers" in {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 < 10").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse less than for booleans" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 < false").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse less than or equals for strings" in {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 <= \"asdf\"").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse less than or equals for numbers" in {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 <= 10").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse less than or equals for booleans" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 <= false").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }



  "parse greater than or equals for strings" in {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 >= \"asdf\"").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse greater than or equals for numbers" in {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 >= 10").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }

  "parse greater than or equals for booleans" in  {

    val p = new SQLParser
    val query: Where = p.parseWhere("field6 >= false").get

    println("predicate clause: " + utils.AnsiSqlRenderer.expandPredicate(query))

    assert(true)
  }
}


