package utils

import scala.util.parsing.combinator._

class SQLParser extends JavaTokenParsers {

  override def stringLiteral: Parser[String] =
    (""""[^"\\]*(?:\\.[^"\\]*)*"""").r


  /** Anything that is a valid Java identifier, according to
    * <a href="http://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.8">The Java Language Spec</a>.
    * Generally, this means a letter, followed by zero or more letters or numbers.
    */
  override def ident: Parser[String] =
  //       """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r
  //    ("""\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""").r

    ("""\[?\p{javaJavaIdentifierStart}[\p{javaJavaIdentifierPart}%\[]*\]?""").r


  def query:Parser[Query] = operation ~ from ~ opt(where) ~ opt(order) ^^ {
    case operation ~ from ~ where ~ order => Query(operation, from, where, order)
  }


  def operation:Parser[Operation] = {
    ("select" | "update" | "delete") ~ repsep(ident, ",") ^^ {
      case "select" ~ f => Select(f:_*)
      case _ => throw new IllegalArgumentException("Operation not implemented")
    }
  }

  def from:Parser[From] = "from" ~> ident ^^ (From(_))

  def where:Parser[Where] = "where" ~> rep(clause) ^^ (Where(_:_*))

  def wherePred:Parser[Where] = rep(clause) ^^ (Where(_:_*))

  def clause:Parser[Clause] = (predicate|parens) * (
    "and" ^^^ { (a:Clause, b:Clause) => And(a,b) } |
      "or" ^^^ { (a:Clause, b:Clause) => Or(a,b) }
    )

  def parens:Parser[Clause] = "(" ~> clause  <~ ")"

  def predicate = (
    ident ~ "=" ~ boolean ^^ { case f ~ "=" ~ b => BooleanEquals(f,b)}
      | ident ~ "like" ~ stringLiteral ^^ { case f ~ "like" ~ v => Like(f,stripQuotes(v))}

      | ident ~ "=" ~ stringLiteral ^^ { case f ~ "=" ~ v => StringEquals(f,stripQuotes(v))}
      | ident ~ "=" ~ wholeNumber ^^ { case f ~ "=" ~ i => NumberEquals(f,i.toInt)}

      | ident ~ "!=" ~ boolean ^^ { case f ~ "!=" ~ b => BooleanNotEquals(f,b)}
      | ident ~ "!=" ~ stringLiteral ^^ { case f ~ "!=" ~ v => StringNotEquals(f,stripQuotes(v))}
      | ident ~ "!=" ~ wholeNumber ^^ { case f ~ "!=" ~ i => NumberNotEquals(f,i.toInt)}

      | ident ~ ">" ~ boolean ^^ { case f ~ ">" ~ b => BooleanGreaterThan(f,b)}
      | ident ~ ">" ~ stringLiteral ^^ { case f ~ ">" ~ v => StringGreaterThan(f,stripQuotes(v))}
      | ident ~ ">" ~ wholeNumber ^^ { case f ~ ">" ~ i => NumberGreaterThan(f,i.toInt)}

      | ident ~ "<" ~ boolean ^^ { case f ~ "<" ~ b => BooleanLessThan(f,b)}
      | ident ~ "<" ~ stringLiteral ^^ { case f ~ "<" ~ v => StringLessThan(f,stripQuotes(v))}
      | ident ~ "<" ~ wholeNumber ^^ { case f ~ "<" ~ i => NumberLessThan(f,i.toInt)}

      | ident ~ ">=" ~ boolean ^^ { case f ~ ">=" ~ b => BooleanGreaterThanEquals(f,b)}
      | ident ~ ">=" ~ stringLiteral ^^ { case f ~ ">=" ~ v => StringGreaterThanEquals(f,stripQuotes(v))}
      | ident ~ ">=" ~ wholeNumber ^^ { case f ~ ">=" ~ i => NumberGreaterThanEquals(f,i.toInt)}

      | ident ~ "<=" ~ boolean ^^ { case f ~ "<=" ~ b => BooleanLessThanEquals(f,b)}
      | ident ~ "<=" ~ stringLiteral ^^ { case f ~ "<=" ~ v => StringLessThanEquals(f,stripQuotes(v))}
      | ident ~ "<=" ~ wholeNumber ^^ { case f ~ "<=" ~ i => NumberLessThanEquals(f,i.toInt)}

      | ident ~ "in" ~ inClauseNumber ^^ { case f ~ "in" ~ v => In(f, v:_*)}
      | ident ~ "in" ~ inClauseStringDoubleQuotes ^^ { case f ~ "in" ~ v => In(f, v:_*)}
      | ident ~ "in" ~ inClauseStringSingleQuotes ^^ { case f ~ "in" ~ v => In(f, v:_*)}

    )

  def boolean = ("true" ^^^ (true) | "false" ^^^ (false))

  def stringLiteralSingleQuotes: Parser[String] =
    ("""('[^'\\]*(?:\\.[^'\\]*)*')""").r


  def inClauseNumber: Parser[List[String]] = "(" ~> wholeNumber ~ rep("," ~> wholeNumber)  <~ ")" ^^ {
    case s => s._2.::(s._1)
  }

  def inClauseStringDoubleQuotes: Parser[List[String]] = "(" ~> stringLiteral ~ rep("," ~> stringLiteral)  <~ ")" ^^ {
    case s => s._2.::(s._1)
  }

  def inClauseStringSingleQuotes: Parser[List[String]] = "(" ~> stringLiteralSingleQuotes ~ rep("," ~> stringLiteralSingleQuotes)  <~ ")" ^^ {
    case s => s._2.::(s._1)
  }

  def order:Parser[Direction] = {
    "order" ~> "by" ~> ident  ~ ("asc" | "desc") ^^ {
      case f ~ "asc" => Asc(f)
      case f ~ "desc" => Desc(f)
    }
  }

  def stripQuotes(s:String) = s.substring(1, s.length-1)

  def parse(sql:String):Option[Query] = {
    parseAll(query, sql) match {
      case Success(r, q) => Option(r)
      case x => println(x); None
    }
  }


  def parseWhere(sql:String):Option[Where] = {
    parseAll(wherePred, sql) match {
      case Success(r, q) => Option(r)
      case x: ParseResult[Where] => throw new RuntimeException(x.toString)
    }
  }
}

case class Query(val operation:Operation, val from: From, val where: Option[Where], val order: Option[Direction] = None) {
  def order(dir: Direction): Query = this.copy(order = Option(dir))
}

abstract class Operation {
  def from(table: String) = From(table, Option(this))
}
case class Select(val fields:String*) extends Operation
case class From(val table: String, val operation:Option[Operation] = None) {
  def where(clauses: Clause*): Query = Query(operation.get, this, Option(Where(clauses: _*)))
}

case class Where(val clauses: Clause*)

abstract class Clause {
  def and(otherField: Clause): Clause = And(this, otherField)
  def or(otherField: Clause): Clause = Or(this, otherField)
}

case class Like(val f: String, val value: String) extends Clause

case class StringEquals(val f: String, val value: String) extends Clause
case class NumberEquals(val f: String, val value: Number) extends Clause
case class BooleanEquals(val f: String, val value: Boolean) extends Clause

case class StringNotEquals(val f: String, val value: String) extends Clause
case class NumberNotEquals(val f: String, val value: Number) extends Clause
case class BooleanNotEquals(val f: String, val value: Boolean) extends Clause

case class StringGreaterThan(val f: String, val value: String) extends Clause
case class NumberGreaterThan(val f: String, val value: Number) extends Clause
case class BooleanGreaterThan(val f: String, val value: Boolean) extends Clause

case class StringLessThan(val f: String, val value: String) extends Clause
case class NumberLessThan(val f: String, val value: Number) extends Clause
case class BooleanLessThan(val f: String, val value: Boolean) extends Clause

case class StringGreaterThanEquals(val f: String, val value: String) extends Clause
case class NumberGreaterThanEquals(val f: String, val value: Number) extends Clause
case class BooleanGreaterThanEquals(val f: String, val value: Boolean) extends Clause

case class StringLessThanEquals(val f: String, val value: String) extends Clause
case class NumberLessThanEquals(val f: String, val value: Number) extends Clause
case class BooleanLessThanEquals(val f: String, val value: Boolean) extends Clause

case class In(val field: String, val values: String*) extends Clause
case class And(val lClause:Clause, val rClause:Clause) extends Clause
case class Or(val lClause:Clause, val rClause:Clause) extends Clause

abstract class Direction
case class Asc(field: String) extends Direction
case class Desc(field: String) extends Direction

object QueryBuilder {
  implicit def tuple2field(t: (String, String)): StringEquals = StringEquals(t._1, t._2)
  implicit def tuple2field(t: (String, Int)): NumberEquals = NumberEquals(t._1, t._2)
  implicit def tuple2field(t: (String, Boolean)): BooleanEquals = BooleanEquals(t._1, t._2)
  implicit def from2query(f: From): Query = Query(f.operation.get, f, Option(Where()))

  /** entrypoint for starting a select query */
  def select(fields:String*) = Select(fields:_*)
  def select(symbol: Symbol): Select = symbol match {
    case 'all => select("*")
    case _ => throw new RuntimeException("Only 'all allowed as symbol")
  }

  def in(field: String, values: String*) = In(field, values: _*)
}

case class SQL(val sql:String)

object AnsiSqlRenderer {
  implicit def query2sql(q:Query):SQL = SQL(sql(q))
  implicit def from2sql(f: From): SQL = SQL(sql(Query(f.operation.get, f, None)))

  def sql(query: Query): String = {
    List(
      expandOperation(query),
      expandFrom(query),
      expandWhere(query),
      expandOrder(query)
    )
      .filter(_ != None)
      .map(_ match {
      case Some(s) => s
      case s:String => s
    })
      .mkString(" ")
  }

  def expandOperation(query:Query):String = query.operation match {
    case Select(field:String) => "select %s".format(field)
    case s:Select => "select %s".format(s.fields.mkString(","))
    case _ => throw new IllegalArgumentException("Operation %s not implemented".format(query.operation))
  }

  def expandFrom(query: Query) = "from %s".format(query.from.table)
  def expandWhere(query: Query):Option[String] = {
    if (query.where.isEmpty || query.where.get.clauses.isEmpty)
      None
    else
      Option("where %s".format(query.where.get.clauses.map(expandClause(_)).mkString(" ")))
  }

  def expandPredicate(where: Where):Option[String] = {
    Option("%s".format(where.clauses.map(expandClause(_)).mkString(" ")))
  }


  def expandClause(clause: Clause): String = clause match {
    case StringEquals(field, value) => "%s = %s".format(field, quote(value))
    case BooleanEquals(field, value) => "%s = %s".format(field, value)
    case NumberEquals(field, value) => "%s = %s".format(field, value)

    case StringNotEquals(field, value) => "%s != %s".format(field, quote(value))
    case BooleanNotEquals(field, value) => "%s != %s".format(field, value)
    case NumberNotEquals(field, value) => "%s != %s".format(field, value)

    case StringGreaterThan(field, value) => "%s > %s".format(field, quote(value))
    case BooleanGreaterThan(field, value) => "%s > %s".format(field, value)
    case NumberGreaterThan(field, value) => "%s > %s".format(field, value)

    case StringLessThan(field, value) => "%s < %s".format(field, quote(value))
    case BooleanLessThan(field, value) => "%s < %s".format(field, value)
    case NumberLessThan(field, value) => "%s < %s".format(field, value)

    case StringGreaterThanEquals(field, value) => "%s >= %s".format(field, quote(value))
    case BooleanGreaterThanEquals(field, value) => "%s >= %s".format(field, value)
    case NumberGreaterThanEquals(field, value) => "%s >= %s".format(field, value)

    case StringLessThanEquals(field, value) => "%s <= %s".format(field, quote(value))
    case BooleanLessThanEquals(field, value) => "%s <= %s".format(field, value)
    case NumberLessThanEquals(field, value) => "%s <= %s".format(field, value)

    case Like(field, value) => "%s like %s".format(field, quote(value))


    case in:In => "%s in (%s)".format(in.field, in.values.map(quote(_)).mkString(","))
    case and:And => "(%s and %s)".format(expandClause(and.lClause), expandClause(and.rClause))
    case or:Or => "(%s or %s)".format(expandClause(or.lClause), expandClause(or.rClause))
    case _ => throw new IllegalArgumentException("Clause %s not implemented".format(clause))
  }

  def expandOrder(query: Query):Option[String] = query.order match {
    case Some(direction) => direction match {
      case Asc(field) => Option("order by %s asc".format(field))
      case Desc(field) => Option("order by %s desc".format(field))
    }
    case None => None
  }

  def quote(value: String) = "'%s'".format(escape(value))
  def escape(value: String) = value.replaceAll("'", "''")
}