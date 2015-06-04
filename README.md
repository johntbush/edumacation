# edumacation

## Scala Parser Combinators

Some good reading...

 * http://bitwalker.org/blog/2013/08/10/learn-by-example-scala-parser-combinators/
 * http://debasishg.blogspot.com/2008/04/external-dsls-made-easy-with-scala.html

### Some basics

You are typically going to extend `RegexParsers` or `JavaTokenParsers`.  This gives you the basic parsing infrastructure and common regex patterns you need to build on. 

* | is the alternation combinator. It says “succeed if either the left or right operand parse successfully”
* ~ is the sequential combinator. It says “succeed if the left operand parses successfully, and then the right parses successfully on the remaining input”
* ~> says “succeed if the left operand parses successfully followed by the right, but do not include the left content in the result”
* <~ is the reverse, “succeed if the left operand is parsed successfully followed by the right, but do not include the right content in the result”
* ^^=> is the transformation combinator. It says “if the left operand parses successfully, transform the result using the function on the right”
* rep => simply says “expect N-many repetitions of parser X” where X is the parser passed as an argument to rep

https://github.com/wdavidw/node-csv-parse/blob/master/lib/index.js