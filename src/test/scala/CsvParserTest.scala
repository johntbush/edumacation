import utils.CsvParser

import scala.io.Source._

/**
 * Test the CsvParser
 */
class CsvParserTest extends org.scalatest.WordSpec with org.scalatest.MustMatchers with org.scalatest.OptionValues {

  "parse a file" in  {

    val inStream = getClass.getResourceAsStream("/sample.csv")
    val docAsString = fromInputStream(inStream).getLines().mkString("\n")
    val parsedDoc = CsvParser(docAsString)

    System.out.println(parsedDoc.toString())

    assert(
      "List(List(heading 1, heading 2, heading 3, heading 4), List(1, 2, 3, 4), List(5, 6, 7, 8), List(9, 10, 11, 12), List(13, 14, 15, 16), List(17, 18, 19, 20), List(woo, dude this is cool, right_on, bro))" ==
      parsedDoc.toString()
    )
  }
}
