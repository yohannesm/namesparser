package example

import org.scalatest._

class NameParserSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    NameParser.greeting shouldEqual "hello"
  }
}
