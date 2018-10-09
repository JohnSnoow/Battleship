import org.scalatest.FunSuite
import game.Cell

class CellSuite extends FunSuite {

  test("A simple created Cell should not be hit") {
    assert(!Cell(1, "A").hit)
  }


  test("An simple created Cell should not be occupied") {
    assert(!Cell(1, "A").occupied)
  }

}
