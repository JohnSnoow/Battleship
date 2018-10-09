import org.scalatest.FunSuite
import scala.collection.immutable.HashMap
import game.Cell
import game.Grid

class GridSuite extends FunSuite {

  val grid: Grid = Grid(HashMap("A1" -> Cell(1, "A"),"F2" -> Cell(2, "F", true, true)))

  test("A grid hit should have a new cell or updated its cell") {
    assert( grid.cellHit(Cell(3, "D")) == (Grid(HashMap("A1" -> Cell(1, "A"), "D3" -> Cell(3, "D", true, false), "F2" -> Cell(2, "F", true, true))), Cell(3, "D", true, false)) )
    assert( grid.cellHit(Cell(1, "A")) == (Grid(HashMap("A1" -> Cell(1, "A", true, false),"F2" -> Cell(2, "F", true, true))), Cell(1, "A", true, false) ) )
  }

  test("A grid hit should return true given the correct cell") {
    assert( !grid.isHit(Cell(1, "A")) )
    assert( grid.isHit(Cell(2, "F")) )
  }

  test("An occupied cell in a grid should be detected") {
    assert( !grid.isOccupied(Cell(8, "G")) )
    assert( grid.isOccupied(Cell(2, "F")) )
  }
}
