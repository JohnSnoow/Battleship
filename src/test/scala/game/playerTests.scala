import org.scalatest.FunSuite
import scala.collection.immutable.HashMap
import game.Cell
import game.Grid
import game.Boat
import game.Player
import game.Human

class PlayerSuite extends FunSuite {
  val grid: Grid = Grid(HashMap("A1" -> Cell(1, "A"), "A2" -> Cell(2, "A")))
  val emptyGrid: Grid = Grid(HashMap())
  val boatToPlace: Boat = Boat("Destroyer", 2, List[Cell]())
  val boatPlaced: Boat = Boat("Destroyer", 2, List[Cell](Cell(1, "A"), Cell(2, "A")))
  val destroyedCruiser: Boat = Boat("Cruiser", 3, List[Cell](Cell(1, "B", true, true), Cell(2, "B", true, true), Cell(3, "B", true, true)))
  val player: Player = Human("Jean", 0, grid, emptyGrid, List[Boat](boatPlaced))
  val playerLost: Player = Human("Jean", 0, grid, emptyGrid, List[Boat](destroyedCruiser))

  test("It should be allowed to place a boat only if the cells are not occupied") {
    assert( player.tryPlaceBoat(boatToPlace, "A", 1, "H", 0, List[Cell]() ) == (boatToPlace, false) )
    assert( player.tryPlaceBoat(boatToPlace, "D", 3, "H", 0, List[Cell]() ) == (boatToPlace.copy(cells = List[Cell](Cell(3, "E", false, true), Cell(3, "D", false, true))), true) )
  }

  test("A player with only sunk boats should be detected") {
    assert( playerLost.checkGameLost() )
    assert( !player.checkGameLost() )
  }

  test("A grid should update if given a boat") {
    assert( Player.addboatTogrid(grid, destroyedCruiser, 0) == Grid( HashMap(
      "A1" -> Cell( 1, "A" ),
      "A2" -> Cell( 2, "A" ),
      "B1" -> Cell( 1, "B", true, true ),
      "B2" -> Cell( 2, "B", true, true ),
      "B3" -> Cell( 3, "B", true, true )
    ) ) )
  }

}
