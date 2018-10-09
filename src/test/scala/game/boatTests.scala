import org.scalatest.FunSuite
import game.Cell
import game.Boat

class BoatSuite extends FunSuite {

  val cruiser: Boat = Boat("Cruiser", 3, List[Cell](Cell(1, "A", false, true), Cell(2, "A", false, true), Cell(3, "A", false, true)))
  val destroyedCruiser: Boat = Boat("Cruiser", 3, List[Cell](Cell(1, "B", true, true), Cell(2, "B", true, true), Cell(3, "B", true, true)))

  test("A boat hit should have his hit cell modified") {
    assert(cruiser.boatHit(Cell(1, "A")) == cruiser.copy(cells = List[Cell](Cell(1, "A", true, true ), Cell(2, "A", false, true), Cell(3, "A", false, true))) )
  }

  test("A destroyed boat should have no non-hit cell") {
    assert(destroyedCruiser.isDestroyed())
    assert(!cruiser.isDestroyed())
  }

  test("boatsAfterHit should return an updated ListOfBoats if the cell is occupied buy one of the boats") {
    assert( Boat.boatsAfterHit(List[Boat](cruiser, destroyedCruiser),  Cell(1, "A", false, true)) == List[Boat](cruiser.boatHit( Cell(1, "A" )), destroyedCruiser ) )
    assert( Boat.boatsAfterHit(List[Boat](cruiser, destroyedCruiser),  Cell(1, "F")) == List[Boat](cruiser, destroyedCruiser ) )
  }

  test("A boat should be found if a cell that has the right is provided") {
    assert(Boat.findBoat(List[Boat](cruiser, destroyedCruiser), Cell(1, "A", false, true)) == cruiser)
  }

}
