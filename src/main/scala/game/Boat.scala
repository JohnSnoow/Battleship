package game


case class Boat(name: String, length: Int, cells: List[Cell]) {

    def boatHit( cell: Cell ): Boat = {
        val newcells: List[Cell] = cells.map( cellToCheck => if ( cellToCheck.row == cell.row && cellToCheck.col == cell.col ) Cell(cell.row, cell.col, true, cellToCheck.occupied) else cellToCheck )
        (Boat(name, length, newcells))
    }

    def isDestroyed(): Boolean = {
        !cells.exists( cell => !cell.hit )
    }

}

object Boat {

    def boatsAfterHit(boats: List[Boat], cell: Cell): List[Boat] = boats.map( boat => boat.boatHit(cell))

    def findBoat(boats: List[Boat], cell: Cell): Boat = boats.dropWhile( boat => !boat.cells.exists( cellToCheck => (cell.row == cellToCheck.row && cell.col == cellToCheck.col) ) ).head

}
