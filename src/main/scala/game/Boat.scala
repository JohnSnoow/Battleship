package game

/** Class Boat
* @param name the term used to indicate the boat
* @param length the number of cell the boat takes
* @param cells a copy of the List of cells that the Boat occupies on the grid
*/
case class Boat(name: String, length: Int, cells: List[Cell]) {

    /** boatHit
    * @param cells the cell that has been hit
    * @return a copy of the boat, updated if the cell' coordinates given are in his list.
    */
    def boatHit( cell: Cell ): Boat = {
        val newcells: List[Cell] = cells.map( cellToCheck => if ( cellToCheck.row == cell.row && cellToCheck.col == cell.col ) Cell(cell.row, cell.col, true, cellToCheck.occupied) else cellToCheck )
        (Boat(name, length, newcells))
    }

    /** isDestroyed
    * @return a Boolean, true if the boats has no non-hit cells, or else false.
    */
    def isDestroyed(): Boolean = {
        !cells.exists( cell => !cell.hit )
    }

}

object Boat {

    /** boatsAfterHit
    * @param boats the list of boats to update after a shot that hit
    * @param cell the cell that has been hit
    * @return a copy of the boats' list, with one boat updated if the cell given is in his list.
    */
    def boatsAfterHit(boats: List[Boat], cell: Cell): List[Boat] = boats.map( boat => boat.boatHit(cell))

    /** findBoat
    * @param boats the list of boats of a player
    * @param cell the cell to search
    * @return the boat that has the coordinates of the given cell in his list.
    */
    def findBoat(boats: List[Boat], cell: Cell): Boat = boats.dropWhile( boat => !boat.cells.exists( cellToCheck => (cell.row == cellToCheck.row && cell.col == cellToCheck.col) ) ).head

}
