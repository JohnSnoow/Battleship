package game

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.util.Random
import Prompt._

/** abstract Class Player
* @param name the given name of the Player
* @param number the number of the Player.
* @param ownedGrid the grid of the Player. Shows his boats and the shots of his opponent
* @param opponentGrid the grid of the Player. SHows his shots and his opponents' hit boats.
* @param boats the list of boats of the Player.
*/
abstract class Player () {
    val name: String
    val number: Int
    val ownedGrid: Grid
    val opponentGrid: Grid
    val boats: List[Boat]


    def shoot(opponent: Player, random: Random): (Player, Player)

    /** tryPlaceBoat, the function used by all players validate a boat placement
    * @param boat the boat to place
    * @param colR the starting column
    * @param rowR the starting row
    * @param dirR horizontal or vertical, the orientation of the boat on the grid
    * @param progress 0 => boat.length, indicates the cell in the list of the boat we need to validate
    * @param cells the updated list of cells of the boat
    * @return a new Boat, with his cells updated, and a Boolean, true if the boat was placed, or else false
    */
    @tailrec
    final def tryPlaceBoat(boat: Boat, colR: String, rowR: Int, dirR: String, progress: Int, cells: List[Cell]): (Boat, Boolean) = {
        if (progress >= boat.length) (boat, true)
        else {
            val newColIndex: Int = Convert_Util.indexOfCol(colR) + progress
            val newRowR: Int = rowR + progress
            if ( (dirR == "H" && newColIndex >= 10) || (dirR == "V" && newRowR > 10) ) (boat, false)
            else {
                val isTaken: Boolean = if (dirR == "H") ownedGrid.cellGrid.exists(_._1 == Convert_Util.gridcolumns( newColIndex ).toString + rowR.toString) else ownedGrid.cellGrid.exists(_._1 == colR + newRowR.toString)
                if (isTaken) (boat, false)
                else {
                    if (dirR == "H") tryPlaceBoat(
                      boat.copy(
                        cells = Cell(rowR, Convert_Util.gridcolumns( newColIndex ).toString, false, true) :: boat.cells),
                      colR,
                      rowR,
                      dirR,
                      progress+1,
                      Cell(rowR, Convert_Util.gridcolumns( newColIndex ).toString, false, true) :: cells)
                    else tryPlaceBoat(
                      boat.copy(
                        cells = Cell(newRowR, colR, false, true) :: boat.cells),
                      colR,
                      rowR,
                      dirR,
                      progress+1,
                      Cell(newRowR, colR, false, true) :: cells)
                }
            }
        }
    }

    /** checkGameLost
    * @return a Boolean, true if the player has all of his boats sunk
    */
    def checkGameLost(): Boolean = !boats.exists( boat => !boat.isDestroyed() )

    /** update, allows player.copy(...)
    * @param ownedGrid the new grid of the player, showing his boats
    * @param opponentGrid the new grid of the player, showing his shots
    * @param boats the new list of boats of the player
    * @return a new player, using the new values
    */
    def update(ownedGrid: Grid, opponentGrid: Grid, boats: List[Boat]): Player

}

object Player {

    /** placeBoatsRandom, the function used by all AI to place their boats on the grid
    * @param ai  the ai to copy
    * @param listBoats the boats to place
    * @param random the random generator
    * @param progress indicates the number of boats placed
    * @return the new ai, with the list of placed boats
    */
    @tailrec
    final def placeBoatsRandom(ai: Player, listBoats: List[Boat], random: Random, progress: Int): (Player) = {
        if (progress <5) {
            val colR: String = Convert_Util.gridcolumns(random.nextInt(10)).toString
            val rowR: Int = random.nextInt(10) + 1
            val dirR: String = if (random.nextInt(2) == 0) "H" else "V"
            val (newBoat, success) = ai.tryPlaceBoat(listBoats(progress), colR, rowR, dirR, 0, List[Cell]())
            if (success) {
                val newGrid: Grid = Player.addboatTogrid(ai.ownedGrid, newBoat, 0)
                if (ai.name == "AIEasy") placeBoatsRandom(AIEasy("AIEasy", ai.number, newGrid, ai.opponentGrid, newBoat :: ai.boats), listBoats, random, progress+1)
                else if (ai.name == "AIMedium") placeBoatsRandom(AIMedium("AIMedium", ai.number, newGrid, ai.opponentGrid, newBoat :: ai.boats), listBoats, random, progress+1)
                else placeBoatsRandom(AIHard("AIHard", ai.number, newGrid, ai.opponentGrid, newBoat :: ai.boats), listBoats, random, progress+1)
            }
            else placeBoatsRandom(ai, listBoats, random, progress)
        } else ai
    }


    /** addboatTogrid, the function used by all players to place their boats on the grid
    * @param grid the grid owned by the player
    * @param boat the boat to place
    * @param progress indicates the number of cells updated
    * @return the new grid, with the cells occupied by the boat updated
    */
    @tailrec
    final def addboatTogrid(grid: Grid, boat: Boat, progress: Int): (Grid) = {
        if (progress >= boat.length) grid
        else {
            val (cellRow, cellCol): (Int, String) = (boat.cells(progress).row, boat.cells(progress).col)
            addboatTogrid(
              Grid( HashMap[String, Cell]((cellCol + cellRow.toString) -> boat.cells(progress)) ++ grid.cellGrid),
              boat,
              progress+1)
        }
    }

}
