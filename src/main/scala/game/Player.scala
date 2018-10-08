package game

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.util.Random
import Prompt._

abstract class Player () {
    val name: String
    val number: Int
    val ownedGrid: Grid
    val opponentGrid: Grid
    val boats: List[Boat]
    def shoot(opponent: Player, random: Random): (Player, Player)
    @tailrec
    final def tryPlaceBoat(boat: Boat, colR: String, rowR: Int, dirR: String, progress: Int, cells: List[Cell]): (Boat, Boolean) = {
        if (progress >= boat.length) (boat, true)
        else {
            val newColIndex: Int = Convert_Util.indexOfCol(colR) + progress
            val newRowR: Int = rowR + progress
            if ( (dirR == "H" && newColIndex >= 10) || (dirR == "V" && newRowR > 10) ) (boat, false)
            else {
                val isTaken: Boolean = if (dirR == "H") ownedGrid.cellGrid.exists(_._1 == Convert_Util.gridCollumns( newColIndex ).toString + rowR.toString) else ownedGrid.cellGrid.exists(_._1 == colR + newRowR.toString)
                if (isTaken) (boat, false)
                else {
                    if (dirR == "H") tryPlaceBoat(boat.copy(cells = Cell(rowR, Convert_Util.gridCollumns( newColIndex ).toString, false, true) :: boat.cells), colR, rowR, dirR, progress+1, Cell(rowR, Convert_Util.gridCollumns( newColIndex ).toString, false, true) :: cells)
                    else tryPlaceBoat(boat.copy(cells = Cell(newRowR, colR, false, true) :: boat.cells), colR, rowR, dirR, progress+1, Cell(newRowR, colR, false, true) :: cells)
                }
            }
        }
    }

    def checkGameLost(): Boolean = !boats.exists( boat => !boat.isDestroyed() )


    def update(ownedGrid: Grid, opponentGrid: Grid, boats: List[Boat]): Player

}

object Player {
    @tailrec
    final def placeBoatsRandom(ai: Player, listBoats: List[Boat], random: Random, progress: Int): (Player) = {
        if (progress <5) {
            val colR: String = Convert_Util.gridCollumns(random.nextInt(10)).toString
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

    @tailrec
    final def addboatTogrid(grid: Grid, boat: Boat, progress: Int): (Grid) = {
        if (progress >= boat.length) grid
        else {
            val (cellRow, cellCol): (Int, String) = (boat.cells(progress).row, boat.cells(progress).col)
            addboatTogrid(Grid( HashMap[String, Cell]((cellCol + cellRow.toString) -> boat.cells(progress)) ++ grid.cellGrid), boat, progress+1)
        }
    }

}
