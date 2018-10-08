package game

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.util.Random
import Prompt._

case class AIHard(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    def shoot(opponent: Player, random: Random): (Player, Player) = {
        val (colR, rowR): (String, Int) = askShot(opponentGrid, random)
        
        // add shot in grid
        val (newGridOpponentGrid, newCellOpponentGrid): (Grid, Cell) = opponentGrid.cellHit(Cell(rowR, colR))
        val (newGridOpp, newCellOpp): (Grid, Cell) = opponent.ownedGrid.cellHit(Cell(rowR, colR))

        val newListBoat: List[Boat] = if ( opponent.ownedGrid.isOccupied( newCellOpp )) Boat.boatsAfterHit(opponent.boats, newCellOpp ) else opponent.boats

        val newAI: Player = update(ownedGrid, Grid ( opponentGrid.cellGrid.updated( (colR + rowR.toString), newCellOpp )), boats)
        val newPlayer: Player = opponent.update(Grid ( opponent.ownedGrid.cellGrid.updated( (colR + rowR.toString), newCellOpp )), opponent.opponentGrid, newListBoat)

        (newAI, newPlayer)
    }

    
    def askShot(grid: Grid, random: Random): (String, Int) = {
        val optionalCell: Option[Cell] = tailRecAskShot(grid, grid.cellGrid)
        optionalCell match {
            case Some ( Cell(row, col, hit, occupied) ) => (col, row)
            case None => (Convert_Util.gridCollumns(random.nextInt(10)).toString, random.nextInt(10) + 1)
        }
    }

    @tailrec
    private def tailRecAskShot(grid: Grid, hashmap: HashMap[String, Cell]): Option[Cell] = {
        hashmap.headOption match {
            case None => None
            case Some( (key, Cell(row, col, hit, occupied)) ) => 
                if (!occupied) tailRecAskShot(grid, hashmap.tail)
                else {
                    if ( row-1 > 0 && !grid.isHit( Cell(row-1, col) ) ) Some(Cell(row-1, col))
                    else if ( row+1 < 11 && !grid.isHit( Cell(row+1, col) ) ) Some(Cell(row+1, col))
                    else if ( ( Convert_Util.indexOfCol(col) - 1 ) >=0 && !grid.isHit( Cell(row, Convert_Util.indexOfCol(col) - 1 ) ) ) Some(Cell(row, Convert_Util.indexOfCol(col) - 1 ))
                    else if ( ( Convert_Util.indexOfCol(col) + 1 ) <10 &&  !grid.isHit( Cell(row, Convert_Util.indexOfCol(col) + 1 ) ) ) Some(Cell(row, Convert_Util.indexOfCol(col) + 1 ))
                    else tailRecAskShot(grid, hashmap.tail)
                }
        }
    }


    def update(ownedGrid: Grid, opponentGrid: Grid, boats: List[Boat]): Player = {
        copy(ownedGrid = ownedGrid, opponentGrid = opponentGrid, boats = boats)
    }

}
