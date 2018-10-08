package game


import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.util.Random
import Prompt._


case class AIMedium(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    @tailrec
    final def shoot(opponent: Player, random: Random): (Player, Player) = {
        val (colR, rowR): (String, Int) = askShot(random)
        if ( opponent.ownedGrid.isHit(Cell(rowR, colR)) ) shoot(opponent, random)
        else {
            // add shot in grid
            val (newGridOpponentGrid, newCellOpponentGrid): (Grid, Cell) = opponentGrid.cellHit(Cell(rowR, colR))
            val (newGridOpp, newCellOpp): (Grid, Cell) = opponent.ownedGrid.cellHit(Cell(rowR, colR))

            val newListBoat: List[Boat] = if ( opponent.ownedGrid.isOccupied( newCellOpp )) Boat.boatsAfterHit(opponent.boats, newCellOpp ) else opponent.boats

            val newAI: Player = update(ownedGrid, Grid ( opponentGrid.cellGrid.updated( (colR + rowR.toString), newCellOpp )), boats)
            val newPlayer: Player = opponent.update(Grid ( opponent.ownedGrid.cellGrid.updated( (colR + rowR.toString), newCellOpp )), opponent.opponentGrid, newListBoat)

            (newAI, newPlayer)
        }
    }

    def askShot(random: Random): (String, Int) = {
        val colR: String = Convert_Util.gridCollumns(random.nextInt(10)).toString
        val rowR: Int = random.nextInt(10) + 1
        (colR, rowR)
    }

    def update(ownedGrid: Grid, opponentGrid: Grid, boats: List[Boat]): Player = {
        copy(ownedGrid = ownedGrid, opponentGrid = opponentGrid, boats = boats)
    }
}
