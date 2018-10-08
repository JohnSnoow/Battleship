package game

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.util.Random
import Prompt._

case class Human(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {

    def shoot(opponent: Player, random: Random): (Player, Player) = {
        val (colR, rowR): (String, Int) = askShot()
        // add shot in grid
        val (newGridOpponentGrid, newCellOpponentGrid): (Grid, Cell) = opponentGrid.cellHit(Cell(rowR, colR))
        val (newGridOpp, newCellOpp): (Grid, Cell) = opponent.ownedGrid.cellHit(Cell(rowR, colR))

        val newListBoat: List[Boat] = if ( opponent.ownedGrid.isOccupied( newCellOpp )) Boat.boatsAfterHit(opponent.boats, newCellOpp ) else opponent.boats
        if ( opponent.ownedGrid.isOccupied( newCellOpp )) {
            val boatHit: Boat = Boat.findBoat(newListBoat, newCellOpp)
            if ( boatHit.isDestroyed() ) showPrompt("Player " + opponent.name + " says : You've sunk my " + boatHit.name + " !\n" )
            else showPrompt("Player " + opponent.name + " says : Hit !\n" )
        } else showPrompt("Player " + opponent.name + " says : Miss !\n" )

        // return the new players
        val newHuman: Player = update(ownedGrid, Grid ( opponentGrid.cellGrid.updated( (colR + rowR.toString), newCellOpp ) ), boats)
        val newPlayer: Player = opponent.update(Grid ( opponent.ownedGrid.cellGrid.updated( (colR + rowR.toString), newCellOpp ) ), opponent.opponentGrid, newListBoat)

        (newHuman, newPlayer)
    }

    @tailrec
    final def askShot(): (String, Int) = {
        opponentGrid.showGrid()
        ownedGrid.showGrid()
        showPrompt("Player : " + name + "\n")
        showPrompt( "Select a collumn to shoot : \n" + "A-J \n" )
        val userInputCol = getUserInput("askCol").getOrElse("tryAgain")
        showPrompt( "Select a row to shoot : \n" + "1-10 \n" )
        val userInputRow = getUserInput("askRow").getOrElse("tryAgain")
        if (userInputCol == "tryAgain" || userInputRow == "tryAgain") askShot()
        else {
            val colR: String = Convert_Util.gridCollumns( Convert_Util.indexOfCol(userInputCol) ).toString
            val rowR: Int = userInputRow.toInt
            (colR, rowR)
        }
    }

    def update(ownedGrid: Grid, opponentGrid: Grid, boats: List[Boat]): Player = {
        copy(ownedGrid = ownedGrid, opponentGrid = opponentGrid, boats = boats)
    }


}

object Human {
    def placeBoats(human: Human, listBoats: List[Boat], random: Random): (Human) = {
        @tailrec
        def placeBoatsLoop(player: Human, boats: List[Boat], progress: Int, random: Random) : Human = {
            if (progress < 5) {
                player.ownedGrid.showGrid()
                showPrompt(
                    "Player " + player.name + "\n"
                    + "Boat to place :" + boats(progress).name + " (length : " + boats(progress).length + " )" + "\n")
                showPrompt(
                    "Select a collumn to place your boat : \n"
                    + "A-J \n")
                val userInputCol = getUserInput("askCol").getOrElse("tryAgain")
                showPrompt(
                    "Select a row to place your boat : \n"
                    + "1-10 \n")
                val userInputRow = getUserInput("askRow").getOrElse("tryAgain")
                showPrompt(
                    "Place your boat (h)orizontally or (v)ertically \n")
                val userInputDir = getUserInput("h or v").getOrElse("tryAgain")
                if (userInputCol == "tryAgain" || userInputRow == "tryAgain" || userInputDir == "tryAgain") placeBoatsLoop(player, boats, progress, random)
                else {
                    val (newBoat, success): (Boat, Boolean) = player.tryPlaceBoat(boats(progress),  userInputCol, userInputRow.toInt, userInputDir, 0, List[Cell]())
                    if (success) {
                        val newGrid: Grid = Player.addboatTogrid(player.ownedGrid, newBoat, 0)
                        placeBoatsLoop(player.copy(boats = newBoat :: player.boats, ownedGrid = newGrid), boats, progress+1, random)
                    } else {
                        placeBoatsLoop(player, boats, progress, random)
                    }
                }
            } else player
        }

        placeBoatsLoop(human, listBoats, 0, random)
    }
}
