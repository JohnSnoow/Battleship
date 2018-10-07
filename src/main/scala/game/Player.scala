package game

import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.util.Random
import Prompt._

abstract class Player () {
    def name: String
    def number: Int
    def ownedGrid: Grid
    def opponentGrid: Grid
    def boats: List[Boat]
    def shoot(opponent: Player, random: Random): (Player, Player)
    @tailrec
    final def tryPlaceBoat(boat: Boat, colR: String, rowR: Int, dirR: String, progress: Int, cells: List[Cell]): (Boat, Boolean) = {
        if (progress >= boat.length) (boat, true)
        else {
            val newColIndex: Int = Convert_Util.gridCollumns.indexOf(colR.head) + progress
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

case class Human(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    def shoot(opponent: Player, random: Random): (Player, Player) = {
        (Human(name, number, ownedGrid, opponentGrid, boats), opponent)
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

case class AIEasy(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    def shoot(opponent: Player, random: Random): (Player, Player) = {
        val colR: String = Convert_Util.gridCollumns(random.nextInt(10)).toString
        val rowR: Int = random.nextInt(10)
        // add shot in grid
        val (newGridOpponentGrid, newCellOpponentGrid): (Grid, Cell) = opponentGrid.cellHit(Cell(rowR, colR, false, false))
        val (newGridOpp, newCellOpp): (Grid, Cell) = opponent.ownedGrid.cellHit(Cell(rowR, colR, false, false))
        if (opponent.number == 1 || opponent.number == 2) {
            opponent.name match {
                case "AIEasy" => 
                    (AIEasy(name, number, ownedGrid, Grid ( opponentGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpponentGrid ) ), boats),  
                    AIEasy(opponent.name, opponent.number, Grid ( opponent.ownedGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpp ) ), opponent.opponentGrid, opponent.boats) )
                case "AIMedium" =>
                    (AIEasy(name, number, ownedGrid, Grid ( opponentGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpponentGrid ) ), boats),  
                    AIMedium(opponent.name, opponent.number, Grid ( opponent.ownedGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpp ) ), opponent.opponentGrid, opponent.boats) )
                case "AIHard" =>
                    (AIEasy(name, number, ownedGrid, Grid ( opponentGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpponentGrid ) ), boats),  
                    AIHard(opponent.name, opponent.number, Grid ( opponent.ownedGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpp ) ), opponent.opponentGrid, opponent.boats) )
            }
        }
        else (AIEasy(name, number, ownedGrid, Grid ( opponentGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpponentGrid ) ), boats),  
        Human(opponent.name, opponent.number, Grid ( opponent.ownedGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpp ) ), opponent.opponentGrid, opponent.boats) )
        
    }
}

case class AIMedium(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    @tailrec
    final def shoot(opponent: Player, random: Random): (Player, Player) = {
        val colR: String = Convert_Util.gridCollumns(random.nextInt(10)).toString
        val rowR: Int = random.nextInt(10)
        if ( opponent.ownedGrid.isHit(Cell(rowR, colR, false, false)) ) shoot(opponent, random)
        else {
            // add shot in grid
            val (newGridOpponentGrid, newCellOpponentGrid): (Grid, Cell) = opponentGrid.cellHit(Cell(rowR, colR, false, false))
            val (newGridOpp, newCellOpp): (Grid, Cell) = opponent.ownedGrid.cellHit(Cell(rowR, colR, false, false))
            if (opponent.number == 1 || opponent.number == 2) {
                opponent.name match {
                    case "AIEasy" => 
                        (AIEasy(name, number, ownedGrid, Grid ( opponentGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpponentGrid ) ), boats),  
                        AIEasy(opponent.name, opponent.number, Grid ( opponent.ownedGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpp ) ), opponent.opponentGrid, opponent.boats) )
                    case "AIMedium" =>
                        (AIEasy(name, number, ownedGrid, Grid ( opponentGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpponentGrid ) ), boats),  
                        AIMedium(opponent.name, opponent.number, Grid ( opponent.ownedGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpp ) ), opponent.opponentGrid, opponent.boats) )
                    case "AIHard" =>
                        (AIEasy(name, number, ownedGrid, Grid ( opponentGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpponentGrid ) ), boats),  
                        AIHard(opponent.name, opponent.number, Grid ( opponent.ownedGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpp ) ), opponent.opponentGrid, opponent.boats) )
                }
            }
            else (AIEasy(name, number, ownedGrid, Grid ( opponentGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpponentGrid ) ), boats),  
            Human(opponent.name, opponent.number, Grid ( opponent.ownedGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpp ) ), opponent.opponentGrid, opponent.boats) )
        }
    }
}

case class AIHard(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    def shoot(opponent: Player, random: Random): (Player, Player) = {
        (AIHard(name, number, ownedGrid, opponentGrid, boats), opponent)
    }
}