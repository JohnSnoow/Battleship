package game

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.util.Random
import Prompt._


/** Class AIHard
* @param name the given name of the AI
* @param number the number of the Player. For an AI it can be : (1-2)
* @param ownedGrid the grid of the Player. Shows his boats and the shots of his opponent
* @param opponentGrid the grid of the Player. SHows his shots and his opponents' hit boats.
* @param boats the list of boats of the Player.
*/
case class AIHard(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {

    /** shoot,
    * @param opponent the other Player
    * @param random the random generator
    * @return the copies, updated, of the two players
    */
    def shoot(opponent: Player, random: Random): (Player, Player) = {
        val (colR, rowR): (String, Int) = askShot(opponentGrid, random)

        // add shot in grid
        val (newGridOpponentGrid, newCellOpponentGrid): (Grid, Cell) = opponentGrid.cellHit(Cell(rowR, colR))
        val (newGridOpp, newCellOpp): (Grid, Cell) = opponent.ownedGrid.cellHit(Cell(rowR, colR))

        val newListBoat: List[Boat] = if ( opponent.ownedGrid.isOccupied( newCellOpp )) Boat.boatsAfterHit(opponent.boats, newCellOpp ) else opponent.boats

        val newAI: Player = update(
          ownedGrid,
          Grid ( opponentGrid.cellGrid.updated( (colR + rowR.toString), newCellOpp )),
          boats)
        val newPlayer: Player = opponent.update(
          Grid ( opponent.ownedGrid.cellGrid.updated( (colR + rowR.toString), newCellOpp )),
          opponent.opponentGrid,
          newListBoat)

        (newAI, newPlayer)
    }

    /** askShot
    * @param grid the grid of the player, showing hit boats and missed shots
    * @param random the random generator
    * @return a column and a row, generated with random or if there are non-hit cells near a hit cell, one of them
    */
    def askShot(grid: Grid, random: Random): (String, Int) = {
        val optionalCell: Option[Cell] = tailRecAskShot(grid, grid.cellGrid)
        optionalCell match {
            case Some ( Cell(row, col, hit, occupied) ) => (col, row)
            case None => (Convert_Util.gridcolumns(random.nextInt(10)).toString, random.nextInt(10) + 1)
        }
    }

    /** tailRecAskShot
    * @param grid the grid of the player, showing hit boats and missed shots
    * @param hashmap the grid.cellGrid of the player, on which we will work
    * @param random the random generator
    * @return an Option[Cell] if there is a non-hit cell near another hit cell, or else None
    */
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

    /** update, allows player.copy(...)
    * @param ownedGrid the new grid of the player, showing his boats
    * @param opponentGrid the new grid of the player, showing his shots
    * @param boats the new list of boats of the player
    * @return a new player, using the new values
    */
    def update(ownedGrid: Grid, opponentGrid: Grid, boats: List[Boat]): Player = {
        copy(ownedGrid = ownedGrid, opponentGrid = opponentGrid, boats = boats)
    }

}
