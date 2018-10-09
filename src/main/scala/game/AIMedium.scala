package game


import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.util.Random
import Prompt._

/** Class AIMedium
* @param name the given name of the AI
* @param number the number of the Player. For an AI it can be : (1-2)
* @param ownedGrid the grid of the Player. Shows his boats and the shots of his opponent
* @param opponentGrid the grid of the Player. SHows his shots and his opponents' hit boats.
* @param boats the list of boats of the Player.
*/
case class AIMedium(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {

    /** shoot, use tailrec to ask another shot if the cell has already been hit
    * @param opponent the other Player
    * @param random the random generator
    * @return the copies, updated, of the two players
    */
    @tailrec
    final def shoot(opponent: Player, random: Random): (Player, Player) = {
        val (colR, rowR): (String, Int) = askShot(random)
        if ( opponent.ownedGrid.isHit(Cell(rowR, colR)) ) shoot(opponent, random)
        else {
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
    }

    /** askShot
    * @param random the random generator
    * @return a column and a row, generated with random
    */
    def askShot(random: Random): (String, Int) = {
        val colR: String = Convert_Util.gridcolumns(random.nextInt(10)).toString
        val rowR: Int = random.nextInt(10) + 1
        (colR, rowR)
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
