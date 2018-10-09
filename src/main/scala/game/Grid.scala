package game

import scala.collection.immutable.HashMap
import scala.annotation.tailrec
import Prompt._

case class Grid(cellGrid: HashMap[String, Cell]) {

    /** showGrid, prints the grid for the user
    */
    def showGrid(): Unit = {
        val str: String = ("Legend: Miss: [O], Boat (Hit): [X], Boat (not Hit): [B] \n"
        + "|    A    B    C    D    E    F    G    H    I    J\n")

        @tailrec
        def prepareGridLoop(printableGrid: String, progressRow: Int, progressCol: Int): String = {
            if (progressRow > 10) printableGrid
            else if (progressCol > 9) prepareGridLoop(printableGrid + "\n", progressRow+1, 0)
            else {
                val tensToPrint = if (progressRow == 10) "10 " else progressRow + "  " // show line number
                val letterCol = Convert_Util.gridcolumns(progressCol).toString
                val cell: Cell = cellGrid.get( letterCol + progressRow.toString ).getOrElse(Cell(0,""))
                val printableCell: String = if ( cell.hit && cell.occupied ) " [X] " else if ( cell.hit ) " [O] " else if ( cell.occupied ) " [B] " else " [ ] "
                if (progressCol == 0) prepareGridLoop(printableGrid + tensToPrint + printableCell, progressRow, progressCol+1)
                else prepareGridLoop(printableGrid + printableCell, progressRow, progressCol+1)
            }
        }

        val gridToPrompt: String = prepareGridLoop(str, 1, 0)
        showPrompt(gridToPrompt)
    }

    /** addCell
    * @param cell a cell to add to the grid
    * @return the new grid, with the given cell added
    */
    def addCell(cell: Cell): (Grid) = {
        val newGrid: Grid = Grid(cellGrid ++ HashMap((cell.col + cell.row) -> cell) )
        (newGrid)
    }

    /** cellHit
    * @param cell the cell that has been hit
    * @return the new grid, updated with the newCell, and a new cell, updated to show that it has been hit
    */
    def cellHit(cell: Cell): (Grid, Cell) = {
        val extractedCell: Cell = cellGrid.get(cell.col+cell.row).getOrElse(Cell(cell.row, cell.col))
        val newCell: Cell = extractedCell.copy( hit = true )
        val newGrid: Grid = Grid ( cellGrid.updated( (cell.col + cell.row.toString), newCell ))
        (newGrid, newCell)
    }

    /** isHit
    * @param cell the cell to check
    * @return a Boolean, true if the cell is hit, or else false
    */
    def isHit(cell: Cell): (Boolean) = cellGrid.get(cell.col+cell.row).getOrElse(Cell(0, "")).hit

    /** isOccupied
    * @param cell the cell to check
    * @return a Boolean, true if the cell is occupied, or else false
    */
    def isOccupied(cell: Cell): (Boolean) = cellGrid.get(cell.col+cell.row).getOrElse(Cell(0, "")).occupied


}
