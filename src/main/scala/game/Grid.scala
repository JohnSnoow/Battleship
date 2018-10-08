package game

import scala.collection.mutable.HashMap
import scala.annotation.tailrec
import Prompt._

case class Grid(cellGrid: HashMap[String, Cell]) {


    def showGrid(): Unit = {
        val str: String = "|    A    B    C    D    E    F    G    H    I    J\n"

        @tailrec
        def prepareGridLoop(printableGrid: String, progressRow: Int, progressCol: Int): String = {
            if (progressRow > 10) printableGrid
            else if (progressCol > 9) prepareGridLoop(printableGrid + "\n", progressRow+1, 0)
            else {
                val tensToPrint = if (progressRow == 10) "10 " else progressRow + "  " // show line number
                val letterCol = Convert_Util.gridCollumns(progressCol).toString
                val cell: Cell = cellGrid.get( letterCol + progressRow.toString ).getOrElse(Cell(0,"", false, false))
                val printableCell: String = if ( cell.hit && cell.occupied ) " [X] " else if ( cell.hit ) " [O] " else if ( cell.occupied ) " [B] " else " [ ] "
                if (progressCol == 0) prepareGridLoop(printableGrid + tensToPrint + printableCell, progressRow, progressCol+1)
                else prepareGridLoop(printableGrid + printableCell, progressRow, progressCol+1)
            }
        }

        val gridToPrompt: String = prepareGridLoop(str, 1, 0)
        showPrompt(gridToPrompt)
    }

    def addCell(cell: Cell): (Grid) = {
        val newGrid: Grid = Grid(cellGrid ++ HashMap((cell.col + cell.row) -> cell) )
        (newGrid)
    }

    def cellHit(cell: Cell): (Grid, Cell) = {
        val extractedCell: Cell = cellGrid.get(cell.col+cell.row).getOrElse(Cell(0, "", false, false))
        val newCell: Cell = Cell( cell.row, cell.col, true, extractedCell.occupied )
        val newGrid: Grid = Grid ( cellGrid.clone() += ( (cell.col + cell.row) -> newCell ) )
        (newGrid, newCell)
    }

    def isHit(cell: Cell): (Boolean) = {
        val extractedCell: Cell = cellGrid.get(cell.col+cell.row).getOrElse(Cell(0, "", false, false))
        (extractedCell.occupied == true)
    }


}
