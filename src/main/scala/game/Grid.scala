package game

import scala.collection.mutable.HashMap
import scala.annotation.tailrec
import Prompt._

case class Grid(cellGrid: HashMap[String, Cell]) {


    def showGrid(): Unit = {
        val str: String = "|    A    B    C    D    E    F    G    H    I    J\n"

        @tailrec
        def prepareGridLoop(printableGrid: String, progress: Int): String = {
            if (progress > 100) printableGrid
            else {
                val tens = (progress / 10) + 1 // Calculates the row we are at
                val tensToPrint = if (tens == 10) "10 " else tens.toString + "  " // show line number
                val letterCol = if (progress % 10 == 0) Convert_Util.gridCollumns(9) else Convert_Util.gridCollumns(progress%10 - 1 )
                val cell: Cell = cellGrid.get( letterCol + tens.toString ).getOrElse(Cell(0,"", false, false))
                val printableCell: String = if ( cell.hit && cell.occupied ) " [X] " else if ( cell.hit ) " [O] " else if ( cell.occupied ) " [B] " else " [ ] "
                if (progress % 10 == 1) prepareGridLoop(printableGrid + tensToPrint + printableCell, progress+1) 
                else if (progress % 10 == 0) prepareGridLoop(printableGrid + printableCell + "\n", progress+1)
                else prepareGridLoop(printableGrid + printableCell, progress+1)
            }
        }

        val gridToPrompt: String = prepareGridLoop(str, 1)
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
