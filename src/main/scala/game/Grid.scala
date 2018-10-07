package game

import scala.collection.mutable.HashMap
import scala.annotation.tailrec
import Prompt._

case class Grid(cellGrid: HashMap[String, Cell]) {


    def showGrid(): Unit = {
        // faire une récursion sur 100 itérations
        // check si le truc existe dans la grille
        // changer en case
        // Quand ça finit. Prompt showPrompt()
        val str: String = "|    A    B    C    D    E    F    G    H    I    J\n"

        @tailrec
        def prepareGridLoop(printableGrid: String, progress: Int): String = {
            if (progress > 100) printableGrid
            else {
                val tens = (progress / 10) + 1 // Calculates the row we are at
                val tensToPrint = if (tens == 10) "10 " else tens.toString + "  " 
                val letterCol = if (progress % 10 == 0) Convert_Util.gridCollumns(9) else Convert_Util.gridCollumns(progress%10 - 1 )
                val cell: Cell = cellGrid.get( letterCol + tens.toString ).getOrElse(Cell(0,"", false, false))
                val printableCell: String = if ( cell.hit && cell.occupied ) " [+] " else if ( cell.hit ) " [X] " else if ( cell.occupied ) " [B] " else " [ ] "
                if (progress % 10 == 1) prepareGridLoop(printableGrid + tensToPrint + printableCell, progress+1) 
                else if (progress % 10 == 0) prepareGridLoop(printableGrid + printableCell + "\n", progress+1)
                else prepareGridLoop(printableGrid + printableCell, progress+1)
            }
        }

        val gridToPrompt: String = prepareGridLoop(str, 1)
        showPrompt(gridToPrompt)
    }

    def addCell(grid: Grid, cell: Cell): (Grid, Cell) = {
        val newGrid: Grid = Grid(grid.cellGrid ++ HashMap((cell.col + cell.row) -> cell) )
        (newGrid, cell)
    }

    def cellHit(cell: Cell): (Grid, Cell) = {
        val extractedCell: Cell = cellGrid.get(cell.col+cell.row).getOrElse(Cell(0, "", false, false))
        val newCell: Cell = Cell( cell.row, cell.col, true, extractedCell.occupied )
        val newGrid: Grid = Grid ( cellGrid.clone() += ( (cell.col + cell.row) -> newCell ) )
        (newGrid, newCell)
    }

    
}
