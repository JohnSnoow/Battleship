package game

import scala.util.Random
import scala.io.StdIn.readLine


object Prompt {

    def showPrompt(message: String): Unit = { 
        print(message) 
    }

    def getUserInput(question: String): Option[String] = {
        val response = readLine.trim.toUpperCase
        question match {
            case "gameMode" => if (response == "1" || response == "2"|| response == "3") Some(response) else if (response == "Q") Some("Quit") else None
            case "IADifficulty" => if (response == "E" || response == "M"|| response == "H") Some(response) else if (response == "Q") Some("Quit") else None
            case "placeCol" => if (Convert_Util.gridCollumns.exists(letter => letter.toString == response)) Some(response) else None
            case "placeRow" => if (Convert_Util.strToInt(response).getOrElse(-1) >= 1 && Convert_Util.strToInt(response).getOrElse(-1) <= 10) Some(response) else None
            case "h or v" => if (response == "H" || response == "V") Some(response) else None
            case _ => None
        }
    }

    def printGameOver(): Unit = println("\n=== GAME OVER ===\n")

    def printGameOver(winner: Player): Unit = println(
        "\n=== GAME OVER ===\n"
        + "The player " + winner.name + "wins !\n")

}

