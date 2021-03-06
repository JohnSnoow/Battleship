package game

import scala.util.Random
import scala.io.StdIn.readLine


object Prompt {

    def showPrompt(message: String): Unit = {
        Console.println(message) 
    }

    def getUserInput(question: String): Option[String] = {
        val response = readLine.trim.toUpperCase
        question match {
            case "gameMode" => if (response == "1" || response == "2"|| response == "3") Some(response) else if (response == "Q") Some("Quit") else None
            case "userName" => Some(response)
            case "AIDifficulty" => if (response == "E" || response == "M"|| response == "H") Some(response) else if (response == "Q") Some("Quit") else None
            case "askCol" => if (Convert_Util.gridcolumns.exists(letter => letter.toString == response)) Some(response) else None
            case "askRow" => if (Convert_Util.strToInt(response).getOrElse(-1) >= 1 && Convert_Util.strToInt(response).getOrElse(-1) <= 10) Some(response) else None
            case "remakeGame" => if (response == "Y" || response == "N") Some(response) else None
            case "h or v" => if (response == "H" || response == "V") Some(response) else None
            case _ => None
        }
    }

    def printGameOver(): Unit = println("\n=== GAME OVER ===\n")

    def printGameOver(winner: Player): Unit = println(
        "\n=== GAME OVER ===\n"
        + "The player " + winner.name + " wins !\n")

}
