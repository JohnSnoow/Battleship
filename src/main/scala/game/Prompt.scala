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
            case _ => None
        }
    }

    def printGameOver(): Unit = println("\n=== GAME OVER ===")

    def printGameOver(winner: Player): Unit = println(
        "\n=== GAME OVER ===\n"
        + "The player " + winner.name + "wins !")

}