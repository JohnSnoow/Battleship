package game

import scala.annotation.tailrec
import scala.util.Random

abstract class Player () {
    def name: String
    def number: Int
    def ownedGrid: Grid
    def opponentGrid: Grid
    def boats: List[Boat]
    def shoot(opponent: Player, random: Random): (Player, Player)
    @tailrec
    final def tryPlaceBoat(ai: Player, boat: Boat, colR: String, rowR: Int, dirR: String, progress: Int, cells: List[Cell]): (Player, Boolean) = {
        if (progress >= boat.length) (ai, true)
        else {

            tryPlaceBoat(ai, boat, colR, rowR, dirR, progress, cells) // to modify
        }
    } 
}

object Player {
    @tailrec
    final def placeBoatsRandom(ai: Player, listBoats: List[Boat], random: Random, progress: Int): (Player) = {
        if (progress <5){
            val colR: String = Convert_Util.gridCollumns(random.nextInt(10)).toString
            val rowR: Int = random.nextInt(10)
            val dirR: String = if (random.nextInt(2) == 0) "H" else "V"
            val (newPlayer, success) = ai.tryPlaceBoat(ai, listBoats(progress), colR, rowR, dirR, 0, List[Cell]())
            if (success) placeBoatsRandom(newPlayer, listBoats, random, progress+1) 
            else placeBoatsRandom(ai, listBoats, random, progress)
        } else ai
    }
}

case class Human(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    def shoot(opponent: Player, random: Random): (Player, Player) = {
        (Human(name, number, ownedGrid, opponentGrid, boats), opponent)
    }

    def placeBoats(human: Player, listBoat: List[Boat]): (Player) = {
        (human)
    }
}
case class AIEasy(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    def shoot(opponent: Player, random: Random): (Player, Player) = {
        val colR: String = Convert_Util.gridCollumns(random.nextInt(10)).toString
        val rowR: Int = random.nextInt(10)
        // add shot in grid

        // check if boat is hit
        (AIEasy(name, number, ownedGrid, opponentGrid, boats), opponent)
    }
}

case class AIMedium(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    def shoot(opponent: Player, random: Random): (Player, Player) = {
        (AIMedium(name, number, ownedGrid, opponentGrid, boats), opponent)
    }
}

case class AIHard(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    def shoot(opponent: Player, random: Random): (Player, Player) = {
        (AIHard(name, number, ownedGrid, opponentGrid, boats), opponent)
    }
}