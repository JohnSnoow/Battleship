package game

case class AIHard(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    def shoot(opponent: Player, random: Random): (Player, Player) = {
        (AIHard(name, number, ownedGrid, opponentGrid, boats), opponent)
    }
}
