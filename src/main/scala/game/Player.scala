package game

abstract class Player (name: String, number: Int) {
    def name: String
}

case class Human(name: String, number: Int) extends Player(name, number)
case class AI(name: String, number: Int) extends Player(name, number)

