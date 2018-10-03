# Battleship

## Short descritpion 

**Battleship** is a two players' guessing game where you need to sink the ships of your opponent to win. Each player deploys five ships that have different lengths, varying from two to five. Each turn a player will take a shot in a grid where the other player's boats are hidden. A ship is said to have been sunk if all the cells it occupies on the grid have been shot. 

##  Detailed description

- There are 4 grids, 2 for each player. In the first the player sees his ships and the cells shot by its opponent. In the other it shows his shots and the locations of damaged or sunk ships. The grids are 10x10 squares.
- There are 5 ships :
1. Carrier : length 5
2. Battleship : length 4
3. Cruiser : length 3
4. Submarine : length 3
5. Destroyer : length 2
- A ship can't be placed if he occupies one or more cells of another of the player's ships.
- The game consists in a series of rounds. Each round, each player takes his turn and announce his shot in the opponent's grid. The opponent then declares if the shot is a "hit", a "miss", and in the case of a "hit", if the ship has been sunk.
- A shot that "missed" is shown in white, a shot that "hit" is shown in red in the corresponding grids.
- When all the ships of a player have been sunk, the game ends and the other player is declared winner.