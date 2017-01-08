import Game.{Board, Game}
import Helpers.{Table, Zobrist}


object Main {

  def main(args: Array[String]): Unit = {
    val game = new Game()
    game.startGame()

  }

}
