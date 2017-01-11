package Game

import java.time.{Duration, LocalTime}
import java.util.Scanner

import AI.Search
import Helpers.Color
import Helpers.Color.Color

class Game {

  val board = new Board
  val ai = new Search

  val translation = Map('a' -> 0, 'b' -> 1,'c' -> 2, 'd' -> 3,'e' -> 4, 'f' -> 5, 'g' -> 6,'h' -> 7)

  var turn: Color = Color.Black

  def switchTurn(): Unit = {
    if(turn == Color.White){
      turn = Color.Black
    }
    else{
      turn = Color.White
    }
  }

  def startGame(): Unit ={

    /*
    try {

      while (true) {
        val m = ai.getBestMove(board, Color.White)
        board.movePiece(m)
        board.printChessBoard()
        val m2 = ai.getBestMove(board, Color.Black)
        board.movePiece(m2)
        board.printChessBoard()
      }
    }
    catch{
      case e: Exception => println(e.printStackTrace())
    }
    */

    val start = LocalTime.now()
    val m = ai.getAction(board,Color.White)
    board.movePiece(m)
    board.printBoard()
    val end = LocalTime.now()
    val time = Duration.between(start,end).getSeconds
    println("The Game took " + time + " seconds")

    while(true){
      val myScanner = new Scanner(System.in)
      val move = myScanner.nextLine().split(" ")

      try {
        val x1 = move(0).charAt(1).asDigit - 1
        val y1 = translation(move(0).charAt(0))
        val x2 = move(1).charAt(1).asDigit - 1
        val y2 = translation(move(1).charAt(0))
        val brusj = new Move((x1,y1),(x2,y2))
        board.movePiece(brusj)
        board.printBoard()
        val m = ai.getAction(board, Color.White)
        board.movePiece(m)
        board.printBoard()

      }
      catch{
        case ex: IllegalArgumentException => println(ex.getLocalizedMessage)
        case e: Exception => println(e.printStackTrace())
      }
    }

  }

  def main(args: Array[String]): Unit = {

  }

}
