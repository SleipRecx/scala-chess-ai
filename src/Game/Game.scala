package Game

import java.time.{Duration, LocalTime}
import java.util.Scanner

import AI.Search
import Helpers.Color
import Helpers.Color.Color

class Game {
  val board = new Board()
  val ai = new Search()
  board.initBoard()

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
    val start = LocalTime.now()

   /* val m = ai.getAction(board,Color.White)
    board.movePiece(m._1,m._2)
    board.printBoard()*/
    try {

      while (true) {
        val m = ai.getAction(board, Color.White)
        board.movePiece(m._1, m._2)
        board.printBoard()
        val m2 = ai.getAction(board, Color.Black)
        board.movePiece(m2._1, m2._2)
        board.printBoard()
      }
    }
    catch{
      case e: Exception => println(e.printStackTrace())
    }

    val end = LocalTime.now()
    val time = Duration.between(start,end).getSeconds
    println("The Game took " + time + " seconds")

    /*
    while(true){
      val myScanner = new Scanner(System.in)
      val move = myScanner.nextLine().split(" ")
      val from = (Integer.valueOf(Integer.parseInt(move(0).charAt(1) + "") - 1 ),Integer.valueOf(translation(move(0).charAt(0))))
      val to = (Integer.valueOf(Integer.parseInt(move(1).charAt(1) + "") - 1 ),Integer.valueOf(translation(move(1).charAt(0))))
      try {
        val player = board.getColorFromPiece(from)
        if(player == turn){
          board.movePiece(from, to)
          board.printBoard()
          val m = ai.getAction(board, Color.White)
          board.movePiece(m._1,m._2)
          board.printBoard()

        }
        else{
          println("You can't move your opponents pieces")
        }

      }
      catch{
        case ex: IllegalArgumentException => println(ex.getLocalizedMessage)
        case e: Exception => println(e.printStackTrace())
      }
    }
    */

  }

  def main(args: Array[String]): Unit = {

  }

}
