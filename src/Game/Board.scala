package Game

import Helpers._
import Helpers.Color
import Helpers.Color.Color
import Pieces._

import scala.collection.mutable.ArrayBuffer


class Board{

  val NUMBER_OF_ROWS, NUMBER_OF_COLS = 8

  var state: Array[Array[Spot]] = Array.ofDim[Spot](8, 8)
  var turn: Color = Color.White
  initBoard()

  def switchTurn(): Unit = {
     turn = if(turn == Color.White) Color.Black else Color.White
  }

  def getAllOccupiedSpotsByColor(c: Color): ArrayBuffer[Spot] = {
    var spots = ArrayBuffer[Spot]()
    state.foreach(e1 => e1.foreach(e2 => if(e2.isOccupied) if(e2.piece.color == c) spots += e2))
    spots
  }

  def getAllSpots: ArrayBuffer[Spot] = {
    var spots = ArrayBuffer[Spot]()
    state.foreach(e1 => e1.foreach(e2 => spots += e2))
    spots
  }

  def initBoard(): Unit = {
    for(i <- 0 until NUMBER_OF_COLS){
      for(j <- 0 until NUMBER_OF_COLS){
        state(i)(j) = new Spot(i,j)
      }
    }
    state(0)(0).addPiece(new Rook(Color.White))
    state(0)(1).addPiece(new Knight(Color.White))
    state(0)(2).addPiece(new Bishop(Color.White))
    state(0)(3).addPiece(new Queen(Color.White))
    state(0)(4).addPiece(new King(Color.White))
    state(0)(5).addPiece(new Bishop(Color.White))
    state(0)(6).addPiece(new Knight(Color.White))
    state(0)(7).addPiece(new Rook(Color.White))

    for(i <- 0 until NUMBER_OF_COLS){
      state(1)(i).addPiece(new Pawn(Color.White))
    }

    state(7)(0).addPiece(new Rook(Color.Black))
    state(7)(1).addPiece(new Knight(Color.Black))
    state(7)(2).addPiece(new Bishop(Color.Black))
    state(7)(3).addPiece(new Queen(Color.Black))
    state(7)(4).addPiece(new King(Color.Black))
    state(7)(5).addPiece(new Bishop(Color.Black))
    state(7)(6).addPiece(new Knight(Color.Black))
    state(7)(7).addPiece(new Rook(Color.Black))

    for(i <- 0 until NUMBER_OF_COLS){
      state(6)(i).addPiece(new Pawn(Color.Black))
    }

  }

  def copyBoardState(): Array[Array[Spot]] = {
    val copyState: Array[Array[Spot]] = Array.ofDim[Spot](8,8)
    for(i <- 0 until NUMBER_OF_COLS){
      for(j <- 0 until NUMBER_OF_COLS){
        copyState(i)(j) = new Spot(i,j)
        if(state(i)(j).isOccupied){
            state(i)(j).piece.pieceType match {
            case Type.Pawn => copyState(i)(j).piece = new Pawn(state(i)(j).piece.color)
            case Type.Rook => copyState(i)(j).piece = new Rook(state(i)(j).piece.color)
            case Type.Bishop => copyState(i)(j).piece = new Bishop(state(i)(j).piece.color)
            case Type.Queen => copyState(i)(j).piece = new Queen(state(i)(j).piece.color)
            case Type.Knight => copyState(i)(j).piece = new Knight(state(i)(j).piece.color)
            case Type.King => copyState(i)(j).piece = new King(state(i)(j).piece.color)
          }
          copyState(i)(j).piece.moved_=(state(i)(j).piece.moved)
        }
      }
    }
    copyState
  }

  def boardInCheck(state: Array[Array[Spot]], color: Color): Boolean = {
    var king: (Integer,Integer) = (0,0)
    var inCheck = false

    for(i <- state.indices){
      for(j <- state.indices){
        if(state(i)(j).piece != null) {
          if (state(i)(j).piece.pieceType == Type.King && color == state(i)(j).piece.color) {
            king = (i, j)
          }
        }
      }
    }

    for(i <- state.indices) {
      for (j <- state.indices) {
        if (state(i)(j).isOccupied && state(i)(j).piece.color != color) {
          if (state(i)(j).piece.isValidMoveSet(state, new Move((i,j),(king._1, king._2)))) {
              inCheck = true
          }
        }
      }
    }
    inCheck
  }

  def printBoard(): Unit = {
    for(i <- 7 to 0 by -1){
      println()
      print(Console.WHITE +  "   +-----+-----+-----+-----+-----+-----+-----+-----+")
      println()
      val x = i + 1
      print(Console.YELLOW + x + " ")
      print(Console.WHITE + " | ")
      for(j <- 0 to 7){
        if(state(i)(j).isOccupied){
          if(state(i)(j).piece.color == Color.White){

            print(Console.WHITE + " " )
            print(Console.RESET + state(i)(j).piece.pieceType)
            print(Console.WHITE + "  | " )
          }
          else{

            print(Console.WHITE + " " )
            print(Console.BLUE + state(i)(j).piece.pieceType)
            print(Console.WHITE + "  | " )
          }
        }

        else{
          print(Console.WHITE + "    | ")
        }
      }

    }
    println()
    println("   +-----+------+------+------+------+------+------+")
    println(Console.YELLOW + "      a     b     c     d     e     f     g     h")
    println(Console.RESET)
  }

  def causesCheck(color: Color, m: Move): Boolean = {
    val state: Array[Array[Spot]] = copyBoardState()
    state(m.to._1)(m.to._2) = state(m.from._1)(m.from._2)
    state(m.from._1)(m.from._2) = new Spot(m.from._1,m.from._2)

    if (boardInCheck(state,color)) true
    else false
  }

  def getColorFromPiece(from: (Int, Int)): Color = {
    if(state(from._1)(from._2).isOccupied){
      state(from._1)(from._2).piece.color
    }
    else{
      throw new IllegalArgumentException("no piece on that square")
    }
  }

  def isLegalMove(m: Move): Boolean = {

    if (m.from  == m.to) return false

    if (m.from._1 > 7 || m.from._1 < 0 || m.from._2 > 7 || m.from._2 < 0) return false

    if (m.to._1 > 7 || m.to._1 < 0 || m.to._2 > 7 || m.to._2 < 0) return false

    if (!state(m.from._1)(m.from._2).isOccupied) return false

    if (state(m.to._1)(m.to._2).isOccupied) {
      if (state(m.from._1)(m.from._2).piece.color == state(m.to._1)(m.to._2).piece.color) return false
    }

    if(!state(m.from._1)(m.from._2).piece.isValidMoveSet(state, m)) return false

    if (causesCheck(state(m.from._1)(m.from._2).piece.color, m)) return false

    true
  }


  def movePiece(m: Move): Unit ={
    val spot = state(m.from._1)(m.from._2)
    val newSpot = state(m.to._1)(m.to._2)

    if(!isLegalMove(m)) throw new IllegalArgumentException("Not a legal move")

    newSpot.addPiece(spot.piece)
    spot.piece.moved = true
    spot.removePiece()

    switchTurn()
  }

}
