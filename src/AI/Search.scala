package AI

import java.time.{Duration, LocalTime}

import AI.Helpers.PieceTable
import AI.Helpers.PieceValues
import Game.Board
import Game.Helpers.Color.Color
import Game.Helpers.Type.Type
import Game.Helpers._

import scala.collection.mutable.ArrayBuffer

class Search {
  val zobrist = new Zobrist
  var transTable: Map[Long,((Integer, Integer), (Integer, Integer))] = Map()

  def getAction(board: Board, color: Color) : ((Integer, Integer), (Integer, Integer)) = {
    val start = LocalTime.now()
    val move = iterativeDeepening(board,3,color)
    val end = LocalTime.now()
    val time = Duration.between(start,end).getSeconds
    println("The move took " + time + " seconds for the AlphaBeta AI")
    move
  }


  def iterativeDeepening(board: Board, depth: Integer, color: Color): ((Integer, Integer), (Integer, Integer)) = {
    var firstGuess: ((Integer, Integer), (Integer, Integer)) = null
    for(d <- 1 to depth){
      firstGuess = alphaBetaSearch(board,d,color)
      transTable += (zobrist.getZobristHash(board) -> firstGuess)
    }
    firstGuess
  }

  def alphaBetaSearch(board: Board, depth: Integer, color: Color): ((Integer, Integer), (Integer, Integer)) = {
    color match {
      case Color.White => maxPrune(board, depth, Double.MinValue, Double.MaxValue)._2
      case Color.Black => minPrune(board, depth, Double.MinValue, Double.MaxValue)._2
    }
  }

  def maxPrune(board: Board,depth: Integer, alpha: Double, beta: Double): (Double,((Integer, Integer), (Integer, Integer))) = {
    var a: Double = alpha

    if(depth == 0){
      return (getBasicEvaluation(board),((0,0),(0,0)))
    }
    var bestValue = Double.MinValue
    var bestMove: ((Integer,Integer),(Integer,Integer)) = ((0,0),(0,0))

    val moves = generateKillerMoves(board,Color.White)


    for (m <- moves) {
      val newState = generateSuccessorState(m, board)
      val value = minPrune(newState, depth - 1, a, beta)._1
      bestValue = Math.max(bestValue,value)
      a = Math.max(a, bestValue)
      if (value == bestValue) {
        bestMove = m
      }
      if (bestValue > beta) {
        return (bestValue,m)
      }
    }
    (bestValue,bestMove)
  }

  def minPrune(board: Board, depth: Integer, alpha: Double, beta: Double): (Double,((Integer, Integer), (Integer, Integer))) ={
    var b: Double = beta

    if(depth == 0){
      return (getBasicEvaluation(board),((0,0),(0,0)))
    }

    var bestValue = Double.MaxValue
    var bestMove: ((Integer,Integer),(Integer,Integer)) = ((0,0),(0,0))
    val moves = generateKillerMoves(board,Color.Black)


    for (m <- moves) {
      val newState = generateSuccessorState(m, board)
      val value = maxPrune(newState, depth - 1, alpha, b)._1
      bestValue = Math.min(bestValue,value)
      if (value == bestValue) {
        bestMove = m
      }
      if (alpha > bestValue) {
        return (bestValue,m)
      }
      b = Math.min(b,bestValue)
    }
    (bestValue,bestMove)
  }

  def getBasicEvaluation(board: Board): Double = {
    var score = 0
    var wK, bK, wQ, bQ, wR, bR, wB, bB, wN, bN, wP, bP = 0
    val wM = generateMoves(board,Color.White).length
    val bM = generateMoves(board,Color.Black).length

    for(i <- 0 until 8){
      for(j <- 0 until 8){
        val current = board.state(i)(j)
        if(current.isOccupied){
          current.piece.pieceType match{
            case Type.Pawn =>
              if(current.piece.color == Color.White){
                wP += 1
                score += PieceTable.WhitePawn(i)(j)
              }
              else {
                bP +=1
                score +=  - PieceTable.BlackPawn(i)(j)
              }
            case Type.Queen =>
              if(current.piece.color == Color.White){
                wQ +=1
                score += PieceTable.WhiteQueen(i)(j)

              }
              else {
                bQ +=1
                score += - PieceTable.BlackQueen(i)(j)
              }
            case Type.Bishop =>
              if(current.piece.color == Color.White){
                wB +=1
                score += PieceTable.WhiteBishop(i)(j)
              }
              else {
                bB +=1
                score += - PieceTable.BlackBishop(i)(j)
              }
            case Type.Knight =>
              if(current.piece.color == Color.White){
                wN +=1
                score += PieceTable.WhiteKnight(i)(j)
              }
              else {
                bN +=1
                score += - PieceTable.BlackKnight(i)(j)
              }
            case Type.Rook =>
              if(current.piece.color == Color.White){
                wR +=1
                score += PieceTable.WhiteRook(i)(j)
              }
              else {
                bR +=1
                score += - PieceTable.BlackRook(i)(j)
              }
            case Type.King => if(current.piece.color == Color.White){wK +=1} else {bK +=1}
          }
        }
      }
    }

    score += PieceValues.KingValue * (wK-bK)
    score += PieceValues.QueenValue * (wQ-bQ)
    score += PieceValues.RookValue * (wR-bR)
    score += PieceValues.BishopValue * (wB-bB)
    score += PieceValues.KnightValue * (wN-bN)
    score += PieceValues.PawnValue * (wP-bP)
    score += PieceValues.MobilityValue * (wM-bM)

    score
  }

  def countPieces(board: Board, pieceType: Type, color: Color): Integer = {
    var value: Integer = 0
    for(i <- 0 until 8){
      for(j <- 0 until 8){
        if(board.state(i)(j).isOccupied){
          if(board.state(i)(j).piece.pieceType == pieceType && board.state(i)(j).piece.color == color){
            value += 1
          }
        }
      }
    }
    value
  }

  def generateSuccessorState(action: ((Integer,Integer),(Integer,Integer)), board: Board): Board = {
    val newBoard = new Board()
    newBoard.state = board.copyBoardState()
    newBoard.movePiece(action._1,action._2)
    newBoard.switchTurn()
    newBoard
  }

  def generateKillerMoves(board: Board, color: Color) : ArrayBuffer[((Integer, Integer), (Integer, Integer))] = {

    val killerMoves = new ArrayBuffer[((Integer, Integer), (Integer, Integer))]()
    val validMoves = new ArrayBuffer[((Integer, Integer), (Integer, Integer))]()

    for(fromRow <- 0 until 8) {
      for(fromCol <- 0 until 8) {
        if (board.state(fromRow)(fromCol).isOccupied){
          if (board.state(fromRow)(fromCol).piece.color == color) {
            for(toRow <- 0 until 8) {
              for(toCol <- 0 until 8) {
                val from: (Integer,Integer) = (fromRow, fromCol)
                val to: (Integer,Integer)  = (toRow, toCol)
                if(board.isLegalMove(from,to)) {
                  if(board.state(toRow)(toCol).isOccupied) killerMoves.+=((from,to))
                  else validMoves.+=((from,to))
                }
              }
            }
          }
        }
      }
    }
    killerMoves ++= validMoves
    killerMoves
  }

  def generateMoves(board: Board, color: Color) : ArrayBuffer[((Integer, Integer), (Integer, Integer))] = {
    val validCoordinates = new ArrayBuffer[(Integer, Integer)]()
    val validMoves = new ArrayBuffer[((Integer, Integer), (Integer, Integer))]()

    for (i <- 0 until 8) {
      for (j <- 0 until 8) {
        validCoordinates.+=((i, j))
      }
    }

    for (from <- validCoordinates) {
      if (board.state(from._1)(from._2).isOccupied) {
        if (board.state(from._1)(from._2).piece.color == color){
          for (to <- validCoordinates) {
            if (board.isLegalMove(from,to)) {
              validMoves.+=((from, to))
            }
          }
        }
      }
    }
    validMoves
  }

  def negaMax(board: Board, depth: Integer, color: Color): (Double,((Integer, Integer), (Integer, Integer))) ={
    if(depth == 0){
      var v = getBasicEvaluation(board)
      if(color == Color.Black) v = -1 * v
      return (v,((0,0),(0,0)) )
    }
    var bestValue = Double.MinValue
    var bestMove: ((Integer,Integer),(Integer,Integer)) = ((0,0),(0,0))
    val moves = generateMoves(board,color)
    for(m <- moves){
      val newState = generateSuccessorState(m,board)
      var c = Color.White
      if(c == color) c = Color.Black
      val r = negaMax(newState,depth - 1,c)
      val value = - r._1
      if(value >= bestValue){
        bestValue = value
        bestMove = m
      }
    }
    (bestValue,bestMove)
  }

}
