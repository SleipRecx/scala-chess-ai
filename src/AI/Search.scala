package AI

import java.time.{Duration, LocalTime}
import AI.Helpers.PieceTable
import AI.Helpers.PieceValues
import Game.{Board, Move}
import Game.Helpers.Color.Color
import Game.Helpers.Type.Type
import Game.Helpers._
import scala.collection.mutable.ArrayBuffer

class Search {

  def getBestMove(board: Board, color: Color) : Move = {
    val start = LocalTime.now()
    val move = alphaBetaSearch(board, 3, color)
    val end = LocalTime.now()
    val time = Duration.between(start,end).getSeconds
    println("The move took " + time + " seconds for the AlphaBeta AI")
    move
  }

  private def alphaBetaSearch(board: Board, depth: Int, color: Color): Move = {

    def maxPrune(board: Board,depth: Int, alpha: Double, beta: Double): (Double, Move) = {
      var a: Double = alpha

      if(depth == 0){
        return (getBasicEvaluation(board), new Move((0,0),(0,0)))
      }
      var bestValue = Double.MinValue
      var bestMove: Move = new Move((0,0),(0,0))

      val moves = generateMoves(board,Color.White)


      for (m <- moves) {
        val newState = generateSuccessorState(m, board)
        val value = minPrune(newState, depth - 1, a, beta)._1

        bestValue = Math.max(bestValue,value)
        a = Math.max(a, bestValue)

        if (value == bestValue) bestMove = m
        if (bestValue > beta) return (bestValue,m)
      }
      (bestValue,bestMove)
    }

    def minPrune(board: Board, depth: Int, alpha: Double, beta: Double): (Double, Move) ={
      var b: Double = beta

      if(depth == 0){
        return (getBasicEvaluation(board), new Move((0,0),(0,0)))
      }

      var bestValue = Double.MaxValue
      var bestMove: Move = new Move((0,0),(0,0))
      val moves = generateMoves(board,Color.Black)


      for (m <- moves) {
        val newState = generateSuccessorState(m, board)
        val value = maxPrune(newState, depth - 1, alpha, b)._1

        bestValue = Math.min(bestValue,value)
        b = Math.min(b,bestValue)

        if (value == bestValue) bestMove = m
        if (alpha > bestValue) return (bestValue,m)
      }
      (bestValue,bestMove)
    }

    color match {
      case Color.White => maxPrune(board, depth, Double.MinValue, Double.MaxValue)._2
      case Color.Black => minPrune(board, depth, Double.MinValue, Double.MaxValue)._2
    }

  }

  private def getBasicEvaluation(board: Board): Double = {
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

    score += PieceValues.value(Type.King) * (wK-bK)
    score += PieceValues.value(Type.Queen) * (wQ-bQ)
    score += PieceValues.value(Type.Rook) * (wR-bR)
    score += PieceValues.value(Type.Bishop) * (wB-bB)
    score += PieceValues.value(Type.Knight) * (wN-bN)
    score += PieceValues.value(Type.Pawn) * (wP-bP)
    score += PieceValues.mobility * (wM-bM)

    score
  }

  private def generateSuccessorState(move: Move, board: Board): Board = {
    val newBoard = new Board()
    newBoard.state = board.cloneBoardState()
    newBoard.movePiece(move)
    newBoard
  }

  private def generateMoves(board: Board, color: Color) : ArrayBuffer[Move] = {

    val killerMoves = new ArrayBuffer[Move]()
    val validMoves = new ArrayBuffer[Move]()

    board.getAllOccupiedSpotsByColor(color).foreach(s1 => board.getAllSpots.foreach(s2 => {
      val move = new Move( (s1.cord._1, s1.cord._2), (s2.cord._1, s2.cord._2) )
      if (board.isLegalMove(move)) {
        if (s2.isOccupied) killerMoves.+=(move)
        else validMoves.+=(move)
      }
    }))

    killerMoves ++= validMoves
    killerMoves
  }

  private def countPieces(board: Board, pieceType: Type, color: Color): Int = {
    var value: Int = 0
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

  private def negaMax(board: Board, depth: Integer, color: Color): (Double, Move) ={
    if(depth == 0){
      var v = getBasicEvaluation(board)
      if(color == Color.Black) v = -1 * v
      return (v, new Move((0,0),(0,0)) )
    }
    var bestValue = Double.MinValue
    var bestMove = new Move((0,0),(0,0))
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
