package Helpers

import java.security.SecureRandom
import Game.Board

class Zobrist {

  val zArray: Array[Array[Array[Array[Long]]]] = Array.ofDim[Long](2, 6, 8, 8)
  val zBlackMove: Long = random64
  fillArray()

  def random64: Long = {
    val random = new SecureRandom()
    random.nextLong()
  }

  def fillArray(): Unit = {
      for (color <- 0 until 2) {
        for (piece <- 0 until 6) {
          for (x <- 0 until 7) {
            for (y <- 0 until 7) {
              zArray(color)(piece)(x)(y) = random64
            }
          }
        }
      }
  }

  def getZobristHash(board: Board): Long = {
    var key: Long = 0
    for (i <- board.state.indices) {
      for (j <- board.state.indices) {
        if (board.state(i)(j).isOccupied) {
          val piece = board.state(i)(j).piece
          if(piece.color == Color.White) {
            piece.pieceType match {
              case Type.Pawn => key ^= zArray(0)(0)(i)(j)
              case Type.Knight => key ^= zArray(0)(1)(i)(j)
              case Type.Bishop => key ^= zArray(0)(2)(i)(j)
              case Type.Rook => key ^= zArray(0)(3)(i)(j)
              case Type.Queen => key ^= zArray(0)(4)(i)(j)
              case Type.King => key ^= zArray(0)(5)(i)(j)
            }
          }
          else{
            piece.pieceType match {
              case Type.Pawn => key ^= zArray(1)(0)(i)(j)
              case Type.Knight => key ^= zArray(1)(1)(i)(j)
              case Type.Bishop => key ^= zArray(1)(2)(i)(j)
              case Type.Rook => key ^= zArray(1)(3)(i)(j)
              case Type.Queen => key ^= zArray(1)(4)(i)(j)
              case Type.King => key ^= zArray(1)(5)(i)(j)
            }
          }
        }
      }
    }
    if(board.turn == Color.Black){
      key ^= zBlackMove
    }
  key
}




}
