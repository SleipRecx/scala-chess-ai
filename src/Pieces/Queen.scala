package Pieces

import Game.Helpers.Color.Color
import Game.Helpers.Type.Type
import Game._
import Helpers.Type

class Queen(color: Color) extends Piece(color: Color) {

  def pieceType: Type = Type.Queen

  def blockedByPiece(state: Array[Array[Spot]], move: Move): Boolean = {

    val x_diff = Math.abs(move.to._1 - move.from._1)
    val y_diff = Math.abs(move.to._2 - move.from._2)

    if (x_diff == y_diff) isBlockedDiagonal(state,move)
    else isBlockedHorisontalOrVertical(state,move)

  }


  override
  def isValidMoveSet(state: Array[Array[Spot]], move: Move): Boolean = {

    if (!super.isValidMoveSet(state,move)) return false

    val x_diff = Math.abs(move.to._1 - move.from._1)
    val y_diff = Math.abs(move.to._2 - move.from._2)

    if(!(x_diff == y_diff) && !(move.to._1 == move.from._1) && !(move.to._2 == move.from._2)) return false

    true
  }

}
