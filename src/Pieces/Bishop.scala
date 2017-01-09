package Pieces

import Game.Helpers.Color.Color
import Game.Helpers.Type.Type
import Game._
import Helpers.Type

class Bishop(color: Color) extends Piece(color: Color) {

  def pieceType: Type = Type.Bishop

  def blockedByPiece(state: Array[Array[Spot]], move: Move): Boolean = {
    isBlockedDiagonal(state,move)
  }

  override
  def isValidMoveSet(state: Array[Array[Spot]], move: Move): Boolean = {

    if (!super.isValidMoveSet(state, move)) return false

    if(Math.abs(move.to._1 - move.from._1) != Math.abs(move.to._2 - move.from._2)) return false

    true
  }

}
