package Pieces

import Game.Helpers.Color.Color
import Game.Helpers.Type.Type
import Game._
import Helpers.Type

class Rook(color: Color) extends Piece(color: Color) {

  def pieceType: Type = Type.Rook

  def blockedByPiece(state: Array[Array[Spot]], move: Move): Boolean = {
    isBlockedHorisontalOrVertical(state,move)
  }

  override
  def isValidMoveSet(state: Array[Array[Spot]], move: Move): Boolean = {

    if (!super.isValidMoveSet(state, move)) return false

    if (move.to._1 != move.from._1 && move.to._2 != move.from._2) return false

    true
  }

}
