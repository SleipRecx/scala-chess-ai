package Pieces

import Helpers.Color.Color
import Helpers.Type.Type
import Game._
import Helpers.Type

class Rook(color: Color) extends Piece(color: Color) {

  override val pieceValue: Integer = 500

  def pieceType: Type = Type.Rook


  def blockedByPiece(state: Array[Array[Spot]], from: (Integer,Integer), to: (Integer,Integer)): Boolean = {
    isBlockedHorisontalOrVertical(state,from,to)
  }


  override
  def isValidMoveSet(state: Array[Array[Spot]], from: (Integer,Integer), to: (Integer,Integer)): Boolean = {

    if (!super.isValidMoveSet(state,from,to)) { return false }

    if (to._1 != from._1 && to._2 != from._2) {
      return false
    }
    true
  }

}