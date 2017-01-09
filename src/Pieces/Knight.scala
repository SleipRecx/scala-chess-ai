package Pieces

import Game.Helpers.Color.Color
import Game.Helpers.Type.Type
import Game._
import Helpers.Type

class Knight(color: Color) extends Piece(color: Color) {

  def pieceType: Type = Type.Knight

  override
  def isValidMoveSet(state: Array[Array[Spot]], move: Move): Boolean = {

    if (!super.isValidMoveSet(state, move)) return false

    var valid = false

    val legalPairs = Array( (1,2),(1,-2),(-1,2),(-1,-2),(2,1),(2,-1),(-2,1),(-2,-1) )

    legalPairs.foreach(p =>  if(move.to._1 == move.from._1 + p._1 && move.to._2 == move.from._2 + p._2){
      valid = true
    })

    valid
  }

  def blockedByPiece(state: Array[Array[Spot]], move: Move): Boolean = false

}
