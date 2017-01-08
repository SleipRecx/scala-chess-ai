package Pieces

import Game.Helpers.Color.Color
import Game.Helpers.Type.Type
import Game._
import Helpers.Type

class Knight(color: Color) extends Piece(color: Color) {

  def pieceType: Type = Type.Knight


  override
  def isValidMoveSet(state: Array[Array[Spot]], from: (Integer,Integer), to: (Integer,Integer)): Boolean = {

    if (!super.isValidMoveSet(state,from, to)) { return false }

    var valid = false

    val legalPairs: Array[(Int, Int)] = Array( (1,2),(1,-2),(-1,2),(-1,-2),(2,1),(2,-1),(-2,1),(-2,-1) )

    for (p <- legalPairs ) {
      if(to._1 == from._1 + p._1 && to._2 == from._2 + p._2){
        valid = true
      }
    }
    valid
  }

  def blockedByPiece(state: Array[Array[Spot]], from: (Integer,Integer), to: (Integer,Integer)): Boolean = { false }

}
