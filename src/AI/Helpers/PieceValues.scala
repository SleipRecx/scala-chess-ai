package AI.Helpers
import Game.Helpers.Type
import Game.Helpers.Type.Type

object PieceValues {


  def value(typ: Type): Integer ={
    typ match{
      case Type.Pawn => 100
      case Type.Knight => 320
      case Type.Bishop => 330
      case Type.Rook => 500
      case Type.Queen => 900
      case Type.King => 20000
    }
  }


  def mobility: Integer = 10

}
