type Point = (Int, Int)
type Field = Vector[Vector[Boolean]]
type Ship = List[Point]
type Fleet = Map[String, Ship]
type Game = (Field, Fleet)

def fillRow(j: Int, vec: Vector[Boolean]): Vector[Boolean] = j match {
  case j if j == 0 => {
    val vecNew = Vector[Boolean](false)
    fillRow(j + 1, vecNew)
  }
  case j if j == 9 => vec :+ false
  case j if j < 9 => {
    val vecNew = vec :+ false
    fillRow(j + 1, vecNew)
  }
}

def fillFieldByRow(i: Int, field: Field): Field = i match {
    case i if i == 9 => field :+ fillRow(0, Nil.toVector)
    case i if i == 0 => {
      val fieldNew = Vector[Vector[Boolean]](fillRow(0, Nil.toVector))
      fillFieldByRow(i + 1, fieldNew)
    }
    case i if i < 9 => {
      val fieldNew = field :+ fillRow(0, Nil.toVector)
      fillFieldByRow(i + 1, fieldNew)
    }
}

fillFieldByRow(0, Nil.toVector)