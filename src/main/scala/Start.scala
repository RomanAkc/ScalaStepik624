object Start extends App {
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

  val field = fillFieldByRow(0, Nil.toVector)

  //println(fillFieldByRow(0, Nil.toVector))

  def validateShip(ship: Ship): Boolean = {
    if(ship.length == 0 || ship.length > 4)
      return  false

    val isXOkay = ship.forall(p => p._1 == ship(0)._1)
    val isYOkay = ship.forall(p => p._2 == ship(0)._2)

    if(!isXOkay && !isYOkay)
      return false

    def checkPrevious(ship: Ship, index: Int, prevValue: Int, checkX: Boolean): Boolean = {
      if(ship.length <= index)
        return true

      if(checkX) {
        if(scala.math.abs(ship(index)._1 - prevValue) != 1)
          return false
        else
          checkPrevious(ship, index + 1, ship(index)._1, checkX)
      } else {
        if(scala.math.abs(ship(index)._2 - prevValue) != 1)
          return false
        else
          checkPrevious(ship, index + 1, ship(index)._1, checkX)
      }
    }

    if(isXOkay)
      checkPrevious(ship, 1, ship(0)._2, false)
    else
      checkPrevious(ship, 1, ship(0)._1, true)
  }

  val ship: Ship = List((2,5), (3,5), (4,5), (5,5))
  //val ship: Ship = List((8,9), (8,10))
  //println(validateShip(ship))

  def validatePosition(ship: Ship, field: Field): Boolean = {
    for(x <- ship(0)._1 - 1 to ship(ship.length - 1)._1 + 1;
        y <- ship(0)._2 - 1 to ship(ship.length - 1)._2 + 1
        if (x != -1 && x !=10 && y != -1 && y != 10)) {
      //println(x + "; " + y)

      if(x < -1 || x > 10 || y < -1 || y > 10)
        return false

      if(field(x)(y))
        return false
    }

    true
  }

  println(validatePosition(ship, field))


}
