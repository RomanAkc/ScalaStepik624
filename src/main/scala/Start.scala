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
          checkPrevious(ship, index + 1, ship(index)._2, checkX)
      }
    }

    if(isXOkay)
      checkPrevious(ship, 1, ship(0)._2, false)
    else
      checkPrevious(ship, 1, ship(0)._1, true)
  }

  //val ship: Ship = List((2,5), (3,5), (4,5), (5,5))
  val ship: Ship = List((0,0), (0,1), (0,2), (0,3))
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

  //println(validatePosition(ship, field))
  /*object NilMap {
    def unapply(map: Map[_, _]): Boolean =
      map.isEmpty
  }*/

  def enrichFleet(fleet: Fleet, name: String, ship: Ship): Fleet = {
    fleet + (name -> ship)
  }

  //val fleet = enrichFleet(Map[String, Ship](), "One", ship)
  //println(fleet)

  def markUsedCells(field: Field, ship: Ship): Field = {
    if(ship.length == 0)
      field
    else {
      val x = ship(0)._1
      val y = ship(0)._2
      val vec: Vector[Boolean] = field(x)
      val newVec: Vector[Boolean] = vec.patch(y, Seq(true), 1)
      val newField: Field = field.patch(x, Seq(newVec), 1)
      markUsedCells(newField, ship.tail)
    }
  }

  //val newField = markUsedCells(field, ship)
  //println(newField)

  val game = (field, Map[String, Ship]())
  def tryAddShip(game: Game, name: String, ship: Ship): Game = ship match {
    case ship if !validateShip(ship) => game
    case ship if !validatePosition(ship, game._1) => game
    case ship => (markUsedCells(game._1, ship), enrichFleet(game._2, name, ship))
  }

  //test 1
  /*val ship1: Ship = List((2,5), (3,5), (4,5), (5,5))
  val ship2: Ship = List((9,9))

  val game1: Game = tryAddShip(game, "MillenniumFalcon", ship1)
  val game2: Game = tryAddShip(game1, "Varyag", ship2)

  println(game2._2)*/

  //test2
  /*val ship10: Ship = List((1,6), (1,7), (1,8))
  val ship20: Ship = List((2,5), (3,5), (4,5), (5,5))
  val ship30: Ship = List((9,9))

  val game10: Game = tryAddShip(game, "BlackPearl", ship10)
  val game20: Game = tryAddShip(game10, "MillenniumFalcon", ship20)
  val game30: Game = tryAddShip(game20, "Varyag", ship30)

  println(game30._2)*/
  val countShips = scala.io.StdIn.readLine().toInt

  def readShip(): (String, Ship) = {
    val header: List[String] = scala.io.StdIn.readLine().split(' ').toList
    def readPoints(index: Int, maxIndex: Int, list: List[Point]) : List[Point] = index match {
      case index if index == maxIndex => list.reverse
      case index => {
        val point = scala.io.StdIn.readLine().split(' ')
        readPoints(index + 1, maxIndex, (point(0).toInt, point(1).toInt) +: list)
      }
    }
    (header(0), readPoints(0, header(1).toInt, Nil))
  }

  val firstShip = readShip()
  println(firstShip._1)
  println(firstShip._2)
}
