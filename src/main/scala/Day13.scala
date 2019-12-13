package advent

object Day13 {
  import IntMachine._

  def day13(): Unit = {
    val memory = readMemory(inputFile)
    println(s"Day13.part1 = ${part1(memory)}")
    println(s"Day13.part2 = ${part2(memory)}")
  }

  def part1(memory: Memory): Int = {
    val (o, m) = takeOutput(runMachine(initializeMachine(memory, List())))
    o.sliding(3, 3).collect { case x :: y :: 2 :: Nil => (x, y) }.size
  }

  def part2(memory: Memory): Long = {
    val solidPaddle = (1522L to 1561L).map(i => i -> 3L)
    val newMemory = memory ++ solidPaddle
    val (s, m) = initialize(newMemory)
    val (s2, _) = playGame(s, m)
    s2.score
  }

  def initialize(memory: Memory): (GameState, Machine) = {
    val (o, m) = takeOutput(runMachine(setMemory(initializeMachine(memory, List()), 0, 2)))
    (updateState(initialState, o), m)
  }

  def runIteration(state: GameState, joystick: JoystickPosition, machine: Machine): (GameState, Machine) = {
    val (o, m) = takeOutput(runMachine(provideInput(machine, List(joystick))))
    val nextState = updateState(state, o)

    (nextState, m)
  }

  def runIterationAndPrint(state: GameState, joystick: JoystickPosition, machine: Machine): (GameState, Machine) = {
    val (o, m) = takeOutput(runMachine(provideInput(machine, List(joystick))))
    val nextState = updateState(state, o)
    print(AnsiCodes.EraseScreen)
    print(AnsiCodes.CursorHome)
    println(s"(${state.ball.x}, ${state.ball.y}), (${state.paddle.x}, ${state.paddle.y})")
    println(showGame(state))

    (nextState, m)
  }

  def playGame(initialState: GameState, initialMachine: Machine): (GameState, Machine) = {
    def helper(n: Int, state: GameState, joystick: JoystickPosition, machine: Machine): (GameState, Machine) = {
      if(machine.state == Halted)      (state, machine)  // We lost
      else if(countBlocks(state) == 0) (state, machine)  // We won
      else {                                            // Play the next iteration
        val (s, m) = runIteration(state, joystick, machine)
        helper(n+1, s, Neutral, m)
      }
    }

    helper(0, initialState, Neutral, initialMachine)
  }

  def playGameVisual(initialState: GameState, initialMachine: Machine): (GameState, Machine) = {
    def helper(n: Int, state: GameState, joystick: JoystickPosition, machine: Machine): (GameState, Machine) = {
      print(AnsiCodes.EraseScreen)
      print(AnsiCodes.CursorHome)
      println(s"$n - (${state.ball.x}, ${state.ball.y}), (${state.paddle.x}, ${state.paddle.y})")
      print(showGame(state))
      Thread.sleep(10)  // Just so you can see something
      if(machine.state == Halted)      (state, machine)  // We lost
      else if(countBlocks(state) == 0) (state, machine)  // We won
      else {                                            // Play the next iteration
        val (s, m) = runIteration(state, joystick, machine)
        helper(n+1, s, Neutral, m)
      }
    }

    helper(0, initialState, Neutral, initialMachine)
  }

  def countBlocks(state: GameState): Int =
    state.display.collect { case (k, Block) => 1 }.size

  case class Coord(x: Long, y: Long) {
    def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
  }

  sealed trait Tile
  case object Empty         extends Tile
  case object Wall          extends Tile
  case object Block         extends Tile
  case object Paddle        extends Tile
  case object Ball          extends Tile
  case class Score(s: Long) extends Tile

  sealed trait Direction
  case object Left  extends Direction
  case object Right extends Direction

  type JoystickPosition = Long
  val ToLeft = -1L
  val Neutral = 0L
  val ToRight = 1L

  case class GameState(display: Map[Coord, Tile], score: Long, ball: Coord, paddle: Coord)
  val initialState = GameState(Map.empty[Coord, Tile], 0, Coord(0, 0), Coord(0, 0))

  def showGame(state: GameState): String = {
    val (Coord(minX, minY), Coord(maxX, maxY)) = extent(state)

    def showRow(y: Long): String =
      (minX to maxX).foldLeft(new StringBuilder()) { case (s, x) =>
        s.append(
          state.display.get(Coord(x, y)) match {
            case None           => ' '
            case Some(Score(_)) => ' '
            case Some(Empty)    => ' '
            case Some(Wall)     => '#'
            case Some(Block)    => 'B'
            case Some(Paddle)   => '_'
            case Some(Ball)     => '*'
          })
      }.mkString

    state.score.toString ++ "\n" ++
    (for {
      y <- minY to maxY
      r =  showRow(y)
    } yield r).mkString("\n")
  }

  def extent(state: GameState): (Coord, Coord) =
    (
      Coord(state.display.keys.minBy(_.x).x, state.display.keys.minBy(_.y).y),
      Coord(state.display.keys.maxBy(_.x).x, state.display.keys.maxBy(_.y).y))

  def updateState(state: GameState, updates: List[Long]): GameState =
    updates.sliding(3, 3).foldLeft(state) { case (st, u) =>
      u match {
        case -1 :: 0 :: s :: Nil => st.copy(score = s)
        case x :: y :: 0 :: Nil  => st.copy(display = st.display.updated(Coord(x, y), Empty))
        case x :: y :: 1 :: Nil  => st.copy(display = st.display.updated(Coord(x, y), Wall))
        case x :: y :: 2 :: Nil  => st.copy(display = st.display.updated(Coord(x, y), Block))
        case x :: y :: 3 :: Nil  => st.copy(display = st.display.updated(Coord(x, y), Paddle), paddle = Coord(x, y))
        case x :: y :: 4 :: Nil  => st.copy(display = st.display.updated(Coord(x, y), Ball),   ball   = Coord(x, y))
        case _                   => throw new Exception("Invalid game state")
      }
    }

  // def parseDisplay(display: List[Long]): GameState = {
  //   val data =
  //     display.sliding(3, 3).map {
  //       case -1 :: 0 :: s :: Nil => (Coord(-1, 0), Score(s))
  //       case x :: y :: 0 :: Nil => (Coord(x, y) -> Empty)
  //       case x :: y :: 1 :: Nil => (Coord(x, y) -> Wall)
  //       case x :: y :: 2 :: Nil => (Coord(x, y) -> Block)
  //       case x :: y :: 3 :: Nil => (Coord(x, y) -> Paddle)
  //       case x :: y :: 4 :: Nil => (Coord(x, y) -> Ball)
  //       case _                  => throw new Exception("Invalid game state")
  //     }.toList

  //   val (List((Coord(_, _), Score(s))), d) = data.partition { case (c, t) => c.x < 0 }

  //   GameState(d.toMap, s)
  // }

  val inputFile = "data/Day13.txt"
  val inputFile2 = "data/Day13-2.txt"
}
