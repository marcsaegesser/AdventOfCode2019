package advent

import scala.math.Integral.Implicits._

object IntMachine {
  type Memory = Map[Long, Long]

  case class Machine(ip: Long, state: State, memory: Memory, relativeBase: Long, input: List[Long],  output: List[Long])


  def initializeMachine(memory: Memory, n: Int, v: Int): Machine =
    Machine(0, Running, memory.updated(1, n.toLong).updated(2, v.toLong), 0, List.empty[Long], List.empty[Long])

  def initializeMachine(memory: Memory, input: List[Long]): Machine =
    Machine(0, Running, memory, 0, input, List.empty[Long])

  def initializeMachine(s: String, input: List[Long]): Machine =
    initializeMachine(parseMemory(s), input)

  def loadMachine(f: String, input: List[Long]) =
    initializeMachine(readMemory(f), input)

  sealed trait State
  case object Running extends State
  case object Waiting extends State
  case object Halted  extends State

  type OpCode = Int
  val ADD       = 1
  val MULT      = 2
  val IN        = 3
  val OUT       = 4
  val JMP_TRUE  = 5
  val JMP_FALSE = 6
  val LT        = 7
  val EQ        = 8
  val SET_RB    = 9
  val HALT      = 99

  sealed trait ParameterMode
  case object PositionMode  extends ParameterMode
  case object ImmediateMode extends ParameterMode
  case object RelativeMode  extends ParameterMode

  sealed trait Instruction { val modes: Vector[ParameterMode] }
  case class AddInstruction(modes: Vector[ParameterMode])             extends Instruction
  case class MultInstruction(modes: Vector[ParameterMode])            extends Instruction
  case class InInstruction(modes: Vector[ParameterMode])              extends Instruction
  case class OutInstruction(modes: Vector[ParameterMode])             extends Instruction
  case class JumpTrueInstruction(modes: Vector[ParameterMode])        extends Instruction
  case class JumpFalseInstruction(modes: Vector[ParameterMode])       extends Instruction
  case class LessThanInstruction(modes: Vector[ParameterMode])        extends Instruction
  case class EqualsInstruction(modes: Vector[ParameterMode])          extends Instruction
  case class SetRelativeBaseInstruction(modes: Vector[ParameterMode]) extends Instruction
  case class HaltInstruction(modes: Vector[ParameterMode])            extends Instruction

  def runMachine(m: Machine): Machine = {
    m.state match {
      case Halted  => m
      case Waiting => m
      case Running => runMachine(step(m))
    }
  }

  def provideInput(m: Machine, input: List[Long]): Machine =
    m.state match {
      case Halted  => m.copy(input = m.input ++ input)  // Still halted
      case Waiting => m.copy(state=Running, input = m.input ++ input)
      case Running => m.copy(input = m.input ++ input)
    }

  def takeOutput(m: Machine): (List[Long], Machine) =
    (m.output.reverse, m.copy(output = List.empty[Long]))

  def setMemory(m: Machine, addr: Long, value: Long): Machine =
    m.copy(memory = m.memory.updated(addr, value))

  def step(machine: Machine): Machine = {
    machine match { case Machine(ip, state, m, b, i, o) if state == Running =>
      parseInstruction(ip, m) match {
        case i@AddInstruction(pm)             => evalADD(i, machine)
        case i@MultInstruction(pm)            => evalMULT(i, machine)
        case i@InInstruction(pm)              => evalIN(i, machine)
        case i@OutInstruction(pm)             => evalOUT(i, machine)
        case i@JumpTrueInstruction(pm)        => evalJumpTrue(i, machine)
        case i@JumpFalseInstruction(pm)       => evalJumpFalse(i, machine)
        case i@LessThanInstruction(pm)        => evalLessThan(i, machine)
        case i@EqualsInstruction(pm)          => evalEquals(i, machine)
        case i@SetRelativeBaseInstruction(pm) => evalSetRelativeBase(i, machine)
        case i@HaltInstruction(pm)            => evalHALT(i, machine)
      }
    }
  }

  def parseInstruction(ip: Long, memory: Memory): Instruction = {
    (memory(ip) /% 100) match {
      case (modes, ADD)        => AddInstruction(parseModes(modes, 3))
      case (modes, MULT)       => MultInstruction(parseModes(modes, 3))
      case (modes, IN)         => InInstruction(parseModes(modes, 1))
      case (modes, OUT)        => OutInstruction(parseModes(modes, 1))
      case (modes, JMP_TRUE)   => JumpTrueInstruction(parseModes(modes, 2))
      case (modes, JMP_FALSE)  => JumpFalseInstruction(parseModes(modes, 2))
      case (modes, LT)         => LessThanInstruction(parseModes(modes, 3))
      case (modes, EQ)         => EqualsInstruction(parseModes(modes, 3))
      case (modes, SET_RB)     => SetRelativeBaseInstruction(parseModes(modes, 1))
      case (modes, HALT)       => HaltInstruction(parseModes(modes, 0))
      case _                   => throw new Exception("Invalid instruction")
    }
  }

  def parseModes(modes: Long, length: Int): Vector[ParameterMode] = {
    def parseMode(m: Char): ParameterMode =
      m match {
        case '0' => PositionMode
        case '1' => ImmediateMode
        case '2' => RelativeMode
      }

    modes.toString
      .reverse
      .padTo(length, '0')
      .map(parseMode)
      .toVector
  }

  def readParam(machine: Machine, offset: Int, modes: Vector[ParameterMode]): Long =
    machine match { case Machine(ip, _, m, rb, _, _) =>
      modes(offset-1) match {
        case PositionMode  => m(m(ip + offset))
        case ImmediateMode => m(ip + offset)
        case RelativeMode  => m(rb + m(ip+offset))
      }
    }

  def getParamAddr(machine: Machine, offset: Int, modes: Vector[ParameterMode]): Long =
    machine match { case (Machine(ip, _, m, rb, _, _)) =>
      modes(offset-1) match {
        case PositionMode  => m(ip + offset)
        case ImmediateMode => throw new Exception("Invalid memory access")
        case RelativeMode  => rb + m(ip+offset)
      }
    }

  def evalADD(i: Instruction, m: Machine): Machine = {
    val a = readParam(m, 1, i.modes)
    val b = readParam(m, 2, i.modes)
    val dest = getParamAddr(m, 3, i.modes) // Don't dereference pointer
    // val dest = m.memory(m.ip+3) // Don't dereference pointer

    m.copy(memory = m.memory.updated(dest, a+b), ip=m.ip+4)
  }

  def evalMULT(i: Instruction, m: Machine): Machine = {
    val a = readParam(m, 1, i.modes)
    val b = readParam(m, 2, i.modes)
    val dest = getParamAddr(m, 3, i.modes) // Don't dereference pointer

    m.copy(memory = m.memory.updated(dest, a*b), ip=m.ip+4)
  }

  def evalIN(i: Instruction, m: Machine): Machine = {
    m.input match {
      case Nil    =>
        m.copy(state=Waiting)
      case h :: t =>
        val dest = getParamAddr(m, 1, i.modes)  // Don't dereference pointer
        m.copy(memory=m.memory.updated(dest, h), input=t, ip=m.ip+2)
    }
  }

  def evalOUT(i: Instruction, m: Machine): Machine = {
    val out = readParam(m, 1, i.modes)

    m.copy(output = out +: m.output, ip=m.ip+2)
  }

  def evalJumpTrue(i: Instruction, m: Machine): Machine = {
    val test = readParam(m, 1, i.modes)
    val addr = readParam(m, 2, i.modes)

    if(test != 0) m.copy(ip=addr)
    else         m.copy(ip=m.ip+3)
  }

  def evalJumpFalse(i: Instruction, m: Machine): Machine = {
    val test = readParam(m, 1, i.modes)
    val addr = readParam(m, 2, i.modes)

    if(test == 0) m.copy(ip=addr)
    else         m.copy(ip=m.ip+3)
  }

  def evalLessThan(i: Instruction, m: Machine): Machine = {
    val a = readParam(m, 1, i.modes)
    val b = readParam(m, 2, i.modes)
    val dest = getParamAddr(m, 3, i.modes)

    if(a < b) m.copy(memory = m.memory.updated(dest, 1), ip=m.ip+4)
    else      m.copy(memory = m.memory.updated(dest, 0), ip=m.ip+4)
  }

  def evalEquals(i: Instruction, m: Machine): Machine = {
    val a = readParam(m, 1, i.modes)
    val b = readParam(m, 2, i.modes)
    val dest = getParamAddr(m, 3, i.modes)

    if(a == b) m.copy(memory = m.memory.updated(dest, 1), ip=m.ip+4)
    else      m.copy(memory = m.memory.updated(dest, 0), ip=m.ip+4)
  }

  def evalSetRelativeBase(i: Instruction, m: Machine): Machine = {
    val rb = readParam(m, 1, i.modes)
    m.copy(relativeBase = m.relativeBase+rb, ip=m.ip+2)
  }

  def evalHALT(i: Instruction, m: Machine): Machine = {
    m.copy(state = Halted)
  }

  def readMemory(f: String): Memory =
    io.Source.fromFile(f)
      .getLines().flatten
      .mkString
      .split(",")
      .zipWithIndex
      .map { case (s, i) => (i.toLong, s.toLong) }  // Swap the index and value and convert both to Long
      .toMap
      .withDefaultValue(0)

  def parseMemory(s: String): Memory =
    s.split(",")
      .zipWithIndex
      .map { case (s, i) => (i.toLong, s.toLong) }  // Swap the index and value and convert both to Long
      .toMap
      .withDefaultValue(0)

}
