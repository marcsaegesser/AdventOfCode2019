package advent

import scala.math.Integral.Implicits._

object IntMachine {
  case class Machine(ip: Int, state: State, memory: Vector[Int], input: List[Int],  output: List[Int])

  sealed trait State
  case object Running extends State
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
  val HALT      = 99

  sealed trait ParameterMode
  case object PositionMode extends ParameterMode
  case object ImmediateMode extends ParameterMode

  sealed trait Instruction { val modes: Vector[ParameterMode] }
  case class AddInstruction(modes: Vector[ParameterMode]) extends Instruction
  case class MultInstruction(modes: Vector[ParameterMode]) extends Instruction
  case class InInstruction(modes: Vector[ParameterMode]) extends Instruction
  case class OutInstruction(modes: Vector[ParameterMode]) extends Instruction
  case class JumpTrueInstruction(modes: Vector[ParameterMode]) extends Instruction
  case class JumpFalseInstruction(modes: Vector[ParameterMode]) extends Instruction
  case class LessThanInstruction(modes: Vector[ParameterMode]) extends Instruction
  case class EqualsInstruction(modes: Vector[ParameterMode]) extends Instruction
  case class HaltInstruction(modes: Vector[ParameterMode]) extends Instruction

  def runMachine(m: Machine): Machine = {
    m.state match {
      case Halted  => m
      case Running => runMachine(step(m))
    }
  }

  def step(machine: Machine): Machine = {
    machine match { case Machine(ip, state, m, i, o) if state == Running =>
      parseInstruction(ip, m) match {
        case i@AddInstruction(pm)       => evalADD(i, machine)
        case i@MultInstruction(pm)      => evalMULT(i, machine)
        case i@InInstruction(pm)        => evalIN(i, machine)
        case i@OutInstruction(pm)       => evalOUT(i, machine)
        case i@JumpTrueInstruction(pm)  => evalJumpTrue(i, machine)
        case i@JumpFalseInstruction(pm) => evalJumpFalse(i, machine)
        case i@LessThanInstruction(pm)  => evalLessThan(i, machine)
        case i@EqualsInstruction(pm)    => evalEquals(i, machine)
        case i@HaltInstruction(pm)      => evalHALT(i, machine)
      }
    }
  }

  def parseInstruction(ip: Int, memory: Vector[Int]): Instruction = {
    (memory(ip) /% 100) match {
      case (modes, ADD)        => AddInstruction(parseModes(modes, 3))
      case (modes, MULT)       => MultInstruction(parseModes(modes, 3))
      case (modes, IN)         => InInstruction(parseModes(modes, 1))
      case (modes, OUT)        => OutInstruction(parseModes(modes, 1))
      case (modes, JMP_TRUE)   => JumpTrueInstruction(parseModes(modes, 2))
      case (modes, JMP_FALSE)  => JumpFalseInstruction(parseModes(modes, 2))
      case (modes, LT)         => LessThanInstruction(parseModes(modes, 3))
      case (modes, EQ)         => EqualsInstruction(parseModes(modes, 3))
      case (modes, HALT)       => HaltInstruction(parseModes(modes, 0))
      case _                   => throw new Exception("Invalid instruction")
    }
  }

  def parseModes(modes: Int, length: Int): Vector[ParameterMode] = {
    def parseMode(m: Char): ParameterMode =
      m match {
        case '0' => PositionMode
        case '1' => ImmediateMode
      }

    modes.toString
      .reverse
      .padTo(length, '0')
      .map(parseMode)
      .toVector
  }

  def readParam(value: Int, mode: ParameterMode, memory: Vector[Int]): Int =
    mode match {
      case PositionMode  => memory(value)
      case ImmediateMode => value
    }

  def readParam(base: Int, i: Int, modes: Vector[ParameterMode], memory: Vector[Int]): Int =
    modes(i-1) match {
      case PositionMode => memory(memory(base+i))
      case ImmediateMode => memory(base+i)
    }

  def evalADD(i: Instruction, m: Machine): Machine = {
    val a = readParam(m.ip, 1, i.modes, m.memory)
    val b = readParam(m.ip, 2, i.modes, m.memory)
    val dest = m.memory(m.ip+3) // Don't dereference pointer

    m.copy(memory = m.memory.updated(dest, a+b), ip=m.ip+4)
  }

  def evalMULT(i: Instruction, m: Machine): Machine = {
    val a = readParam(m.memory(m.ip+1), i.modes(0), m.memory)
    val b = readParam(m.memory(m.ip+2), i.modes(1), m.memory)
    val dest = m.memory(m.ip+3) // Don't dereference pointer

    m.copy(memory = m.memory.updated(dest, a*b), ip=m.ip+4)
  }

  def evalIN(i: Instruction, m: Machine): Machine = {
    val in = m.input.head
    val dest = m.memory(m.ip+1)  // Don't dereference pointer

    m.copy(memory=m.memory.updated(dest, in), input=m.input.tail, ip=m.ip+2)
  }

  def evalOUT(i: Instruction, m: Machine): Machine = {
    val out = readParam(m.memory(m.ip+1), i.modes(0), m.memory)

    m.copy(output = out +: m.output, ip=m.ip+2)
  }

  def evalJumpTrue(i: Instruction, m: Machine): Machine = {
    val test = readParam(m.ip, 1, i.modes, m.memory)
    val addr = readParam(m.ip, 2, i.modes, m.memory)

    if(test != 0) m.copy(ip=addr)
    else         m.copy(ip=m.ip+3)
  }

  def evalJumpFalse(i: Instruction, m: Machine): Machine = {
    val test = readParam(m.memory(m.ip+1), i.modes(0), m.memory)
    val addr = readParam(m.memory(m.ip+2), i.modes(1), m.memory)

    if(test == 0) m.copy(ip=addr)
    else         m.copy(ip=m.ip+3)
  }

  def evalLessThan(i: Instruction, m: Machine): Machine = {
    val a = readParam(m.memory(m.ip+1), i.modes(0), m.memory)
    val b = readParam(m.memory(m.ip+2), i.modes(1), m.memory)
    val dest = m.memory(m.ip+3) // Don't dereference pointer

    if(a < b) m.copy(memory = m.memory.updated(dest, 1), ip=m.ip+4)
    else      m.copy(memory = m.memory.updated(dest, 0), ip=m.ip+4)
  }

  def evalEquals(i: Instruction, m: Machine): Machine = {
    val a = readParam(m.memory(m.ip+1), i.modes(0), m.memory)
    val b = readParam(m.memory(m.ip+2), i.modes(1), m.memory)
    val dest = m.memory(m.ip+3) // Don't dereference pointer

    if(a == b) m.copy(memory = m.memory.updated(dest, 1), ip=m.ip+4)
    else      m.copy(memory = m.memory.updated(dest, 0), ip=m.ip+4)
  }

  def evalHALT(i: Instruction, m: Machine): Machine = {
    m.copy(state = Halted)
  }

  def initializeMachine(memory: Vector[Int], n: Int, v: Int): Machine =
    Machine(0, Running, memory.updated(1, n).updated(2, v), List.empty[Int], List.empty[Int])

  def initializeMachine(memory: Vector[Int], input: List[Int]) =
    Machine(0, Running, memory, input, List.empty[Int])
}
