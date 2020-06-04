package advent

object Day23 {
  import IntMachine._

  case class Network(
    machines: Vector[Machine],
    buffers: Map[Int, List[Long]],
    nat: Option[List[Long]],
    natSent: List[List[Long]]
  )

  def mkNetwork(memory: Memory): Network =
    Network(
      Vector.fill(50)(initializeMachine(memory, List.empty[Long])),
      (0 until 50).map(i => (i, List(i.toLong))).toMap,
      Option.empty[List[Long]],
      List.empty[List[Long]]
    )

  def runToCompletion(network: Network): Network = {
    val next = runIteration(network)
    if(next.nat.isDefined) next
    else                   runToCompletion(next)
  }

  def runToCompletion2(network: Network): Network = {
    val next = runIteration(network)
    next.natSent match {
      case a :: b :: rest if a == b => next
      case _                        => runToCompletion2(next)
    }
  }

  def runToCompletion2Watch(network: Network): Network = {
    val next = runIteration(network)
    println(s"next.nat=${next.nat}, next.natSent=${next.natSent.take(10)}")
    next.natSent match {
      case a :: b :: _ if a == b => next
      case _                        => runToCompletion2Watch(next)
    }
  }

  def runIteration(network: Network): Network = {

    val (bufs, natSent) =
      if(!network.buffers.isEmpty)   (network.buffers, network.natSent)
      else {
        network.nat match {
          case Some(l) => (Map(0 -> l), l :: network.natSent)
          case None    => (network.buffers, network.natSent)
        }
      }

    val next =
      network.machines.zipWithIndex.map { case (m, i) =>
        val input = bufs.getOrElse(i, List(-1L))
        provideInput(m, input)
      }

    val (output, ms) = next.map(m => takeOutput(runMachine(m))).unzip

    val buffers =
      output.foldLeft(Map.empty[Int, List[Long]]) { case (accum, o) =>
        o.sliding(3, 3).foldLeft(accum) {
          case (a, d :: x :: y :: Nil) => a.updatedWith(d.toInt) {
            case Some(l) => Some(l ++ List(x, y))
            case None    => Some(List(x, y))
          }
          case (a, l) => throw new Exception("Malformed message")
        }
      }

    val nat =
      buffers.get(255) match {
        case Some(l) => Some(l.reverse.take(2).reverse)
        case None    => network.nat
      }

    Network(ms, buffers.removed(255), nat, natSent)
  }

  val inputFile = "data/Day23.txt"
}
