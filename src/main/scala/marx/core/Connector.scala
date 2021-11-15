package marx.core

sealed abstract class Connector:
  val args: List[String]
  val ins: List[String]
  val outs: List[String]

object Connector:
  case class CNet(n:Network  , args: List[String], ins:List[String], outs:List[String]) extends Connector
  case class CAut(a:Automaton, args: List[String], ins:List[String], outs:List[String]) extends Connector
