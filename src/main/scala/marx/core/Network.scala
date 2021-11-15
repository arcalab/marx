package marx.core

import marx.Error.Who
import Connector.CAut
import Network.{Constructor, Link}
import Term.{Fun, IntVal, Interpretation, Var}
import marx.typing.Type.{BaseType, VarType}
import marx.Error.Who
import marx.Prelude
import marx.core.Connector.CAut
import marx.typing.Type

case class Network(data:Map[String,(List[String],List[Constructor])],
                   functions: Map[String,Interpretation],
                   connectors: Map[String,Connector],
                   links: List[Link]):
  def toAut: Automaton = Network.netToAutomaton(this)
  override def toString: String = Show(this) //Show.simple(this) //Show(this)

//inputs: Set[String], outputs: Set[String],
//                   outputs: List[String])

object Network:
  case class Constructor(name:String, args:List[Type])
  case class Link(name:String, terms:List[Term], inputs:List[String], outputs:List[String])

  val empty = Network(Map(),Map(),Map(),Nil)


  private class MutSubst(var s:Int=0,var subst:Map[String,Term]=Map()):
    private def fresh(base:String = "x"):String =
      s+=1
      s"$base§${s-1}"
    def setSubst(subst2:Iterable[(String,Term)]) =
      subst = subst2.toMap
    def getVar(v:String) =
      subst.get(v) match
        case Some(Term.Var(v2)) => v2
        case _ => val v2 = fresh(v); subst += v -> Term.Var(v2); v2
    def getTerm(v:String) =
      subst.get(v) match
        case Some(t) => t
        case _ => val v2 = Term.Var(fresh(v)); subst += v -> v2; v2

  /** Instantiate all automata in a network and compose them into a single automaton. */
  def netToAutomaton(net: Network): Automaton =
    val aut = instantiateAuts(net)
    if aut.isEmpty then Automaton.empty else
      aut.tail.foldRight[Automaton](aut.head)((a1,a2) => a1 * a2).hiding
  //    sys.error("not done yet")


  /** Collect all automata used in a network after instantiating with the right names,
    * ready to be composed. */
  def instantiateAuts(net:Network): List[Automaton] =
    given Who = Who("Netw")
    import marx.Error.debug
    debug(s"Translating net with conn ${net.connectors.keys} - ${net}")
    given subst: MutSubst = new MutSubst()
    val auts = for Link(name,terms,inputs,outputs) <- net.links yield
      if !net.connectors.contains(name) then
        debug(s"NOT FOUND $name in ${net.connectors.keys.mkString(",")}")
      net.connectors(name) match {
        case Connector.CNet(net2,args,ins,outs) =>
          instantiate(CAut(netToAutomaton(net2),args,ins,outs),terms,inputs,outputs)
        case c:Connector.CAut =>
          instantiate(c,terms,inputs,outputs)
      }
    println(s"[Inst] got subst: ${subst.subst}")
    auts

  private def instantiate(c:CAut, iArgs:List[Term], iIns:List[String],iOuts:List[String])(using gen:MutSubst): Automaton =
    // replace args, ins, outs by new terms and ports.
    // replace everything else with a fresh variable.
    val subst:List[(String,Term)] =
      (c.args zip iArgs) :::
      (c.ins zip iIns.map(Var.apply)) :::
      (c.outs zip iOuts.map(Var.apply))
    gen.setSubst(subst)
    instantiate(c.a)

  private def instantiate(a:Automaton)(using gen:MutSubst): Automaton =
    Automaton(
      a.init.map(instantiate),
      a.inv.map(instantiate),
      a.rs.map(r => Rule(r.get.map(getVar),r.ask.map(getVar),r.und.map(getVar),
        r.pred.map(instantiate),r.eqs.map(instantiate),r.upd.map(instantiate),r.highlights,r.lbls)),
      a.inputs.map(getVar),
      a.outputs.map(getVar),
      a.registers.map(getVar),
      a.clocks.map(getVar), // seems to be needed...
      a.args.map(getVar) // seemes to be needed...
    )

  private def instantiate(as:Rule.Assignment)(using gen:MutSubst): Rule.Assignment =
    Rule.Assignment(getVar(as.v),instantiate(as.t))

  private def instantiate(t:Term)(using gen:MutSubst): Term = t match {
    case Var(v) => getTerm(v)
    case Fun(n,terms) => Fun(n,terms.map(instantiate))
    case IntVal(i) => t
  }

  private def getVar(v:String)(using gen:MutSubst): String = gen.getVar(v)
  private def getTerm(v:String)(using gen:MutSubst): Term = gen.getTerm(v)
//    subst.get(v) match
//      case Some(Term.Var(v2)) => v2
//      case _ => gen.fresh
//  private def getTerm(v:String)(using subst:Map[String,Term], gen:MutSubst): Term =
//    subst.get(v) match
//      case Some(t) => t
//      case None => Term.Var(gen.fresh)


  ////////////////
  // experimenting
  ////////////////

  object Examples:
    import Automaton.Examples._
    import Term.~~

    val myData = Map(
      "Planet" -> (Nil, List(
        Constructor("Mercury",Nil),
        Constructor("Venus",Nil),
        Constructor("Earth",Nil),
        Constructor("Mars",Nil))),
      "List" -> (List("a"), List(
        Constructor("Cons",List(VarType("a"),BaseType("List",List(VarType("a"))))),
        Constructor("Nil",Nil)))
    )
    val myConn = Map(
      "fifo" -> CAut(fifo("a","b","m"),Nil,List("a"),List("b")),
      "lossy" -> CAut(lossy("a","b"),Nil,List("a"),List("b")),
      "fifoTrue" -> CAut(fifofull("a","b","n",Fun("True",Nil)),Nil,List("a"),List("b")),
      "fifof" -> CAut(fifofull("a","b","n","v"),List("v"),List("a"),List("b")),
      "sendV" -> CAut(Automaton(
        inv=Set(
          Fun("!=",List("a",Fun("Earth",Nil))),
          Fun("!", List(Fun("is§Venus",List("a")))) ),
        rs = Set("a" ~~ Fun("Mars",Nil)),
        outputs = Set("a"), clocks=Set("t")
      ),Nil,Nil,List("a"))
    )
    val myLinks = List(
      Link("sendV",Nil,List(),List("x")),
      Link("fifo",Nil,List("x"),List("y")),
      Link("fifo",Nil,List("y"),List("z")),
      Link("fifof",List(IntVal(51)),List("f"),List("g")),
      Link("fifoTrue",Nil,List("v"),List("w"))
    )

    val myNet = Network(myData,Prelude.interpretations,myConn,myLinks)


