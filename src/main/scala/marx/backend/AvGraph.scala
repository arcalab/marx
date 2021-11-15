package marx.backend

import marx.Error.Who
import AvGraph.Node
import marx.core.Network.Link
import marx.Error
import marx.core.Rule.Assignment
import marx.core.{Automaton, Connector, Network, Rule, Show, Term}

import scala.annotation.tailrec

/**
  * Builds an availability graph, where states denote what registers are avaliable (defined) and which are not.
  */

case class AvGraph(init:Node, edges: Map[Node,Set[(Rule,Node)]], inv:Set[Term]):
  def +(from:Node, to:Set[(Rule,Node)]) =
    val e2: Map[Node,Set[(Rule,Node)]] = edges + (from->to)
    AvGraph(init,e2,inv)

//  def simplify(): AvGraph =
//    def simp(r: Rule): Rule =
//      Rule(get=Set(),ask=Set(),)

  def toMermaid(name:String,aut:Automaton): String =
    val name2 = if name=="" then "sync" else fix4Mermaid(name)
    val allNodes:Set[Node] = edges.values.flatten.map(_._2).toSet
    val intersectNodes = allNodes.foldRight(init)((n,res)=>n.intersect(res))

    def mkNodeId(n: Node) = s"${name2}_${(n--intersectNodes).toList.sorted.map(fix4Mermaid).mkString("_")}"
    def mkNode(n:Node) = s"${mkNodeId(n)}([ ${(n--intersectNodes).toList.sorted.map(fix4Mermaid).mkString(",")} ])"
    def mkNodeInit =
      val initAut = aut.init.filter(_.t!=Term.unitT)
      mkNode(init).dropRight(3)+
        (if initAut.nonEmpty && (init--intersectNodes).nonEmpty then "<br>" else "") +
        (if initAut.nonEmpty then initAut.map(fixAssgn(_,":=")).mkString(",") else "")+
      " ])"
    def mkLbl(r:Rule): String =
//      Rule(r.get--aut.registers, r.ask--aut.registers, r.und--aut.registers,r.pred,r.eqs,r.upd,r.highlights,r.lbls)
//      r // --LBL--\\GET(in1,in2) PUT(out1,out2)\\preds\\writes
      fixRule(r,aut)
      .toString
//      .replaceAll(";",",")
//      .replaceAll("§","_")
//      .replaceAll("\\|","_")
//      .replaceAll("get\\(","")
//      .replaceAll("\\(","_")
//      .replaceAll("\\)","")
//      .replaceAll("-->","<br>")
//      .replaceAll("\\[","--")
//      .replaceAll("\\]","--<br>")

    val es = for e<-edges; target<-e._2 yield
      s"  ${mkNode(e._1)} --> |${mkLbl(target._1)}| ${mkNode(target._2)}"
    val es2 = for e<-edges if e._2.isEmpty yield
      s"  ${mkNode(e._1)}"
    val fst = s"style ${mkNodeId(init)} fill:#4f4,stroke:#333,stroke-width:4px\n" +
      (if inv.nonEmpty then s"style __inv_${name2}__ fill:#ff4,stroke:#333,stroke-width:2px" else "")
    val inv2 = if inv.nonEmpty
      then inv.map(fixTerm).toList.sorted.mkString(s"  __inv_${name2}__["," <br> ","]; \n")
      else ""
    if edges.isEmpty then "subgraph $name2\n  empty([ ])\n  end"
    else s"subgraph $name2\n  direction TB\n$inv2${(es++es2).mkString("\n")}\n  $fst\n  $mkNodeInit\n  end"


  private def fixRule(r:Rule,aut:Automaton): String =
    def mb[A](x:Iterable[A],f:Iterable[A]=>String) = if x.isEmpty then "" else f(x)
    mb(r.lbls,x=>s"--${x.mkString(",")}--<br>")+
    mb((r.get++r.ask++r.eqs.map(_.v))--aut.registers, x=>
      {val (i,o)=x.partition(r.get++r.ask);
        s" ${i.map(p=>if r.get(p) then fix4Mermaid(p) else fix4Mermaid(p)+"?").mkString(",")} >> ${
          o.map(fix4Mermaid).mkString(",")} <br>"}) +
    mb(r.und--aut.registers , x=>s" und-${x.map(fix4Mermaid).mkString("_")}, ")+
    mb(r.pred, x=> s" ${x.map(fixTerm).mkString(", ")}")+
      (if r.pred.isEmpty && r.und.isEmpty then "" else "<br>")+
    mb(r.eqs, x=> s" ${x.map(fixAssgn(_,"=")).mkString(", ")}")+
    mb(r.upd.filter(_.t!=Term.unitT), ups=>ups.map(fixAssgn(_,":=")).mkString(",") )
  private def fixTerm(t:Term): String = fix4Mermaid(Show(t))
  private def fixAssgn(a:Assignment,op:String): String =
    s"${fix4Mermaid(a.v)}${op}${fixTerm(a.t)}"
  private def fix4Mermaid(s:String) = s
    .replaceAll(";",",")
    .replaceAll("§","_")
    .replaceAll("\\(","_")
    .replaceAll("\\)","_")


object AvGraph:

  type Node = Set[String] // set of registers that are defined

  def apply(n:Network): AvGraph =
    apply(n.toAut)

  def oneMermaid(n:Network): String =
    val a=n.toAut
    val res = "flowchart LR\n"+
      apply(a).toMermaid("Composed",a)
    //println(res)
    res

  def allGraphs(n:Network): Set[AvGraph] =
    collectAut(n).map(a=>apply(a._2))

  def allMermaid(n:Network): String =
    val auts = collectAut(n).map(aut => apply(aut._2).toMermaid(aut._1,aut._2))
    s"flowchart LR\n${auts.mkString("\n")}"

  def apply(a:Automaton): AvGraph =
    val initNode = a.init.map(_.v).intersect(a.registers)
    buildGraph(Set(initNode),AvGraph(initNode,Map(),a.inv))(using a)

  @tailrec
  def buildGraph(nodes:Set[Node],graph: AvGraph)(using a:Automaton): AvGraph =
    nodes.headOption match
      case None => graph
      case Some(node) =>
        if graph.edges contains node then buildGraph(nodes-node,graph)
        else
          val newEdges = a.rs // get rules
            .filter(r=>ready(r,node,a.registers)) // filter active ones
            .map(r=>r -> ((node--r.get)++(r.upd.map(_.v).intersect(a.registers)))) // update node for each one
          val newNodes = newEdges.map(_._2)
          buildGraph((nodes-node)++newNodes,graph+(node,newEdges))


  private def ready(r:Rule,node:Node,regs:Set[String]): Boolean =
    ((r.get++r.ask).intersect(regs) subsetOf node) &&
    !(r.und.exists(node))

  private def collectAut(net:Network): Set[(String,Automaton)] =
    given Who = Who("AGr")
    var auts = for Link(name,terms,inputs,outputs) <- net.links yield
      if !net.connectors.contains(name) then
        Error.encoding(s"Connector not found: '$name' among ${net.connectors.keys.mkString(",")}")
      net.connectors(name) match {
        case Connector.CNet(net2,args,ins,outs) =>
          collectAut(net2)
        case c:Connector.CAut =>
          Set(name -> c.a)
      }
    auts.flatten.toSet
