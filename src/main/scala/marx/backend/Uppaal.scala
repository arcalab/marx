package marx.backend

import marx.core.Rule.Assignment
import marx.core.Term.{Fun, Var}
import marx.core.{Automaton, Network, Rule, Term}
import marx.Error.Who
import marx.{Error, Prelude}
import marx.typing.Type.BaseType
import marx.typing.{Infer, MutTypeCtxt, Type}

// Should only support data declarations at the top level! (no mechanism for hierarchical scope of overriden names)

object Uppaal :
  given Who = Who("Upp")

  def autToUppaal: String =
    """<?xml version="1.0" encoding="utf-8"?>
      |<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>
      |<nta>
      |	<declaration>
      |  XXX
      | </declaration>
      |	<template>
      |		<name>XXX</name>
      |		<declaration>XXX</declaration>
      |		<location id="idXXX" x="535" y="17">
      |			<name x="493" y="34">LabelXXX</name>
      |		</location>
      |  	<init ref="idXXX"/>
      |		<transition>
      |			<source ref="idXXX"/>
      |			<target ref="idXXX"/>
      |			<label kind="guard" x="501" y="-34"> XXX </label>
      |			<label kind="synchronisation" x="833" y="93"> XXX </label>
      |			<label kind="assignment" x="833" y="110"> XXX </label>
      |  </transition>
      | </template>
      | <system> XXX
      | </system>
      |	<queries>
      |		<query>
      |			<formula>XXX</formula>
      |			<comment>XXX</comment>
      |		</query>
      |  </queries>
      |</nta>""".stripMargin

  private case class Edge(sel:String="",guard:String="",sync:String="",upd:String=""):
    override def toString: String =
      (if sel.nonEmpty then s"\n  sel: $sel" else "") ++
      (if guard.nonEmpty then s"\n  guard: $guard" else "") ++
      (if sync.nonEmpty then s"\n  sync: $sync" else "") ++
      (if upd.nonEmpty then s"\n  upd: $upd" else "")

  def netToUppaals(net:Network): String =
    val auts = Network.instantiateAuts(net) // todo: error if connectors include data or constants!
    given ctx:MutTypeCtxt = new MutTypeCtxt(functions = Prelude.functions)
    val x = Infer.inferType(net) // typecheck top-level variables, also used by the automata
    for a<-auts do Infer.inferType(a) // collect type constraints, adding to the top-level variables
    Infer.unify // unify types
    println(s"temp auts: $ctx")
    //for a<-auts do println(s"$a\n---")
    val (autUps,usedTypes) = (for a<-auts yield autToUppaal(a,ctx)).toSet.unzip
    val dataDecl = getData(usedTypes.flatten,net.data)
//    println(s">>> data: ${datas.flatten.mkString(";\n")}")
    val data = (s">>> data: $dataDecl")
    val autStrs = for a<-autUps yield(s"$a\n--")
    data + autStrs.mkString("\n")


  def netToUppaal(net:Network): String =
    val (aut,typeCtx) = Infer.typeAut(net) // flatten and typecheck automaton
    val (autUpp,usedTypes) = autToUppaal(aut, typeCtx)  // convert the flattened automata into an UPPAAL automaton (core)
    val dataDecl = getData(usedTypes,net.data)
    s">>> data: $dataDecl\n$autUpp"

  def autToUppaal(aut:Automaton,typeCtx: MutTypeCtxt): (String,Set[String]) =
    val (init,usedTypes1) = getInit(aut,typeCtx)
    val (rules,usedTypes2) = getRules(aut,typeCtx)
    val (inv,usedTypes3) = getInv(aut,typeCtx)
    println(s"ins: ${aut.inputs.map(x=>s"${fixVar(x)}:${typeCtx.getConcreteType(x)}").mkString(",")}; outs: ${
      aut.outputs.map(x=>s"${fixVar(x)}:${typeCtx.getConcreteType(x)}").mkString(",")}")
    val autUpp = s">>> init: $init\n>>> inv $inv\n>>> rules: ${rules.mkString("\n  +++")}"
    (autUpp, usedTypes1++usedTypes2++usedTypes3)

  def getData(usedTypes: Set[String], dataDecl: Map[String,(List[String],List[Network.Constructor])]): String =
    val x = for uTyp <- usedTypes yield
      uTyp match
        case "Int" => ""// s"typedef int[2] Int;" //
        case "Bool" => ""//s"typedef bool[2] Bool;'"//s"typedef int[-1,1] Bool;" // und | False | True
        case "Unit" => "typedef int[0,0] Unit; const Unit UnitC = 0;" // und | ()
        case _ =>
          if !dataDecl.contains(uTyp) || dataDecl(uTyp)._1.nonEmpty || dataDecl(uTyp)._2.exists(_.args.nonEmpty)
          then Error.encoding(s"Cannot use data type '${uTyp} - complex or missing data type")
          else
            // uTyp = constr1 | constr2 | ... -> typedef int[0,{constr.size-1}] uTyp; const uTyp constr(0) - 0; ...
            val constrs = dataDecl(uTyp)._2.map(_.name).zipWithIndex.map((s,i)=>s"const $uTyp $s = $i;")
//            s"typedef int[0,${constrs.size}] ${uTyp}_;\ntypedef ${uTyp}_[2] ${uTyp}\n${constrs.mkString("\n")}"
            s"typedef int[0,${constrs.size}] ${uTyp};\n${constrs.mkString("\n")}"
    x.mkString("\n")


  private def getInv(a:Automaton,types:MutTypeCtxt): (String,Set[String]) =
    val (x,y) = (for term <- a.inv yield getTerm(term,types)(using Set())).unzip
    (x.mkString(" && "),y.flatten)

  // generates the declaration and initialisation of variables, and also returns
  // the names of the data types that were used.
  def getInit(a:Automaton,types:MutTypeCtxt): (String,Set[String]) =
    val x = for (asgm <- a.init) yield
      getInitAssgm(asgm,types)
    val (ini,types2) = x.unzip
    val regs = a.registers -- a.init.map(_.v) -- a.clocks
    val regs2 = for m <- regs yield getInit(m,types)
    val clocks = a.clocks.map(c=>s"clock ${fixVar(c)};").mkString("\n")
    ((regs2++ini+clocks).mkString("\n"),types2.flatten)

  private def getInitAssgm(a:Assignment,types:MutTypeCtxt): (String,Set[String]) =
    if !isConcreteTerm(a.t) then
      Error.encoding(s"Cannot assign a term with variables: ${a.v} := ${a.t}")
    val t = types.getConcreteType(a.v)
    val (e,ts2) = getTerm(a.t,types)(using Set())
    (s"bool ${fixVar(a.v)}_set = true; ${fixType(t)} ${fixVar(a.v)} = $e;", ts2 + t.toString)

  private def getAssgm(a:Assignment,clocks:Set[String],types:MutTypeCtxt)(using ins:Set[String]): (String,Set[String]) =
    val set = if clocks(a.v) then "" else s"${fixVar(a.v)}_set = true; "
    val (e,ts2) = getTerm(a.t,types)
    (s"$set${fixVar(a.v)} = ${getTerm(a.t,types)._1};" , ts2)

  private def isConcreteTerm(term: Term): Boolean = term match {
    case Var(_) => false
    case Fun(_, l) => l.forall(isConcreteTerm)
    case Term.IntVal(_) => true
  }

  private def getInit(v:String,types:MutTypeCtxt): String =
    val t = types.getConcreteType(v)
    s"${fixType(t)} ${fixVar(v)}; bool ${fixVar(v)}_set = false;"

  private def getTerm(t:Term, types:MutTypeCtxt)(using ins:Set[String]): (String,Set[String]) = t match
    case Var(name) =>  (fixVar(if ins(name) then s"${name}_in" else name),Set(types.getConcreteType(name).toString))
    case Fun("()",Nil) => ("0",Set(Prelude.unitType.name))
    case Fun("True",Nil) => ("true",Set(Prelude.boolType.name))
    case Fun("False",Nil) => ("false",Set(Prelude.boolType.name))
    case Fun(name,Nil) => (name,Set(types.functions(name)._2.toString))
    case Fun(name, t1::t2::Nil) if name.forall("+-><!%/*=|&".toSet)=>
      val myType = types.functions(name)._2.toString
      val (t1_,e1) = getTermMP(t1,types)
      val (t2_,e2) = getTermMP(t2,types)
      (s"$t1_ $name $t2_", e1 ++ e2 + myType)
    case Fun(name, terms) =>
      val t2 = types.functions(name)._2.toString
      val (terms2,typ2) = terms.map(x => getTerm(x,types)).toSet.unzip
      (s"$name(${terms2.mkString(",")})", typ2.flatten + t2)
    case Term.IntVal(i) => (i.toString, Set(Prelude.intType.name))

  private def getTermMP(t:Term,ts:MutTypeCtxt)(using ins:Set[String]) =
    val (t2,ts2) = getTerm(t,ts)
    t match
      case Fun(_,_::_::Nil) => (s"($t2)",ts2)
      case _ => (t2,ts2)


  private def getRules(aut: Automaton, ctxt: MutTypeCtxt): (Set[Edge],Set[String]) =
    val (x,usedTs) = (for r <- aut.rs yield getRule(r,aut,ctxt)).unzip
    (x,usedTs.flatten)

  private def getRule(r:Rule, a:Automaton, ctxt:MutTypeCtxt): (Edge,Set[String]) =
    //println(s"getRule $r with ${a.inputs}")
    val allRead = r.get++r.ask
    val (regRead,portsRead) = allRead.partition(a.registers)
    //val regWritten = r.upd.map(_.v)
    val portsWritten = r.eqs.map(_.v)
    val (predTerms,usedTypes1) = r.pred.map(getTerm(_,ctxt)(using a.inputs)).unzip
    val (eqsTerms,usedTypes2) = r.eqs.map(x=>(x.v -> getTerm(x.t,ctxt)(using a.inputs))).map(x=>((x._1,x._2._1),x._2._2)).unzip
    val (updAssg,usedTypes3) = r.upd.map(getAssgm(_,a.clocks,ctxt)(using a.inputs)).unzip
    val guards = regRead.filter(!a.clocks(_)).map(x=>s"${fixVar(x)}_set") ++ predTerms.map("("+_+")")
    /// todo: FIX - get of ports has no "set", value in channel "x" comes as "x[x_in]" and is used as "x_in"
    val (getReg,getPort) = r.get.partition(a.registers)
    val upd = updAssg ++ getReg.map(x=>s"${fixVar(x)}_set = false;")
    val sync = portsRead.map(x=>s"${fixVar(x)}[${fixVar(x)}_in]?")++eqsTerms.map(x=>s"${fixVar(x._1)}[${x._2}]!")
    val sel = for r <- portsRead yield s"${fixVar(r)}_in: ${ctxt.getConcreteType(r)}"
    val allUsedTypes = (usedTypes1++usedTypes2++usedTypes3).flatten

    //if sync.size>1 then println(s"Multiple sincronization ports not yet supported (${sync.mkString(",")})")
    for port <- (portsRead ++ r.eqs.map(_.v)) do ctxt.getConcreteType(port) match
      case t if t==Prelude.intType =>  Error.encoding(s"Cannot send or receive unbounded integers (port $port)")
      case _ =>

    ( Edge(sel=sel.mkString(", ") ,guard=guards.mkString(" && "), sync=sync.mkString(", "), upd=upd.mkString(" "))
      , allUsedTypes)


  private def fixVar(str: String) = str.replaceAll("§","_")
  private def fixType(str: Type) = str.toString match
    case "Int" => "int"
    case "Bool" => "bool"
    case x => x



