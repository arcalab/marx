package marx.frontend

import caos.common.Example
import caos.frontend.Configurator
import caos.frontend.Configurator.{Visualize, Widget}
import caos.view.View
import caos.view.Mermaid
import marx.backend.{ArchGraph, AvGraph, Uppaal}
import marx.core.Network
import marx.syntax.{Encode, Program}

object CaosConfig extends Configurator[Program]:
  val name = "Marx"
  /** Parser for Choreo expressions. */
  val parser: String=>Program = marx.syntax.Parser.parseProgram

  import marx.syntax.Parser.Examples
  val examples = List(
    Example(Examples.ex2,"Fruit",""),
    Example(Examples.ex1,"controller","")
  )

  val widgets: Iterable[(String,Widget[Program])] = List(
    "Architecture" ->
      Visualize[Program,Network](net=>View(ArchGraph.apply(net).toMermaid), Mermaid, Encode.apply(_,Set())),
    "Automata" ->
      Visualize[Program,Network](net=>View(AvGraph.allMermaid(net)), Mermaid, Encode.apply(_,Set())),
    "Automaton" ->
      Visualize[Program,Network](net=>View(AvGraph.oneMermaid(net)), Mermaid, Encode.apply(_,Set())),
    "Uppaal" ->
      Visualize[Program,Network](net=>View(Uppaal.netToUppaal(net)), caos.view.Text, Encode.apply(_,Set())),
    "Uppaals" ->
      Visualize[Program,Network](net=>View(Uppaal.netToUppaals(net)), caos.view.Text, Encode.apply(_,Set())),
    "Encoded" ->
      Visualize[Program,Network](net=>View(net.toString), caos.view.Text, Encode.apply(_,Set())),
    "Type" ->
      Visualize[Program,Network](net=>View(marx.typing.Infer(net).toString), caos.view.Text, Encode.apply(_,Set()))
  )
//    "Encode NPomset"
//      -> Visualize(viewNPomMerm,Mermaid,chor2npom),
//    "Sequence Diagram"
//      -> Visualize(viewChorMerm,Mermaid,id),
//    //    "NPomset as Text"
//    //      -> Visualize((p:NPomset)=>Text(p.toString),chor2npom),
//    //    "Simulate NPomset"
//    //      -> Simulate(NPomDefSOS,(p:NPomset)=>Text(p.toString),chor2npom),
//    "Project NPomset"
//      -> Visualize(viewNPomsMerm, Mermaid, chor2npom(_).projectAll),
//
//    "Well-branched (choreo: default proj+SOS)"
//      -> Visualize((c:Choreo)=>View(WellBranched(c).show),Text,id),
//    "Well-channelled (choreo: default proj+SOS)"
//      -> Visualize((c:Choreo)=>View(WellChannelled(c).show),Text,id),
//    "Realisability via bisimulation (choreo: no-tau-proj + default SOS)"
//      -> compareBranchBisim(ChorDefSOS,Network.sosMS(ChorDefSOS),id,mkNetMS(_,ChorNoTauProj)),
//    //    "Realisability via branch-bisimulation (default proj+SOS w/o taus)"
//    //      -> compareBranchBisim(ChorDefSOS,Network.sos(postponeTaus(ChorDefSOS)),id,Network(_,ChorDefProj)),
//    //    "Realisability via branch-bisimulation (many-taus proj+SOS w/o taus)"
//    //      -> compareBranchBisim(ChorDefSOS,Network.sos(postponeTaus(ChorDefSOS)),id,Network(_,ChorManyTausProj)),
//    "Realisability via bisimulation (choreo: no-tau-proj + CAUSAL net + default SOS)"
//      -> compareBranchBisim(ChorDefSOS,Network.sosCS(ChorDefSOS),id,mkNetCS(_,ChorNoTauProj)),
//
//    "Realisability via trace equivalence (MSet) (choreo: default proj+SOS)"
//      -> compareTraceEq(ChorDefSOS,Network.sosMS(ChorDefSOS),id,mkNetMS(_,ChorDefProj)),
//    "Realisability via trace equivalence (Causal) (choreo: default proj+SOS)"
//      -> compareTraceEq(ChorDefSOS,Network.sosCS(ChorDefSOS),id,mkNetCS(_,ChorDefProj)),
//    //"Realisability via branch-bisimulation (NPomSOS + proj with interclosure all-in-1)"
//    //  -> compareBranchBisim(NPomDefSOS,NPomDefSOS,chor2npom,chor2npom(_).icnpom.head.getPom),
//    "Realisability via branch-bisimulation (NPomSOS + proj)"
//      -> compareBranchBisim(NPomDefSOS,Network.sosMS(NPomDefSOS),chor2npom,(c:Choreo) => mkNetMS(chor2npom(c),NPomDefProj)),
//
//    "CC2-NPOM NPomset Inter-Closure"
//      -> VisualizeOpt(showIC, Mermaid, chor2npom(_).icnpom),
//    "CC2-NPOM-plus-plus Merge Interclosure"
//      -> Visualize(viewNPomMerm,Mermaid, chor2npom(_).mergeIC),
//    "CC2-NPOM NPomset (Simplified)"
//      -> Visualize(viewNPomMerm, Mermaid, chor2npom(_).simplifyChoices),
//    "CC2-NPOM NPomset Inter-Closure (Simplified)"
//      -> VisualizeOpt(showIC, Mermaid, (c:Choreo) => ICNPOM(chor2npom(c))(using true)),
//    "CC2-NPOM-plus-plus Merge Interclosure (Simplified)"
//      -> Visualize(viewNPomMerm, Mermaid, (c:Choreo) => Merge.compose(ICNPOM(chor2npom(c))(using true).head)),
//    //"CC2-NPOM Summary (Simplified)"
//    //  -> Visualize((r:CCPomInfo)=>View(CC.ppcc2(r)),Text,chor2npom(_).cc2npom),
//    //"Inter-Closure (Emilio)"
//    //  -> Visualize(viewEICPomsMerm, chor2npom(_).einterclosure),
//    "CC2-POM Global Refinements"
//      -> VisualizeOpt(showRef,Mermaid,chor2npom(_).refinements),
//    "CC2-POM Projections per Refinement"
//      -> VisualizeOpt(showRefProj,Mermaid,chor2npom(_).refinementsProj),
//    "CC2-POM Inter-Closures with Result"
//      -> VisualizeOpt(showICWithResMermaid,Mermaid,chor2npom(_).cc2),
//    "CC2-POM Summary"
//      -> Visualize((r:CCPomInfo)=>View(CC.ppcc2(r)),Text,chor2npom(_).cc2),
//    "CC3-POM Global Prefixes"
//      -> VisualizeOpt(showRef,Mermaid,chor2npom(_).refinements.map(r=>NPomDAG.prefixes(r).toList).flatten.distinct),
//    "CC3-POM Summary"
//      -> Visualize((r:CCPomInfo)=>View(CC.ppcc3(r)),Text,chor2npom(_).cc3),
//    //"Realisability NPomset (experiments)"
//    //  -> Visualize((b:Boolean) => Text(b.toString), chor2npom(_).realisable),
//    //    "Project NPomset at a"
//    //      -> Visualize(viewNPomMerm, chor2npom(_).project(Agent("a"))),
//    //    "Pomset as Text"
//    //      -> Visualize(viewPomTxt,chor2pom),
//    //    "Simulate Choreo (basic)"
//    //      -> Simulate(ChorBasicSOS,viewChorTxt,id),
//    "Simulate Choreo (default)"
//      -> Simulate(ChorDefSOS,viewChorTxt,Text,id),
//    //    "Simulate Network of Choreo (default)"
//    //      -> simulateNet(ChorDefSOS,viewChorTxt,ChorDefProj,id),
//    "Simulate Network of Choreo (no-taus)"
//      -> simulateNet(ChorDefSOS,viewChorTxt,ChorNoTauProj,id),
//    "Simulate Causal Network of Choreo (no-taus)"
//      -> simulateCNet(ChorDefSOS,viewChorTxt,ChorNoTauProj,id),
//    "Visualize a Causal Network of Choreo (no-taus)"
//      -> Visualize(x=>viewCSNetConc[Choreo](x,viewChorTxt),Text, (ch:Choreo) => Network.mkNetCS(ChorNoTauProj.allProj(ch))),
//    //    "Simulate Network of Choreo (many-taus)"
//    //      -> simulateNet(ChorManyTausSOS,viewChorTxt,ChorManyTausProj,id),
//    //    "Simulate Network of Choreo (default w/o taus)"
//    //      -> simulateNet(postponeTaus(ChorDefSOS),viewChorTxt,ChorDefProj,id),
//    //    "Simulate Network of Choreo (many-taus w/o taus)"
//    //      -> simulateNet(postponeTaus(ChorManyTausSOS),viewChorTxt,ChorManyTausProj,id),
//    "Simulate NPomset (default)"
//      -> Simulate(NPomDefSOS,viewNPomMerm,Mermaid,chor2npom),
//    //    "Simulate Pomset (keeper)"
//    //      -> Simulate(PomKeepSOS,viewPomMerm,chor2pom),
//    //"Simulate NPomset Interclosure"
//    // -> Simulate(NPomDefSOS,viewNPomMerm,chor2npom(_).icnpom.head.getPom) ,
//    "Simulate NPomset Network"
//      -> simulateNet(NPomDefSOS,(p:NPomset)=>View(p.toString),NPomDefProj,chor2npom) ,
//    "Choreo (def) vs NPomset (v2)"
//      -> compareBranchBisim(ChorDefSOS,NPomDefSOS,id,chor2npom)
//    //    "Choreo (def) vs Pomset (def)"
//    //      -> compareBranchBisim(ChorDefSOS,PomDefSOS,id,chor2pom),
//    //    "Realisability via branch-bisimulation (default proj+SOS)"
//    //      -> compareBranchBisim(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj)),
//
//    //    "Experiments with syntactic realisability"
//    //      -> Visualize(Text,SyntAnalysis.realisablePP)
//    //    "Default realisability of all examples"
//    //      -> Visualize(Text, (_:Choreo)=>(for (s,c)<-examples
//    //          yield s"- $s: "+choreo.DSL.realisable(c)).mkString("\n")),
//    //    "Choreo vs. Pomsets of all examples"
//    //      -> Visualize(Text, (_:Choreo)=>(for (s,c)<-examples
//    //        yield s"- $s: "+BranchBisim.findBisim(c,chor2pom(c))(using ChorDefSOS,PomDefSOS,50).isRight).mkString("\n")),
//    //    "Visualize projections of Pomsets"
//    //      -> Visualize(viewSeqMerm[Pomset](_,viewPomMerm), (c:Choreo) => PomDefProj.allProj(chor2pom(c)))
//    //...
//  )

//  def simulateNet[S](sos:SOS[Action,S],
//                     sview:S=>View,
//                     proj:Projection[_,S],
//                     enc:(Choreo=>S)): Simulate[Choreo,Action,NetworkMS[S]] =
//    Simulate(Network.sosMS(sos),net=>ViewChoreo.viewNetConc(net,sview), Text, (c:Choreo)=>Network.mkNetMS(enc(c),proj))
//
//  def simulateCNet[S](sos:SOS[Action,S],
//                      sview:S=>View,
//                      proj:Projection[_,S],
//                      enc:(Choreo=>S)): Simulate[Choreo,Action,NetworkCausal[S]] =
//    Simulate(Network.sosCS(sos),net=>ViewChoreo.viewCSNetConc(net,sview), Text, (c:Choreo)=>Network.mkNetCS(enc(c),proj))
