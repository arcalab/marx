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
  /** Parser for Marx expressions. */
  val parser: String=>Program = marx.syntax.Parser.parseProgram

  import marx.syntax.Parser.Examples
  val examples = List(
    Example(Examples.ex3,"lossyfifo",""),
    Example("""// this automata already exists in
// the prelude, as "fifo"
aut myFifo(a) {
  rules
    [get] from(a), notAt(m) --> m:=a;
    [put] from(m) --> b=m;
  return b;
}

myFifo(a) --> b;
myFifo(b) --> c;
myFifo(c) --> d;""","3xFifo1",""),
    Example(Examples.ex4,"Sequencer-3",""),
    Example(Examples.ex2,"Fruit",""),
    Example(Examples.ex1,"controller","")
  )

  val widgets: Iterable[(String,Widget[Program])] = List(
    "Architecture" ->
      Visualize[Program,Network](net=>View(ArchGraph.apply(net).toMermaid), Mermaid, Encode.apply(_,Set())),
    "Main Automata" ->
      Visualize[Program,Network](net=>View(AvGraph.allMermaid(net)), Mermaid, Encode.apply(_,Set())),
    "Used Automata" ->
      Visualize[Program,Network](net=>View(AvGraph.allUsedMermaid(net)), Mermaid, Encode.apply(_,Set())),
    "Automaton" ->
      Visualize[Program,Network](net=>View(AvGraph.oneMermaid(net)), Mermaid, Encode.apply(_,Set())),
    "Uppaal" ->
      Visualize[Program,Network](net=>View(Uppaal.netToUppaal(net)), caos.view.Text, Encode.apply(_,Set())),
    "Uppaals" ->
      Visualize[Program,Network](net=>View(Uppaal.netToUppaals(net)), caos.view.Text, Encode.apply(_,Set())),
    "Encoded" ->
      Visualize[Program,Network](net=>View(net.toString), caos.view.Text, Encode.apply(_,Set())),
    "Used Automata [txt]" ->
      Visualize[Program,Network](
        net=>View(Network.instantiateNamedAuts(net).map((n,a)=>s"== $n ==\n$a").mkString("\n\n")),
        caos.view.Text, Encode.apply(_,Set())),
    "Automaton [txt]" ->
      Visualize[Program,Network](net=>View(net.toAut.toString), caos.view.Text, Encode.apply(_,Set())),
    "Type" ->
      Visualize[Program,Network](net=>View(marx.typing.Infer(net).toString), caos.view.Text, Encode.apply(_,Set()))
  )