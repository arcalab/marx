package marx.frontend

import caos.frontend.Site.initSite
import marx.syntax.{Parser, Program}

/** Main function called by ScalaJS' compiled javascript when loading. */
object Main {
  def main(args: Array[String]):Unit =
    initSite[Program](CaosConfig)
}