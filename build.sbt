// to compile alone
val scala3Version = "3.0.2"

lazy val caos = project.in(file("lib/caos"))
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaVersion := scala3Version)

lazy val choreo = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "marx",
    version := "0.3.0",
    scalaVersion := scala3Version,
    scalacOptions += "-new-syntax",
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("marx.frontend.Main"),
    //Compile / fastLinkJS / artifactPath := baseDirectory.value / "lib" / "caos" / "tool" / "js" / "gen" / "fastLink.js",
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "lib" / "caos"/ "tool" / "js" / "gen",
    //Compile / fastLinkJS / scalaJSLinkerConfig ~= (_.withOutputPatterns(OutputPatterns.fromJSFile(name.value))),
    libraryDependencies ++= Seq(
//      "org.scalatest" % "scalatest_3" % "3.2.9" % "test",
      ("org.typelevel" %%% "cats-parse" % "0.3.4"),
      //
      ("be.doeraene" %%% "scalajs-jquery" % "1.0.0").cross(CrossVersion.for3Use2_13),//.withDottyCompat(scalaVersion.value),
      ("org.scala-js" %%% "scalajs-dom" % "1.2.0").cross(CrossVersion.for3Use2_13),//.withDottyCompat(scalaVersion.value),
      ("com.lihaoyi" %%% "scalatags" % "0.9.1").cross(CrossVersion.for3Use2_13)//.withDottyCompat(scalaVersion.value)
    )
  )
  .dependsOn(caos)




// to compile within ReoLive with crossVersions
//scalaVersion := "3.0.0-M1"
//libraryDependencies ++= Seq(
//  ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2").withDottyCompat(scalaVersion.value), //.cross(CrossVersion.for2_13Use3),
//  ("org.scalatest" %% "scalatest" % "3.2.9" % "test").withDottyCompat(scalaVersion.value), //.cross(CrossVersion.for2_13Use3),
//  ("org.typelevel" %% "cats-parse" % "0.3.4").withDottyCompat(scalaVersion.value)
//)

