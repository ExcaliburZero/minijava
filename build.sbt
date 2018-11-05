name := "minijava"

version := "1.0"

scalaVersion := "2.12.1"

// Packages
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies ++= Seq(
  "org.antlr" % "antlr4-runtime" % "4.7.1",
  "org.antlr" % "stringtemplate" % "3.2"
)

libraryDependencies += "org.scalaz" % "scalaz-core_2.10" % "7.2.26"
libraryDependencies += "org.ow2.jonas.osgi" % "asm" % "5.3.0-M3"

mainClass in Compile := Some("minijava.Main")

// Linting
resolvers += Resolver.sonatypeRepo("snapshots")
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1-SNAPSHOT")
