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
//libraryDependencies += "org.ow2.jonas.osgi" % "asm" % "5.3.0-M3"
libraryDependencies += "org.ow2.asm" % "asm" % "7.0"

mainClass in Compile := Some("minijava.Main")

// Linting
resolvers += Resolver.sonatypeRepo("snapshots")
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1-SNAPSHOT")

// Soot
resolvers += "Soot Snapshot" at "https://soot-build.cs.uni-paderborn.de/nexus/repository/soot-snapshot/"
resolvers += "Soot Release" at "https://soot-build.cs.uni-paderborn.de/nexus/repository/soot-release"
//libraryDependencies += "ca.mcgill.sable" % "soot-j9" % "4.0.0-SNAPSHOT"
//libraryDependencies += "ca.mcgill.sable" % "soot" % "3.1.0-SNAPSHOT"
libraryDependencies += "ca.mcgill.sable" % "soot" % "3.1.0"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.10"
