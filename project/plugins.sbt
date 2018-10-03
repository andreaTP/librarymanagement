addSbtPlugin("org.scala-sbt" % "sbt-houserules"  % "0.3.8")
addSbtPlugin("org.scala-sbt" % "sbt-contraband"  % "0.4.1")
addSbtPlugin("com.lightbend" % "sbt-whitesource" % "0.1.9")

scalacOptions += "-language:postfixOps"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit.pgm" % "3.2.0.201312181205-r"
