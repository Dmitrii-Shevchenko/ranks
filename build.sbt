name := "Chalanges"

version := "1.0"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.25"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.0" withSources () withJavadoc ()

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-language:higherKinds",
  "-Ypartial-unification"
)
