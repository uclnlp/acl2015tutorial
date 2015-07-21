import sbt._
import sbt.Keys._

object BuildSettings {
  val buildName = "acltutorial"
  val buildOrganization = "uk.ac.ucl.cs.mr"
  val buildScalaVersion = "2.11.4"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"), //, "-Yrangepos"?
    libraryDependencies ++= Seq(
      "org.sameersingh.scalaplot" % "scalaplot" % "0.1",
      "com.google.guava" % "guava" % "18.0",
      "org.scala-lang.modules" %% "scala-pickling" % "0.10.0"),

    resolvers ++= Seq(
      "IESL Release" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
      Resolver.mavenLocal,
      Resolver.defaultLocal,
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      "UIUC Releases" at "http://cogcomp.cs.illinois.edu/m2repo"
    ),

    //shellPrompt := ShellPrompt.buildShellPrompt,
    fork in run := true, //use a fresh JVM for sbt run
    connectInput in run := true, //to use readLine after sbt run
    commands ++= Seq(vmargs),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M1" cross CrossVersion.full)
  )

  val generalSettings =
    Seq(
      initialCommands := """
        import ml.wolfe.Wolfe._
        import ml.wolfe.macros.OptimizedOperators._
                         												 """
    )

  def vmargs = Command.args("vmargs", "<name>") {
    (state, args) =>
      val javaRunOptions = args.mkString(" ")
      println("Applying JVM arguments: " + javaRunOptions)
      Project.extract(state).append(javaOptions := Seq(javaRunOptions), state)
  }
}

object Build extends Build {

  import BuildSettings._

  lazy val wolfeNLP = ProjectRef(file("./wolfe"), "wolfe-nlp")
  lazy val wolfeCore = ProjectRef(file("./wolfe"), "wolfe-core")
  lazy val wolfeUI = ProjectRef(file("./wolfe"), "wolfe-ui")
  lazy val wolfeExamples= ProjectRef(file("./wolfe"), "wolfe-examples")

  lazy val acltutorial = Project(
    id = "acltutorial",
    base = file("."),
    settings = buildSettings
  ) dependsOn(
    wolfeNLP % "test->test;compile->compile",
    wolfeUI % "test->test;compile->compile",
    wolfeCore % "test->test;compile->compile",
    wolfeExamples % "test->test;compile->compile"

    )
}
