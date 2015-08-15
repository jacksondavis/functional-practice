name := "specs2-tutorial"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.2.2" % "test"
)

parallelExecution in Test := false