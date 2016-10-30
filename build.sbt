val standard = List(
  scalaVersion := "2.12.0-RC2"
, crossScalaVersions := Seq("2.11.8", scalaVersion.value)
, libraryDependencies ++= List(
    "org.scalaz" %% "scalaz-core" % "7.2.7"
  )
)

lazy val core = Project(
  "folds4s-core"
, file("folds4s-core")
, settings = standard
)

lazy val fs2 = Project(
  "folds4s-fs2"
, file("folds4s-fs2")
, settings = standard ++ List(
    libraryDependencies += "co.fs2" %% "fs2-core" % "0.9.1"
  )
, dependencies = List(core)
)

lazy val folds4s = Project(
  "folds4s"
, file(".")
, settings = standard
, aggregate = List(core, fs2)
)
