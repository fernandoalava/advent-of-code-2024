val scala3Version = "3.5.2"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",      // Warn about use of deprecated APIs
  "-feature",          // Warn about misused language features
  "-unchecked",        // Enable additional warnings for unchecked code
  "-Xlint",            // Enable recommended linting
  "-Wunused:all"       // Enable all unused warnings
)


lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2024",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
