// Add dependency on ScalaFX library
libraryDependencies += "org.scalafx" %% "scalafx" % "15.0.1-R21"

// Add dependency for socket management
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.12.0"


// Add upickle for Json parsing
libraryDependencies += "com.lihaoyi" %% "upickle" % "1.3.8"

// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

// Add dependency on JavaFX libraries, OS dependent
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % "15.0.1" classifier osName
)
