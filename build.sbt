import com.typesafe.sbt.SbtGit.git

name := "fp-learn"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= {

  val scalazVersion = "7.2.11"

  Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion
  )
}
