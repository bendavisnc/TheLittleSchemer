name := "TheLittleScalaSchemer"

description := """Practice for myself as I've been going through "The Little Schemer" books"""

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6.4" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")
    