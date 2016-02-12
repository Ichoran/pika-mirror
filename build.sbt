// This file is distributed under the Apache 2 license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr

lazy val root = (project in file(".")).
  settings(
    scalaVersion := "2.11.7",
    name := "pika-mirror",
    version := "0.0.0",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test"
  )
