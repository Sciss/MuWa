lazy val commonSettings = Seq(
  name         := "MuWa",
  version      := "0.1.0-SNAPSHOT",
  description  := "Sound installation",
  organization := "de.sciss",
  homepage     := Some(url(s"https://git.iem.at/sciss/${name.value}")),
  licenses     := Seq("agpl v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  scalaVersion := "2.12.8",
  scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8", "-Xlint"),
  libraryDependencies ++= Seq(
    "de.sciss"          %% "fileutil"       % "1.1.3",
    "de.sciss"          %% "fscape-core"    % "2.20.0",
//    "de.sciss"          %% "scalacollider"  % "1.28.0",
    "de.sciss"          %% "lucresynth"     % "3.11.0",
    "de.sciss"          %% "swingplus"      % "0.4.0",
    "org.scala-stm"     %% "scala-stm"      % "0.9",
    "com.github.scopt"  %% "scopt"          % "3.7.1",
  ),
  javaCppPresetLibs ++= Seq(
    "ffmpeg" -> "4.0.2"
  )
)

lazy val assemblySettings = Seq(
  mainClass       in assembly := Some("de.sciss.muwa.Main"),
  assemblyJarName in assembly := "muwa.jar",
  target          in assembly := baseDirectory.value
)

lazy val buildInfoSettings = Seq(
  // ---- build info ----
  buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
    BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
    BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
  ),
  buildInfoOptions += BuildInfoOption.BuildTime,
  buildInfoPackage := "de.sciss.muwa"
)

lazy val root = project.in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(assemblySettings)
  .settings(buildInfoSettings)
