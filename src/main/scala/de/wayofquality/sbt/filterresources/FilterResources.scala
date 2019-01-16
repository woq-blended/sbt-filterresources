package de.wayofquality.sbt.filterresources

import java.nio.file.Files

import sbt.Keys._
import sbt._
import sbt.io.syntax

import scala.util.matching.Regex
import scala.util.matching.Regex.quoteReplacement

object FilterResources extends AutoPlugin {

  object autoImport {
    val filterSources = settingKey[Seq[File]]("Resource to filter (files and directories supported)")
    val filterTargetDir = settingKey[File]("Target directory for the filtered files")
    val filterResources = taskKey[Seq[(File, String)]]("Filter the unfiltered resources")
    val filterProperties = settingKey[Map[String, String]]("Extra properties to be applied while filtering")
    val filterRegex = settingKey[String]("The replacement pattern. The actual lookup-key must be matched by regex group 1.")
    val filterResourcesFailOnMissingMatch = settingKey[Boolean]("Iff true, fail when replacement pattern was detected, but no replacement property was found.")
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      filterRegex := """\$\{(.+?)\}"""

    ) ++ inConfig(Compile)(Seq(
        filterResourcesFailOnMissingMatch := true,
        filterProperties := Map.empty,
        filterSources := Seq(baseDirectory.value / "src" / "main" / "filterResources"),
        filterTargetDir := classDirectory.value,
        filterResources := {
          val envProps: Map[String, String] = sys.env.map { case (k, v) => s"env.$k" -> v }
          val sysProps: Map[String, String] = sys.props.map { case (k, v) => s"sys.$k" -> v }.toMap

          ResourceFilter(
            filterSources.value,
            filterRegex.value,
            filterTargetDir.value,
            envProps ++ sysProps ++ filterProperties.value,
            filterResourcesFailOnMissingMatch.value
          )(streams.value.log)
        },
        exportedProducts := {
          // exec before exportedProducts
          filterResources.value
          exportedProducts.value
        }
      )) ++
      inConfig(Test)(Seq(
        filterResourcesFailOnMissingMatch := true,
        filterProperties := Map.empty,
        filterSources := Seq(baseDirectory.value / "src" / "test" / "filterResources"),
        filterTargetDir := classDirectory.value,
        filterResources := {
          val envProps: Map[String, String] = sys.env.map { case (k, v) => s"env.$k" -> v }
          val sysProps: Map[String, String] = sys.props.map { case (k, v) => s"sys.$k" -> v }.toMap

          ResourceFilter(
            filterSources.value,
            filterRegex.value,
            filterTargetDir.value,
            envProps ++ sysProps ++ filterProperties.value,
            filterResourcesFailOnMissingMatch.value
          )(streams.value.log)
        },
        exportedProducts := {
          // exec before exportedProducts
          filterResources.value
          exportedProducts.value
        }
      ))
}

object ResourceFilter {

  private def filterCandidates(sources: File): Seq[(File, String)] = {
    if (!sources.exists()) {
      Seq.empty
    } else {
      if (sources.isFile) {
        Seq(sources -> sources.getName)
      } else {
        val mapper: syntax.File => Option[String] = {
          f =>
            if (f.isFile) {
              Some(f.getAbsolutePath().substring(sources.getAbsolutePath().length + 1))
            } else {
              None
            }
        }
        PathFinder(sources).**("***").pair(mapper, false)
      }
    }
  }

  private def applyFilter(
    source: File,
    pattern: Regex,
    targetDir: File,
    relative: String,
    properties: Map[String, String],
    failOnMiss: Boolean
  )(implicit log: Logger): (File, String) = {

    def performReplace(in: String): String = {
      val replacer = { m: Regex.Match =>
        val variable = m.group(1)
        val matched = m.matched

        quoteReplacement(properties.getOrElse(
          variable,
          if (failOnMiss) sys.error(s"Unknown variable: [$variable]") else {
            log.warn(s"${source}: Can't replace unknown variable: [${variable}]")
            matched
          }
        ))
      }

      pattern.replaceAllIn(in, replacer)
    }

    val destination = new File(targetDir, relative)
    deleteRecursive(destination)

    Files.createDirectories(destination.getParentFile.toPath)

    val content = IO.read(source)
    IO.write(destination, performReplace(content))

    (destination, relative)
  }

  private def deleteRecursive(f: File): Unit = {
    if (f.isDirectory()) {
      f.listFiles().foreach(deleteRecursive)
    }
    f.delete()
  }

  def apply(
    unfilteredResources: Seq[File],
    pattern: String,
    filterTargetDir: File,
    props: Map[String, String],
    failOnMiss: Boolean
  )(implicit log: Logger): Seq[(File, String)] = {
    val files = unfilteredResources.flatMap(filterCandidates)
    val regex = new Regex(pattern)
    val filtered = files.map { case (file, relative) => applyFilter(file, regex, filterTargetDir, relative, props, failOnMiss) }
    log.debug("Filtered Resources: " + filtered.mkString(","))

    filtered
  }
}
