package org.brianmckenna.wartremover

import tools.nsc.plugins.PluginComponent
import tools.nsc.{Global, Phase}

import java.net.{URL, URLClassLoader}

class Plugin(val global: Global) extends tools.nsc.plugins.Plugin { thisPlugin =>
  import global._

  val name = "wartremover"
  val description = "Linting library and plugin. Allows rules to run inside a plugin or inside macros."
  val components = List[PluginComponent](Traverser)

  private[this] var traversers: List[WartTraverser] = List.empty
  private[this] var onlyWarnTraversers: List[WartTraverser] = List.empty
  private[this] var excludedFiles: List[String] = List.empty
  private[this] var debug: Boolean = false

  def getTraverser(mirror: reflect.runtime.universe.Mirror)(name: String): WartTraverser = {
    val moduleSymbol = mirror.staticModule(name)
    val instance = mirror.reflectModule(moduleSymbol).instance
    instance.asInstanceOf[WartTraverser]
  }

  def prefixedOption(prefix: String)(option: String) =
    if (option.startsWith(s"$prefix:"))
      Some(option.substring(s"$prefix:".length))
    else
      None

  def filterOptions(prefix: String, options: List[String]) =
    options.map(prefixedOption(prefix)).flatten

  override def processOptions(options: List[String], error: String => Unit): Unit = {
    val classPathEntries = filterOptions("cp", options).map(new URL(_))
    val classLoader = new URLClassLoader(classPathEntries.toArray, getClass.getClassLoader)
    val mirror = reflect.runtime.universe.runtimeMirror(classLoader)

    def ts(p: String) = {
      val traverserNames = filterOptions(p, options)
      traverserNames.map(getTraverser(mirror))
    }

	 debug = options.contains("debug")
    traversers = ts("traverser")
    onlyWarnTraversers = ts("only-warn-traverser")
    excludedFiles = filterOptions("excluded", options) flatMap (_ split ":") map (_.trim) map (new java.io.File(_).getAbsolutePath)
  }

  object Traverser extends PluginComponent {
    import global._

    val global = Plugin.this.global

    override val runsAfter = List("typer")

    val phaseName = "wartremover-traverser"

    override def newPhase(prev: Phase) = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) = {
        val isExcluded = excludedFiles contains unit.source.file.absolute.path

        if (!isExcluded) {
          def wartUniverse(onlyWarn: Boolean) = new WartUniverse {
            val universe: global.type = global
            def error(tree: Tree, message: String) = {
              if (onlyWarn) global.reporter.warning(tree.pos, message)
              else global.reporter.error(tree.pos, message)
				  if (debug) {
					 global.reporter.info(NoPosition, "Tree:\n" + show(tree), true)
					 global.reporter.info(NoPosition, "Raw Tree:\n" + showRaw(tree), true)
					 global.reporter.info(NoPosition, "Is Synthetic:\n" + tree.pos.lineContent, true)
				  }
				}
            def warning(tree: Tree, message: String) = global.reporter.warning(tree.pos, message)
          }

          def go(ts: List[WartTraverser], onlyWarn: Boolean) =
            ts.foreach(_(wartUniverse(onlyWarn)).traverse(unit.body))

          go(traversers, onlyWarn = false)
          go(onlyWarnTraversers, onlyWarn = true)
        }
      }
    }
  }
}
