package org.brianmckenna.wartremover
package warts

object AsInstanceOf extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._
    val EqualsName: TermName = "equals"
    val AsInstanceOfName: TermName = "asInstanceOf"

    val allowedCasts = List(
      "scala.tools.nsc.interpreter.IMain" // REPL needs this
    ) // cannot do `map rootMirror.staticClass` here because then:
      //   scala.ScalaReflectionException: object scala.tools.nsc.interpreter.IMain in compiler mirror not found.

    val synthetic = isSynthetic(u)(tree)
    tree match {

      // Ignore usage in synthetic classes
      case ClassDef(_, _, _, _) if synthetic => skip(u)

      // Ignore synthetic equals()
      case DefDef(_, EqualsName, _, _, _, _) if synthetic => skip(u)

      // Pattern matcher writes var x1 = null.asInstanceOf[...]
      case ValDef(mods, _, _, _) if mods.hasFlag(Flag.MUTABLE) && synthetic => skip(u)

      // Ignore allowed casts
      case TypeApply(Select(_, AsInstanceOfName), List(tt))
        if tt.isType && allowedCasts.contains(tt.tpe.typeSymbol.fullName) => skip(u)

      // Otherwise it's verboten for non-synthetic exprs
      case Select(e, AsInstanceOfName) if !isSynthetic(u)(e) =>
        err(u)(tree.pos, "asInstanceOf is disabled")

      case _ => continue(u)
    }
  }
}


