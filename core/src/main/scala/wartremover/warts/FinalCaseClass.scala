package org.brianmckenna.wartremover
package warts

object FinalCaseClass extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._
    import u.universe.Flag._

    tree match {
      case ClassDef(mods, _, _, _) if mods.hasFlag(CASE) && !mods.hasFlag(FINAL) =>
        err(u)(tree.pos, "case classes must be final")
      // Do not look inside other classes.
      // See: https://groups.google.com/forum/#!msg/scala-internals/vw8Kek4zlZ8/LAeakfeR3RoJ
      case ClassDef(_, _, _, _) => skip(u)
      case t => continue(u)
    }
  }
}
