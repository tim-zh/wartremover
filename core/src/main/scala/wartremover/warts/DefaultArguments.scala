package org.brianmckenna.wartremover
package warts

object DefaultArguments extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._
    import u.universe.Flag._

    def containsDef(v : List[ValDef]) =
      v.find(_.mods.hasFlag(DEFAULTPARAM)).isDefined

    tree match {
      case d@DefDef(_, _, _, vs, _, _) if !isSynthetic(u)(d) && vs.find(containsDef).isDefined =>
        err(u)(tree.pos, "Function has default arguments")
      case _ => continue(u)
    }
  }
}
