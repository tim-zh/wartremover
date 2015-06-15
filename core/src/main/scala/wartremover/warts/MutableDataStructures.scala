package org.brianmckenna.wartremover
package warts

object MutableDataStructures extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._

    val mutablePackage = rootMirror.staticPackage("scala.collection.mutable")

    tree match {
      case Select(tpt, _) if tpt.tpe.contains(mutablePackage) && tpt.tpe.termSymbol.isPackage =>
        err(u)(tree.pos, "scala.collection.mutable package is disabled")
      case _ => continue(u)
    }
  }
}
