package org.brianmckenna.wartremover
package warts

object JavaConversions extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._

    val javaConversions = rootMirror.staticModule("scala.collection.JavaConversions")

    tree match {
      case Select(tpt, _) if tpt.tpe.contains(javaConversions) => {
        err(u)(tree.pos, "scala.collection.JavaConversions is disabled - use scala.collection.JavaConverters instead")
      }
      case _ => continue(u)
    }
  }
}
