package org.brianmckenna.wartremover
package warts

object Return extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._

    tree match {
      case u.universe.Return(_) =>
        err(u)(tree.pos, "return is disabled")
      case _ => continue(u)
    }
  }
}
