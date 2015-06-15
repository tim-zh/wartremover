package org.brianmckenna.wartremover
package warts

object Throw extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._
    val ProductElementName: TermName = "productElement"

    tree match {
      case dd@DefDef(_, ProductElementName , _, _, _, _) if isSynthetic(u)(dd) => skip(u)
      case u.universe.Throw(_) =>
        err(u)(tree.pos, "throw is disabled")
      case _ => continue(u)
    }
  }
}
