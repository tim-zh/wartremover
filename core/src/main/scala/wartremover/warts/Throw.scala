package org.brianmckenna.wartremover
package warts

object Throw extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    import u.universe._
    val ProductElementName: TermName = "productElement"
    new u.Traverser {
      override def traverse(tree: Tree) {
        tree match {
          // Ignore trees marked by SuppressWarnings
          case t if hasWartAnnotation(u)(t) =>
          case tree if isSynthetic(u)(tree) =>
          case u.universe.Throw(_) =>
            u.error(tree, "throw is disabled")
            super.traverse(tree)
          case _ =>
            super.traverse(tree)
        }
      }
    }
  }
}
