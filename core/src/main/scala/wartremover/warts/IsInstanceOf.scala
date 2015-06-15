package org.brianmckenna.wartremover
package warts

object IsInstanceOf extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._
    val IsInstanceOfName: TermName = "isInstanceOf"
    val CanEqualName: TermName = "canEqual"
    val EqualsName: TermName = "equals"

    val synthetic = isSynthetic(u)(tree)
    tree match {

      // Ignore synthetic canEquals() and equals()
      case DefDef(_, CanEqualName | EqualsName, _, _, _, _) if synthetic => skip(u)

      // Otherwise nope, for non-synthetic receivers
      case Select(id, IsInstanceOfName) if !isSynthetic(u)(id) =>
        err(u)(tree.pos, "isInstanceOf is disabled")

      case _ => continue(u)
    }
  }
}
