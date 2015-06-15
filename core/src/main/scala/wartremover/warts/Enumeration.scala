package org.brianmckenna.wartremover
package warts

object Enumeration extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._

    val enumeration = typeOf[scala.Enumeration].typeSymbol

    tree match {
      case t: ImplDef if t.symbol.typeSignature.baseClasses.contains(enumeration) =>
        err(u)(tree.pos, "Enumeration is disabled - use case objects instead")
      case t => continue(u)
    }
  }
}
