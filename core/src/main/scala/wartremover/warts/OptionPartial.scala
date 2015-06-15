package org.brianmckenna.wartremover
package warts

object OptionPartial extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._

    val optionSymbol = rootMirror.staticClass("scala.Option")
    val GetName: TermName = "get"
    tree match {
      case Select(left, GetName) if left.tpe.baseType(optionSymbol) != NoType =>
        err(u)(tree.pos, "Option#get is disabled - use Option#fold instead")
      // TODO: This ignores a lot
      case LabelDef(_, _, rhs) if isSynthetic(u)(tree)=> skip(u)
      case _ => continue(u)
    }
  }
}
