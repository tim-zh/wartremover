package org.brianmckenna.wartremover
package warts

object EitherProjectionPartial extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._

    val leftProjectionSymbol = rootMirror.staticClass("scala.util.Either.LeftProjection")
    val rightProjectionSymbol = rootMirror.staticClass("scala.util.Either.RightProjection")
    val GetName: TermName = "get"

    tree match {
      case Select(left, GetName) if left.tpe.baseType(leftProjectionSymbol) != NoType =>
        err(u)(tree.pos, "LeftProjection#get is disabled - use LeftProjection#toOption instead")
      case Select(left, GetName) if left.tpe.baseType(rightProjectionSymbol) != NoType =>
        err(u)(tree.pos, "RightProjection#get is disabled - use RightProjection#toOption instead")
      case _ => continue(u)
    }
  }
}
