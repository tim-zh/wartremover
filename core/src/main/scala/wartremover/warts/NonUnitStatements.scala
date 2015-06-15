package org.brianmckenna.wartremover
package warts

object NonUnitStatements extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._
    import scala.reflect.NameTransformer

    val ReadName: TermName = "$read"
    val IwName: TermName = "$iw"
    val NodeBufferAddName: TermName = NameTransformer.encode("&+")

    def isIgnoredStatement(tree: Tree) = tree match {
      // Scala creates synthetic blocks with <init> calls on classes.
      // The calls return Object so we need to ignore them.
      case Apply(Select(_, nme.CONSTRUCTOR), _) => true
      // scala.xml.NodeBuffer#&+ returns NodeBuffer instead of Unit, so
      // val x = <x>5</x> desugars to a non-Unit statement; ignore.
      case Apply(Select(qual, NodeBufferAddName), _)
        if qual.symbol.typeSignature =:= typeOf[scala.xml.NodeBuffer] => true
      // REPL needs this
      case Select(Select(Select(Ident(_), ReadName), IwName), IwName) => true
      case _ => false
    }

    def checkUnitLike(statements: List[Tree]): Unit = {
      statements.foreach { stat =>
        val unitLike = stat.isEmpty || stat.tpe == null || stat.tpe =:= typeOf[Unit] || stat.isDef || isIgnoredStatement(stat)
        if (!unitLike)
          u.error(stat.pos, "Statements must return Unit")
      }
    }

    tree match {
      case Block(statements, _) =>
        checkUnitLike(statements)
        continue(u)
      case ClassDef(_, _, _, Template((_, _, statements))) =>
        checkUnitLike(statements)
        continue(u)
      case ModuleDef(_, _, Template((_, _, statements))) =>
        checkUnitLike(statements)
        continue(u)
      case _ => continue(u)
    }
  }
}
