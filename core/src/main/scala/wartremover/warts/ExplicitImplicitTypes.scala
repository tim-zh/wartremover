package org.brianmckenna.wartremover
package warts

import scala.util.matching.Regex

object ExplicitImplicitTypes extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._
    import u.universe.Flag._

    def hasTypeAscription(tree: ValOrDefDef) : Boolean = 
      new Regex("""(val|def)\s*""" + tree.name.decodedName.toString.trim + """(\[.*\])?(\(.*\))*\s*:""")
        .findFirstIn(tree.pos.lineContent).nonEmpty

    tree match {
      case t: ValOrDefDef if t.mods.hasFlag(IMPLICIT) && !t.mods.hasFlag(PARAM) && !isSynthetic(u)(t) && !hasTypeAscription(t) =>
        err(u)(tree.pos, "implicit definitions must have an explicit type ascription")
      case t => continue(u)
    }
  }
}
