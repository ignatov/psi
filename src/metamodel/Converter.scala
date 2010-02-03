package metamodel

import pcis.metamodel.P
import psic.parser.Package

/**
 * @author: ignatov
 * Date:    04.02.2010
 */

/**
 * Convert from PSI AST to PSI metamodel
 */
object Converter {
  def run(pack: Package): P = {
    return P("empty", null)
  }
}