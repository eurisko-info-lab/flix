/*
 *  Copyright 2020 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{Symbol, Type}

/**
  * A substitution is a map from type variables to types.
  */
class BasicSubstitution(m: Map[Symbol.KindedTypeVarSym, Type]) {

  /**
    * Returns `true` if `this` is the empty substitution.
    */
  val isEmpty: Boolean = m.isEmpty

  /**
    * Applies `this` substitution to the given type `tpe0`.
    */
  def apply(tpe0: Type): Type = {
    // NB: The order of cases has been determined by code coverage analysis.
    def visit(t: Type): Type =
      t match {
        case x: Type.Var => m.getOrElse(x.sym, x)

        case Type.Cst(_, _) => t

        case Type.Apply(t1, t2, loc) =>
          // Note: While we could perform simplifications here,
          // experimental results have shown that it is not worth it.
          val x = visit(t1)
          val y = visit(t2)
          // Performance: Reuse t, if possible.
          if ((x eq t1) && (y eq t2))
            t
          else
            Type.Apply(x, y, loc)

        case Type.Alias(sym, args0, tpe0, loc) =>
          val args = args0.map(visit)
          val tpe = visit(tpe0)
          Type.Alias(sym, args, tpe, loc)

        case Type.AssocType(cst, args0, kind, loc) =>
          val args = args0.map(visit)
          Type.AssocType(cst, args, kind, loc)

        case Type.JvmToType(tpe0, loc) =>
          val tpe = visit(tpe0)
          Type.JvmToType(tpe, loc)

        case Type.JvmToEff(tpe0, loc) =>
          val tpe = visit(tpe0)
          Type.JvmToEff(tpe, loc)

        case Type.UnresolvedJvmType(member0, loc) =>
          val member = member0.map(visit)
          Type.UnresolvedJvmType(member, loc)
      }

    // Optimization: Return the type if the substitution is empty. Otherwise visit the type.
    if (isEmpty) tpe0 else visit(tpe0)
  }

  /**
    * Applies `this` substitution to the given types `ts`.
    */
  def apply(ts: List[Type]): List[Type] = if (isEmpty) ts else ts map apply
}
