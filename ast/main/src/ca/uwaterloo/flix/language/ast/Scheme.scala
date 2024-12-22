/*
 * Copyright 2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.fmt.{FormatOptions, FormatScheme}

object Scheme {

  /**
    * Instantiates the given type scheme `sc` by replacing all quantified variables with fresh type variables.
    */
  def instantiate(sc: Scheme, loc: SourceLocation)(implicit scope: Scope, genSym: GenSym): (List[TraitConstraint], List[BroadEqualityConstraint], Type, Map[Symbol.KindedTypeVarSym, Type.Var]) = {
    // Compute the base type.
    val baseType = sc.base

    //
    // Compute the fresh variables taking the instantiation mode into account.
    //
    val substMap = sc.quantifiers.foldLeft(Map.empty[Symbol.KindedTypeVarSym, Type.Var]) {
      case (macc, tvar) =>
        // Determine the rigidity of the fresh type variable.
        macc + (tvar -> Type.freshVar(tvar.kind, loc, tvar.isRegion, VarText.Absent))
    }
    val freshVars = substMap.map { case (k, v) => k.id -> v }

    /**
      * Replaces every variable occurrence in the given type using `freeVars`.
      *
      * Replaces all source locations by `loc`.
      *
      * Performance Note: We are on a hot path. We take extra care to avoid redundant type objects.
      */
    def visitType(tpe0: Type): Type = tpe0 match {
      case Type.Var(sym, _) =>
        // Performance: Reuse tpe0, if possible.
        freshVars.getOrElse(sym.id, tpe0)

      case Type.Cst(_, _) =>
        // Performance: Reuse tpe0.
        tpe0

      case Type.Apply(tpe1, tpe2, _) =>
        val t1 = visitType(tpe1)
        val t2 = visitType(tpe2)
        // Performance: Reuse tpe0, if possible.
        if ((t1 eq tpe1) && (t2 eq tpe2)) {
          tpe0
        } else {
          Type.Apply(t1, t2, loc)
        }

      case Type.Alias(sym, args, tpe, _) =>
        // Performance: Few aliases, not worth optimizing.
        Type.Alias(sym, args.map(visitType), visitType(tpe), loc)

      case Type.AssocType(sym, args, kind, _) =>
        // // Performance: Few associated types, not worth optimizing.
        Type.AssocType(sym, args.map(visitType), kind, loc)

      case Type.JvmToType(tpe, loc) =>
        Type.JvmToType(visitType(tpe), loc)

      case Type.JvmToEff(tpe, loc) =>
        Type.JvmToEff(visitType(tpe), loc)

      case Type.UnresolvedJvmType(member, loc) =>
        Type.UnresolvedJvmType(member.map(visitType), loc)
    }

    val newBase = visitType(baseType)

    val newTconstrs = sc.tconstrs.map {
      case TraitConstraint(head, tpe0, loc) =>
        val tpe = tpe0.map(visitType)
        TraitConstraint(head, tpe, loc)
    }

    val newEconstrs = sc.econstrs.map {
      case BroadEqualityConstraint(tpe1, tpe2) =>
        BroadEqualityConstraint(visitType(tpe1), visitType(tpe2))
    }

    (newTconstrs, newEconstrs, newBase, substMap)
  }

  /**
    * Generalizes the given type `tpe0` with respect to the empty type environment.
    */
  def generalize(tconstrs: List[TraitConstraint], econstrs: List[BroadEqualityConstraint], tpe0: Type, renv: RigidityEnv)(implicit scope: Scope): Scheme = {
    val tvars = tpe0.typeVars ++ tconstrs.flatMap(tconstr => tconstr.arg.typeVars) ++ econstrs.flatMap(econstr => econstr.tpe1.typeVars ++ econstr.tpe2.typeVars)
    val quantifiers = renv.getFlexibleVarsOf(tvars.toList)
    Scheme(quantifiers.map(_.sym), tconstrs, econstrs, tpe0)
  }
}

/**
  * Representation of polytypes.
  */
case class Scheme(quantifiers: List[Symbol.KindedTypeVarSym], tconstrs: List[TraitConstraint], econstrs: List[BroadEqualityConstraint], base: Type) {

  /**
    * Returns a human readable representation of the polytype.
    */
  override def toString: String = {
    FormatScheme.formatSchemeWithOptions(this, FormatOptions.Internal)
  }

}
