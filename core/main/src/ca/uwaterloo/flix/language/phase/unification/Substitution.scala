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

import ca.uwaterloo.flix.language.ast.shared.{BroadEqualityConstraint, EqualityConstraint, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{Scheme, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Companion object for the [[Substitution]] class.
  */
object Substitution {
  /**
    * Returns the empty substitution.
    */
  val empty: Substitution = Substitution(Map.empty)

  /**
    * Returns the singleton substitution mapping the type variable `x` to `tpe`.
    */
  def singleton(x: Symbol.KindedTypeVarSym, tpe: Type): Substitution = {
    // Ensure that we do not add any x -> x mappings.
    tpe match {
      case y: Type.Var if x.id == y.sym.id => empty
      case y: Type.Var if y.sym.text isStrictlyLessPreciseThan x.text => Substitution(Map(x -> y.withText(x.text)))
      case y: Type.Var if x.text isStrictlyLessPreciseThan y.sym.text => Substitution(Map(x.withText(y.sym.text) -> y))
      case _ => Substitution(Map(x -> tpe))
    }
  }

}

/**
  * A substitution is a map from type variables to types.
  */
case class Substitution(m: Map[Symbol.KindedTypeVarSym, Type]) extends BasicSubstitution(m) {
  /**
    * Applies `this` substitution to the given type constraint `tc`.
    */
  def apply(tc: TraitConstraint): TraitConstraint = if (isEmpty) tc else tc.copy(arg = apply(tc.arg))

  /**
    * Applies `this` substitution to the given type scheme `sc`.
    *
    * NB: Throws an InternalCompilerException if quantifiers are present in the substitution.
    */
  def apply(sc: Scheme): Scheme = sc match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      if (sc.quantifiers.exists(m.contains)) {
        throw InternalCompilerException("Quantifier in substitution.", base.loc)
      }
      Scheme(quantifiers, tconstrs.map(apply), econstrs.map(apply), apply(base))
  }

  /**
    * Applies `this` substitution to the given equality constraint.
    */
  def apply(ec: EqualityConstraint): EqualityConstraint = if (isEmpty) ec else ec match {
    case EqualityConstraint(cst, t1, t2, loc) => EqualityConstraint(cst, apply(t1), apply(t2), loc)
  }

  /**
    * Applies `this` substitution to the given equality constraint.
    */
  def apply(ec: BroadEqualityConstraint): BroadEqualityConstraint = if (isEmpty) ec else ec match {
    case BroadEqualityConstraint(t1, t2) => BroadEqualityConstraint(apply(t1), apply(t2))
  }

  /**
    * Applies `this` substitution to the given provenance.
    */
  // TODO: PERF: Do we really need to apply the subst. aggressively to provenance?
  def apply(prov: TypeConstraint.Provenance): TypeConstraint.Provenance = prov match {
    case Provenance.ExpectType(expected, actual, loc) =>
      val e = apply(expected)
      val a = apply(actual)
      // Performance: Reuse prov, if possible.
      if ((e eq expected) && (a eq actual))
        prov
      else
        Provenance.ExpectType(e, a, loc)

    case Provenance.ExpectEffect(expected, actual, loc) =>
      val e = apply(expected)
      val a = apply(actual)
      // Performance: Reuse prov, if possible.
      if ((e eq expected) && (a eq actual))
        prov
      else
        Provenance.ExpectEffect(e, a, loc)

    case Provenance.ExpectArgument(expected, actual, sym, num, loc) =>
      val e = apply(expected)
      val a = apply(actual)
      // Performance: Reuse prov, if possible.
      if ((e eq expected) && (a eq actual))
        prov
      else
        Provenance.ExpectArgument(e, a, sym, num, loc)

    case Provenance.Match(tpe1, tpe2, loc) =>
      val t1 = apply(tpe1)
      val t2 = apply(tpe2)

      // Performance: Reuse prov, if possible.
      if ((t1 eq tpe1) && (t2 eq tpe2))
        prov
      else
        Provenance.Match(t1, t2, loc)
  }

  /**
    * Removes the binding for the given type variable `tvar` (if it exists).
    */
  def unbind(tvar: Symbol.KindedTypeVarSym): Substitution = Substitution(m - tvar)

  /**
    * Returns the left-biased composition of `this` substitution with `that` substitution.
    */
  def ++(that: Substitution): Substitution = {
    if (this.isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      Substitution(
        this.m ++ that.m.filter(kv => !this.m.contains(kv._1))
      )
    }
  }

  /**
    * Returns the composition of `this` substitution with `that` substitution.
    */
  def @@(that: Substitution): Substitution = {
    // Case 1: Return `that` if `this` is empty.
    if (this.isEmpty) {
      return that
    }

    // Case 2: Return `this` if `that` is empty.
    if (that.isEmpty) {
      return this
    }

    // Case 3: Merge the two substitutions.

    // Performance: Use of mutability improve performance.
    import scala.collection.mutable
    val mutMap = mutable.Map.empty[Symbol.KindedTypeVarSym, Type]

    // Add all bindings in `that`. (Applying the current substitution).
    for ((x, t) <- that.m) {
      val tpe = this.apply(t)
      mutMap.update(x, tpe)
    }

    // Performance: We now switch back to building the immutable map in `result`.

    // Add all bindings in `this` that are not in `that`.
    var result = mutMap.toMap
    for ((x, t) <- this.m) {
      if (!that.m.contains(x)) {
        result = result.updated(x, t)
      }
    }

    Substitution(result)
  }
}