package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.Scheme.generalize
import ca.uwaterloo.flix.language.ast.shared.{AssocTypeDef, BroadEqualityConstraint, Scope, TraitConstraint}
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver.ResolutionResult
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver, TypeConstraint, TypeReduction}
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnvironment, Substitution, TraitEnv}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Options, Result}

object SchemeEquality {
  /**
   * Instantiate one of the variables in the scheme, adding new quantifiers as needed.
   */
  def partiallyInstantiate(sc: Scheme, quantifier: Symbol.KindedTypeVarSym, value: Type, loc: SourceLocation)(implicit scope: Scope): Scheme = sc match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      if (!quantifiers.contains(quantifier)) {
        throw InternalCompilerException("Quantifier not in scheme.", loc)
      }
      val subst = Substitution.singleton(quantifier, value)
      val newTconstrs = tconstrs.map(subst.apply)
      val newEconstrs = econstrs.map(subst.apply)
      val newBase = subst(base)
      generalize(newTconstrs, newEconstrs, newBase, RigidityEnv.empty)
  }

  /**
   * Returns `true` if the given schemes are equivalent.
   */
  // TODO can optimize?
  def equal(sc1: Scheme, sc2: Scheme, traitEnv: TraitEnv, eqEnv: ListMap[Symbol.AssocTypeSym, AssocTypeDef])(implicit scope: Scope, flix: Flix): Boolean = {
    implicit val genSym: GenSym = flix.genSym
    implicit val options: Options = flix.options

    lessThanEqual(sc1, sc2, traitEnv, eqEnv) && lessThanEqual(sc2, sc1, traitEnv, eqEnv)
  }

  /**
   * Returns `true` if the given scheme `sc1` is smaller or equal to the given scheme `sc2`.
   *
   * Θₚ [T/α₂]π₂ ⊫ₑ {π₁, τ₁ = [T/α₂]τ₂} ⤳! ∙ ; R
   * T new constructors
   * ---------------------------------------
   * Θₚ ⊩ (∀α₁.π₁ ⇒ τ₁) ≤ (∀α₂.π₂ ⇒ τ₂)
   */
  private def lessThanEqual(sc1: Scheme, sc2: Scheme, tenv0: TraitEnv, eenv0: ListMap[Symbol.AssocTypeSym, AssocTypeDef])(implicit scope: Scope, flix: Flix): Boolean = {
    implicit val options: Options = flix.options

    // Instantiate sc2, creating [T/α₂]π₂ and [T/α₂]τ₂
    // We use the top scope because this function is only used for comparing schemes, which are at top-level.
    val (cconstrs2_0, econstrs2_0, tpe2_0, _) = Scheme.instantiate(sc2, SourceLocation.Unknown)(Scope.Top, flix.genSym)

    // Resolve what we can from the new econstrs
    // TODO ASSOC-TYPES probably these should be narrow from the start
    val tconstrs2_0 = econstrs2_0.map { case BroadEqualityConstraint(t1, t2) => TypeConstraint.Equality(t1, t2, Provenance.Match(t1, t2, SourceLocation.Unknown)) }
    val (subst, econstrs2_1) = ConstraintSolver.resolve(tconstrs2_0, Substitution.empty, RigidityEnv.empty)(scope, tenv0, eenv0) match {
      case Result.Ok(ResolutionResult(newSubst, newConstrs, _)) =>
        (newSubst, newConstrs)
      case _ => throw InternalCompilerException("unexpected inconsistent type constraints", SourceLocation.Unknown)
    }

    // Anything we didn't solve must be a standard equality constraint
    // Apply the substitution to the new scheme 2
    val econstrs2 = econstrs2_1.map {
      case TypeConstraint.Equality(t1, t2, prov) => EqualityEnvironment.narrow(BroadEqualityConstraint(subst(t1), subst(t2)))
      case _ => throw InternalCompilerException("unexpected constraint", SourceLocation.Unknown)
    }
    val tpe2 = subst(tpe2_0)
    val cconstrs2 = cconstrs2_0.map {
      case TraitConstraint(head, arg, loc) =>
        // should never fail
        val (t, _) = TypeReduction.simplify(subst(arg), RigidityEnv.empty, loc)(scope, eenv0).unsafeGet
        TraitConstraint(head, t, loc)
    }

    // Add sc2's constraints to the environment
    val eenv = ConstraintSolver.expandEqualityEnv(eenv0, econstrs2)
    val cenv = ConstraintSolver.expandTraitEnv(tenv0, cconstrs2)

    // Mark all the constraints from sc2 as rigid
    val tvars = cconstrs2.flatMap(_.arg.typeVars) ++
      econstrs2.flatMap { econstr => econstr.tpe1.typeVars ++ econstr.tpe2.typeVars } ++
      tpe2.typeVars
    val renv = tvars.foldLeft(RigidityEnv.empty) { case (r, tvar) => r.markRigid(tvar.sym) }

    // Check that the constraints from sc1 hold
    // And that the bases unify
    val cconstrs = sc1.tconstrs.map { case TraitConstraint(head, arg, loc) => TypeConstraint.Trait(head.sym, arg, loc) }
    val econstrs = sc1.econstrs.map { case BroadEqualityConstraint(t1, t2) => TypeConstraint.Equality(t1, t2, Provenance.Match(t1, t2, SourceLocation.Unknown)) }
    val baseConstr = TypeConstraint.Equality(sc1.base, tpe2, Provenance.Match(sc1.base, tpe2, SourceLocation.Unknown))
    ConstraintSolver.resolve(baseConstr :: cconstrs ::: econstrs, subst, renv)(scope, cenv, eenv) match {
      // We succeed only if there are no leftover constraints
      case Result.Ok(ResolutionResult(_, Nil, _)) => true
      case _ => false
    }

  }
}
