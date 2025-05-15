package co.blocke.edi4s
package model


// Keep this file for now. It's not used, but there's learnings required around these contstraints, eg allOf

sealed trait Constraint

case class TypeConstraint(dataType: String) extends Constraint
case class FormatConstraint(format: String) extends Constraint
case class EnumConstraint(values: List[String]) extends Constraint
case class LengthConstraint(minLength: Option[Int], maxLength: Option[Int]) extends Constraint
case class ElementIdConstraint(id: String) extends Constraint
case class SyntaxRuleConstraint(rules: List[String]) extends Constraint
case class AllOfConstraint(allConstraints: List[Constraint]) extends Constraint
case class OneOfConstraint(oneOfConstraints: List[Constraint]) extends Constraint
case class AnyOfConstraint(anyOfConstraints: List[Constraint]) extends Constraint
case class NotConstraint(negated: Constraint) extends Constraint

// Only used on the Canonical side--dereferenced and converted to EnumConstraint for Refined side
case class RefConstraint(ref: String) extends Constraint