package swaggerboot

import io.swagger.models.Swagger
import io.swagger.models.properties.{ArrayProperty, ObjectProperty, Property}
import swaggerboot.swaggerops._

import scala.annotation.tailrec

object Definitions {
  case class SortingDefinition(md: ModelDefinition, references: Seq[String]) {
    val name = md.name
  }

  case class DefinitionRef(name: String, referencedNames: Set[String])

  /**
   * This function both orders input definitions, *and* identifies those with cycles, for optional separate treatments...
   * This may not be the best way to do this...
   *
   * @return (SortedSeq, NamesOfDefinitionsInCycles)
   */
  def order(unsorted: Seq[ModelDefinition])(cyclicWarning: String => Unit = _ => ()): (Seq[SortingDefinition], Seq[DefinitionRef]) = {

    // calculate the referenced definitions for each definition...
    val defsWithRefNames: Seq[SortingDefinition] = unsorted.map { d =>
      SortingDefinition(d, d.attributes.flatMap(_.referencedName))
    }

    @tailrec
    def recur(remaining: Seq[SortingDefinition], acc: Seq[SortingDefinition], cycleAcc: Seq[DefinitionRef], bumps: Seq[DefinitionRef]): (Seq[SortingDefinition], Seq[DefinitionRef]) = {

      def inAcc(name: String, accumulator: Seq[SortingDefinition] = acc) = accumulator.exists(_.name == name)

      // Cleanse bumps
      val cleansedBumps = bumps.foldLeft(Seq.empty[DefinitionRef]) {
        case (newBumps, DefinitionRef(bumpedName, bumpedRefs)) if bumpedRefs.forall(inAcc(_)) =>
          newBumps
        case (newBumps, DefinitionRef(bumpedName, bumpedRefs)) =>
          newBumps :+ DefinitionRef(bumpedName, bumpedRefs.filterNot(inAcc(_)))
      }

      def isBumped(name: String) = bumps.exists(_.name == name)
      def allBumped(refs: Set[String]) = refs.forall(isBumped)
      def refsAllBumped(name: String) = bumps.exists(b => b.name == name && allBumped(b.referencedNames))

      def hasCycle(bumpToCheck: DefinitionRef): Boolean = {
        def recur(bump: DefinitionRef, seen: Set[String]): Boolean = {
          // its circular when all its references are bumps, and all those have references that are bumps, etc., etc... recursively.
          // of course, this will result in infinite loop for the circular cases, so track the refs already checked, and if this
          // function gets to check one again, its an obvious cycle.
          if (seen.contains(bump.name)) {
            true
          } else if (!allBumped(bump.referencedNames)) {
            false
          } else {
            bump.referencedNames.flatMap(b => bumps.find(_.name == b)).exists(recur(_, seen + bump.name))
          }
        }
        recur(bumpToCheck, Set.empty)
      }

      val circularRefs: Set[String] = cleansedBumps.filter(hasCycle).map(_.name).toSet

      val newCycleAcc = cycleAcc ++ cleansedBumps.filter(hasCycle)

      circularRefs.foreach(cyclicWarning)

      val newBumps = bumps.filterNot(b => circularRefs.contains(b.name))
      val newAcc = acc ++ remaining.filter(r => circularRefs.contains(r.name))
      val newRemaining = remaining.filterNot(r => circularRefs.contains(r.name))

      // Process remaining for head/tail
      newRemaining match {
        case Nil =>
          (newAcc, newCycleAcc)
        case head +: tail if head.references.forall(r => inAcc(r, newAcc) || head.name == r) =>
          recur(tail, newAcc :+ head, newCycleAcc, newBumps)
        case head +: tail =>
          recur(tail :+ head, newAcc, newCycleAcc, newBumps :+ DefinitionRef(head.name, head.references.toSet))
      }
    }

    recur(defsWithRefNames, Nil, Nil, Nil)
  }

  /**
   * A Synthetic model is one that is built without an actual "Definition", but rather, in response to "object" properties, with
   * corresponding embedded "properties"...
   * It gets named based on its "parent" which may itself be synthetic, but no matter.
   * These need to get merged into the real "definitions" before ordering - these are needed for all code generation...
   *
   * this gathers all parse errors as we go, and build up definitions using "replacements"
   */
  def getSynthetics(swagger: Swagger): (Seq[ModelDefinition], Seq[ParseError]) = {

    // FIXME delve into MapProperty.additionalProperties here also.

    def makeDefinition(defName: String, prop: ObjectProperty): (ModelDefinition, Seq[ParseError]) = {
      val attrsAndErrors = prop.propertiesMap.map { case (propName, prop) =>
        val (scalaType, required, refname, error) = prop.scalaType(defName, propName).fold(
          l = parseError => (parseError.replacement, parseError.required, None, Some(parseError)),
          r = { case (stype, required, refname) => (stype, required, refname, None) }
        )
        (ModelAttribute(propName, prop.swaggerType, scalaType, required, refname, prop.modeledEnum), error)
      }

      // FIXME - change this to retain attribute order as per Swagger input...
      val (attrs, errors) = attrsAndErrors.foldLeft((Seq.empty[ModelAttribute], Seq.empty[ParseError])) {
        case ((attrs, errors), (attr, errOpt)) =>
          (attrs :+ attr, errOpt.fold(errors)(errors :+ _))
      }

      (ModelDefinition(defName, attrs.toList, false), errors)
    }

    def recur(parentName: String, namedProperties: Seq[(String, Property)], defAcc: Seq[ModelDefinition] = Nil, errAcc: Seq[ParseError] = Nil): (Seq[ModelDefinition], Seq[ParseError]) = {
      namedProperties match {
        case Nil => (defAcc, errAcc)
        case head +: tail =>
          val (newDefs, newErrs) = head match {
            case (propName, oprop: ObjectProperty) =>
              val (defs, errs) = recur(
                syntheticModelName(parentName, propName),
                oprop.propertiesMap().toSeq)
              val (newDef, newErrors) = makeDefinition(syntheticModelName(parentName, propName), oprop)
              (defs :+ newDef, errs ++ newErrors)
            case (propName, aprop: ArrayProperty) if aprop.getItems.isInstanceOf[ObjectProperty] =>
              val oprop = aprop.getItems.asInstanceOf[ObjectProperty]
              val (defs, errs) = recur(
                syntheticModelName(parentName, propName),
                oprop.propertiesMap().toSeq)
              val (newDef, newErrors) = makeDefinition(syntheticModelName(parentName, propName), oprop)
              (defs :+ newDef, errs ++ newErrors)
            case _ =>
              (Nil, Nil)
          }
          recur(parentName, tail, defAcc ++ newDefs, errAcc ++ newErrs)
      }
    }

    // FIXME - expand this to include synthetics to be derived from objects defined inline in responses - note, this may be blocked by the swagger parser logic...

    swagger.definitionsMap.toList.foldLeft((Seq.empty[ModelDefinition], Seq.empty[ParseError])) {
      case ((defsAcc, errAcc), (name, model)) =>
        val (defs, errs) = recur(name, model.properties.toSeq)
        (defsAcc ++ defs, errAcc ++ errs)
    }
  }

  def syntheticModelName(parentName: String, propertyName: String): String = {
    s"${codegen.camelOf(parentName)}${codegen.camelOf(propertyName)}"
  }
}
