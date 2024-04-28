package scala.meta.internal.pc.completions

import scala.collection.immutable.Nil
import scala.meta.internal.pc.Identifier
import scala.meta.internal.pc.MetalsGlobal
import org.eclipse.{lsp4j => l}

import scala.meta.internal.metals.Fuzzy
import scala.util.chaining.scalaUtilChainingOps

trait ArgCompletions { this: MetalsGlobal =>

  case class ArgCompletion(
      ident: Ident,
      apply: Apply,
      pos: Position,
      text: String,
      completions: CompletionResult
  ) extends CompletionPosition { self =>
    val editRange: l.Range =
      pos.withStart(ident.pos.start).withEnd(pos.start).toLsp
    val funPos = apply.fun.pos
    val method: Tree = typedTreeAt(funPos) match {
      // Functions calls with default arguments expand into this form
      case Apply(Block(defParams, app @ (_: Apply | _: Select)), _)
          if defParams.forall(p => p.isInstanceOf[ValDef]) =>
        app
      case New(c) => c
      case t => t
    }
    val methodSym = method.symbol
    lazy val methodsParams: List[MethodParams] = {
      if (methodSym.isModule) getParamsFromObject(methodSym)
      else if (
        methodSym.isMethod && methodSym.name == nme.apply && methodSym.owner.isModuleClass
      ) getParamsFromObject(methodSym.owner)
      else if (methodSym.isClass)
        List(MethodParams(methodSym.constrParamAccessors))
      else if (method.tpe == null)
        method match {
          case Ident(name) =>
            metalsScopeMembers(funPos)
              .flatMap {
                case m: Member
                    if m.symNameDropLocal == name && m.sym != NoSymbol =>
                  for {
                    params <- m.sym.paramss.headOption
                    if checkIfArgsMatch(params)
                  } yield MethodParams(params)
                case _ => None
              }
          case _ => Nil
        }
      else {
        method.tpe match {
          case OverloadedType(_, alts) =>
            alts.flatMap(_.info.paramss.headOption).map(MethodParams(_))
          case _ =>
            val params =
              method.tpe.paramss.headOption.getOrElse(methodSym.paramss.flatten)
            List(MethodParams(params))
        }
      }
    }

    def checkIfArgsMatch(possibleMatch: List[Symbol]): Boolean = {
      apply.args.length <= possibleMatch.length &&
      !apply.args.zipWithIndex.exists {
        // the identifier we want to generate completions for
        //            v
        // m(arg1 = 3, wri@@)
        case (Ident(name), _) if name.decodedName.endsWith(CURSOR) => false
        case (AssignOrNamedArg(Ident(name), rhs), _) =>
          !possibleMatch.exists { param =>
            name.decodedName == param.name.decodedName &&
            (rhs.tpe == null || rhs.tpe <:< param.tpe)
          }
        case (rhs, i) =>
          rhs.tpe != null && !(rhs.tpe <:< possibleMatch(i).tpe)
      }
    }

    val prefix: String = ident.name.toString.stripSuffix(CURSOR)
    lazy val allParams: List[Symbol] = methodsParams.flatMap(_.allParams)
    lazy val params: List[Symbol] =
      allParams
        .filter(param => param.name.startsWith(prefix))
        .foldLeft(List.empty[Symbol])((acc, curr) =>
          if (acc.exists(el => el.name == curr.name && el.tpe == curr.tpe)) acc
          else curr :: acc
        )

    lazy val isParamName: Set[String] = params.iterator
      .map(_.name)
      .map(_.toString().trim())
      .toSet

    lazy val paramNames: List[String] = params.iterator
      .map(_.name)
      .map(_.toString().trim())
      .toList

    def argPosition(m: Member): Int = m match {
      case o: ArgCompletionTextEditMember =>
        paramNames.indexOf(o.param.nameString.trim())
      case _: NamedArgMember => paramNames.indexOf(m.sym.nameString.trim())
      case _ =>
        params.indexWhere { param =>
          param.tpe != definitions.AnyTpe && memberMatchType(param.tpe, m)
        }
    }

    override def compare(o1: Member, o2: Member): Int = {
      val byPrioritized = super.compare(o1, o2)

      if (byPrioritized == 0 && isPrioritizedCached(o1)) {
        val byArgPosition =
          -java.lang.Integer.compare(argPosition(o1), argPosition(o2))
        if (byArgPosition != 0) {
           byArgPosition
        } else {
          prioritize(o1).compare(prioritize(o2))
        }
      } else byPrioritized
    }

    private def prioritize(member: Member): Int =
      member match {
        case _: ArgCompletionTextEditMember => -1
        case _: NamedArgMember => 0
        case _ => -2
      }

    override def isPrioritized(member: Member): Boolean = member match {
      case _: NamedArgMember => true
      case m: ArgCompletionTextEditMember =>
        isParamName(m.param.nameString.trim())
      case _ =>
        params.exists(param =>
          param.tpe != definitions.AnyTpe && memberMatchType(param.tpe, member)
        )
    }

    private def matchingTypesInScope(
        paramType: Type
    ): List[Symbol] = {

      completions match {
        case members: CompletionResult.ScopeMembers
            if paramType != definitions.AnyTpe =>
          members.results
            .collect {
              case mem if memberMatchType(paramType, mem) =>
                mem.sym
            }
        case _ =>
          Nil
      }
    }

    private def notNothingOrNull(mem: Member): Boolean = {
      !(mem.sym.tpe =:= definitions.NothingTpe || mem.sym.tpe =:= definitions.NullTpe)
    }

    private def memberMatchType(paramType: Type, member: Member): Boolean = {
      member.sym.tpe match {
        case _
            if member.sym.tpe <:< paramType && notNothingOrNull(
              member
            ) && member.sym.isTerm =>
          val name = member.sym.name.toString().trim()
          // None and Nil are always in scope
          name != "Nil" && name != "None"
        case method: MethodType
            if method.resultType <:< paramType && notNothingOrNull(
              member
            ) && member.sym.isTerm =>
          val name = member.sym.name.toString().trim()
          // None and Nil are always in scope
          name != "Nil" && name != "None"
        case lambda: ArgsTypeRef
            if lambda.sym.fullName.startsWith(
              "scala.Function"
            ) && lambda.args.lastOption.exists(tpe =>
              tpe <:< paramType && notNothingOrNull(member) && member.sym.isTerm
            ) =>
          val name = member.sym.name.toString().trim()
          // None and Nil are always in scope
          name != "Nil" && name != "None"
        case _ => false
      }
    }

    private def findDefaultValue(param: Symbol): String = {
      val matchingType = matchingTypesInScope(param.tpe)
        .map(_.name.toString().trim())
      if (matchingType.size == 1) {
        s":${matchingType.head}"
      } else if (matchingType.size > 1) {
        s"|???,${matchingType.mkString(",")}|"
      } else {
        ":???"
      }
    }

    private def fillAllFields(): List[TextEditMember] = {
      val suffix = "autofill"
      val shouldShow =
        allParams.exists(param => param.name.startsWith(prefix))
      val isExplicitlyCalled = suffix.startsWith(prefix)
      val hasParamsToFill = allParams.count(!_.hasDefault) > 1
      if (
        methodsParams.length == 1 && (shouldShow || isExplicitlyCalled) && hasParamsToFill && clientSupportsSnippets
      ) {
        val editText = allParams.zipWithIndex
          .collect {
            case (param, index) if !param.hasDefault => {
              s"${Identifier.backtickWrap(param.name).replace("$", "$$")} = $${${index + 1}${findDefaultValue(param)}}"
            }
          }
          .mkString(", ")
        val edit = new l.TextEdit(editRange, editText)
        List(
          new TextEditMember(
            filterText = s"$prefix-$suffix",
            edit = edit,
            methodSym,
            label = Some("Autofill with default values")
          )
        )
      } else {
        List.empty
      }
    }

    private def findPossibleDefaults(): List[TextEditMember] = {
      params.flatMap { param =>
        val allMembers = matchingTypesInScope(param.tpe)
        allMembers.map { member =>
          val memberName = member.name.toString().trim()
          val editText =
            Identifier.backtickWrap(param.name) + " = " + memberName
          val edit = new l.TextEdit(editRange, editText)
          new ArgCompletionTextEditMember(
            filterText = param.name.toString(),
            edit = edit,
            param = param,
            argValue = member,
            memberName = memberName,
            label = Some(editText)
          )
        }
      }
    }

    private def getParamsFromObject(objectSym: Symbol): List[MethodParams] = {
      objectSym.info.members.flatMap {
        case m if m.name == nme.apply =>
          for {
            params <- m.paramss.headOption
            if (checkIfArgsMatch(params))
          } yield MethodParams(params)
        case _ => None
      }.toList
    }

    override def contribute: List[Member] = {
      if (methodSym == null) Nil
      else
        params.distinct.map(param =>
          new NamedArgMember(param).tap(_ => println(param, param.flagString))
        ) ::: findPossibleDefaults() ::: fillAllFields()
    }

    case class MethodParams(params: List[Symbol], isNamed: Set[Name]) {
      def allParams: List[Symbol] =
        params.filterNot(param => isNamed(param.name) || param.isSynthetic)
    }

    object MethodParams {
      def apply(baseParams: List[Symbol]): MethodParams = {
        val isNamed =
          self.apply.args.iterator
            .filterNot(_ == ident)
            .zip(baseParams.iterator)
            .map {
              case (AssignOrNamedArg(Ident(name), _), _) => name
              case (_, param) => param.name
            }
            .toSet
        MethodParams(baseParams, isNamed)
      }
    }
  }
}
