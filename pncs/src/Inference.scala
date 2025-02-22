import panther.*

enum Constraint {
  case Equality(param: Type, arg: Type)
//  case SubType(param: Type, arg: Type)
}

/** Inference module
  *
  *   1. instantiate all types
  *   2. unify all type pairs
  *   3. infer via substitution
  *
  * @param diagnostics
  *   errors for reporting
  */
case class Inference(diagnostics: DiagnosticBag) {
  var typeVariables: Dictionary[int, Type] = DictionaryModule.empty()

  def freshTypeVariable(location: TextLocation): Type = {
    val variable = Type.Variable(location, typeVariables.length)
    typeVariables = typeVariables.put(typeVariables.length, variable)
    variable
  }

  def get(index: int): Type = {
    val t = typeVariables.getUnsafe(index)
    t match {
      case Type.Variable(_, i) =>
        if (i == index) t
        else {
          // since the type variable index is different, lets replace it with same type variable
          val x = get(i)
          typeVariables = typeVariables.put(index, x)
          x
        }
      case _ => t
    }
  }

  def has(index: int): bool = typeVariables.getUnsafe(index) match {
    case Type.Variable(_, i) => i != index
    case _                   => true
  }

  def substitute(t: Type): Type = {
    t match {
      case Type.Variable(_, i) =>
        if (has(i)) substitute(get(i))
        else t
      case Type.Function(loc, p, r) =>
        Type.Function(loc, substituteParameters(p), substitute(r))

      case Type.GenericFunction(
            loc,
            generics,
            traits,
            parameters,
            returnType
          ) =>
        Type.GenericFunction(
          loc,
          generics,
          substituteList(traits),
          substituteParameters(parameters),
          substitute(returnType)
        )

      case Type.Class(loc, ns, name, args) =>
        Type.Class(loc, ns, name, substituteList(args))

      case _: Type.GenericClass => t
      case Type.Error           => t
      case Type.Any             => t
      case Type.Never           => t
    }
  }

  def unifyConstraint(constraint: Constraint): unit = {
    constraint match
//      case Constraint.SubType(Type.Variable(_, i), Type.Variable(_, arg)) if i == arg =>
      case Constraint.Equality(Type.Variable(_, i), Type.Variable(_, arg))
          if i == arg =>
      case Constraint.Equality(Type.Variable(_, i), arg) if has(i) =>
        unifyConstraint(Constraint.Equality(get(i), arg))
//      case Constraint.SubType(Type.Variable(_, i), arg) if has(i) =>
//        unifyConstraint(Constraint.SubType(get(i), arg))
      case Constraint.Equality(param, Type.Variable(_, i)) if has(i) =>
        unifyConstraint(
          Constraint.Equality(param, get(i))
        )
//      case Constraint.SubType(param, Type.Variable(_, i)) if has(i) => unifyConstraint(
//        Constraint.SubType(
//          param, get(i))
//      )
      case Constraint.Equality(Type.Variable(_, i), arg) =>
        if (occursInType(i, arg)) {
          // circularity
          panic("circularity")
        } else {
          typeVariables = typeVariables.put(i, arg)
        }
      case Constraint.Equality(param, Type.Variable(_, i)) =>
        if (occursInType(i, param)) {
          // circularity
          panic("circularity")
        } else {
          typeVariables = typeVariables.put(i, param)
        }

      case Constraint.Equality(
            Type.Function(loc1, p1, r1),
            Type.Function(loc2, p2, r2)
          ) =>
        if (p1.length != p2.length) {
          panic("Type mismatch: " + loc1.toString() + " vs. " + loc2.toString())
        }
        unifyConstraints(
          buildConstraints(paramTypes(p1), paramTypes(p2), List.Nil)
        )
        unifyConstraint(Constraint.Equality(r1, r2))

      case Constraint.Equality(
            Type.Class(loc1, ns1, name1, args1),
            Type.Class(loc2, ns2, name2, args2)
          ) =>
        if (name1 == name2 && namespaceEquals(ns1, ns2)) {
          unifyConstraints(buildConstraints(args1, args2, List.Nil))
        } else {
          panic(
            "Type mismatch1: " + substitute(
              Type.Class(loc1, ns1, name1, args1)
            ) + " vs. " + substitute(Type.Class(loc2, ns2, name2, args2))
          )
        }
      case Constraint.Equality(Type.Any, Type.Any) =>
      case Constraint.Equality(param, arg) =>
        panic(
          "Type " + substitute(arg) + " is not assignable to " + substitute(
            param
          )
        )
  }

  def buildConstraints(
      params: List[Type],
      args: List[Type],
      acc: List[Constraint]
  ): List[Constraint] =
    params match {
      case List.Nil =>
        args match {
          case List.Nil => acc
          case List.Cons(head, tail) =>
            panic("argument count mismatch")
        }
      case List.Cons(param, tailParams) =>
        args match {
          case List.Nil =>
            panic("argument count mismatch")
          case List.Cons(arg, tailArgs) =>
            buildConstraints(
              tailParams,
              tailArgs,
              List.Cons(Constraint.Equality(param, arg), acc)
            )
        }
    }

  def unify(t1: Type, t2: Type): unit = {
    Tuple2(t1, t2) match {
      case Tuple2(Type.Variable(_, i1), Type.Variable(_, i2)) if i1 == i2 =>
      case Tuple2(Type.Variable(_, i), _) if has(i) => unify(get(i), t2)
      case Tuple2(_, Type.Variable(_, i)) if has(i) => unify(t1, get(i))
      case Tuple2(Type.Variable(_, i), _) =>
        if (occursInType(i, t2)) {
          // circularity
          panic("circularity")
        } else {
          typeVariables = typeVariables.put(i, t2)
        }
      case Tuple2(_, Type.Variable(_, i)) =>
        if (occursInType(i, t1)) {
          // circularity
          panic("circularity")
        } else {
          typeVariables = typeVariables.put(i, t1)
        }

      case Tuple2(Type.Function(loc1, p1, r1), Type.Function(loc2, p2, r2)) =>
        unifyLists(paramTypes(p1), paramTypes(p2))
        unify(r1, r2)

      case Tuple2(
            Type.Class(loc1, ns1, name1, args1),
            Type.Class(loc2, ns2, name2, args2)
          ) =>
        if (name1 == name2 && namespaceEquals(ns1, ns2)) {
          unifyLists(args1, args2)
        } else {
          panic("Type mismatch1: " + substitute(t1) + " vs. " + substitute(t2))
        }
      case Tuple2(Type.Any, Type.Any) =>
      case _ =>
        panic("Type mismatch2: " + substitute(t1) + " vs. " + substitute(t2))
    }
  }

  def namespaceEquals(ns1: List[string], ns2: List[string]): bool = {
    ns1 match {
      case List.Nil => ns2.isEmpty
      case List.Cons(head1, tail1) =>
        ns2 match {
          case List.Nil => false
          case List.Cons(head2, tail2) =>
            (head1 == head2) && namespaceEquals(tail1, tail2)
        }
    }
  }

  def unifyLists(t1: List[Type], t2: List[Type]): unit = {
    t1 match {
      case List.Nil => ()
      case List.Cons(head1, tail1) =>
        t2 match {
          case List.Nil => ()
          case List.Cons(head2, tail2) =>
            unify(head1, head2)
            unifyLists(tail1, tail2)
        }
    }
  }

  def removeNonVarTypes(list: List[Type]): List[Type] = {
    list match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        head match {
          case Type.Variable(_, _) =>
            List.Cons(head, removeNonVarTypes(tail))
          case _ =>
            removeNonVarTypes(tail)
        }
    }
  }

  def substituteParameters(
      parameters: List[BoundParameter]
  ): List[BoundParameter] = {
    parameters match {
      case List.Nil => List.Nil
      case List.Cons(parameter, tail) =>
        val newHead = BoundParameter(parameter.name, substitute(parameter.typ))
        List.Cons(newHead, substituteParameters(tail))
    }
  }

  def substituteList(types: List[Type]): List[Type] = {
    types match {
      case List.Nil => List.Nil
      case List.Cons(typ, tail) =>
        List.Cons(substitute(typ), substituteList(tail))
    }
  }

  def occursInType(index: int, t: Type): bool = {
    t match {
      case Type.Variable(loc, i) if has(i) => occursInType(index, get(i))

      case Type.Variable(loc, i) => i == index
      case Type.Function(loc, p, r) =>
        occursInType(index, r) || occursInTypes(index, paramTypes(p))
      case Type.GenericFunction(
            loc,
            generics,
            traits,
            parameters,
            returnType
          ) =>
        occursInTypes(index, traits) || occursInTypes(
          index,
          paramTypes(parameters)
        ) || occursInType(index, returnType)
      case Type.Class(loc, _, _, args) =>
        occursInTypes(index, args)

      case _: Type.GenericClass => false
      case Type.Any             => false
      case Type.Never           => false
      case Type.Error           => false
    }
  }

  def paramTypes(parameters: List[BoundParameter]): List[Type] = {
    parameters match {
      case List.Nil => List.Nil
      case List.Cons(parameter, tail) =>
        List.Cons(parameter.typ, paramTypes(tail))
    }
  }

  def occursInTypes(index: int, types: List[Type]): bool = {
    types match {
      case List.Nil => false
      case List.Cons(typ, tail) =>
        occursInType(index, typ) || occursInTypes(index, tail)
    }
  }

  /** Ensure that a type is instantiated such that there are no GenericTypes and
    * all GenericTypeParameters are replaced with fresh type variables.
    *
    * @param instantiation
    *   a mapping of generic type names to types
    * @param t
    * @return
    */
  def instantiate(instantiation: Dictionary[string, Type], t: Type): Type = {
    ???
//    t match {
////      case Type.GenericType(loc, generics, traits, uninstantiatedType) =>
////        if (generics.length == 0) {
////          uninstantiatedType
////        } else {
////          val newInstantiation =
////            instantiateGenerics(generics, DictionaryModule.empty())
////          val typ = instantiate(newInstantiation, uninstantiatedType)
////          // TODO: traits
////          // val newTraits = traits.map(instantiate(newInstantiation, _))
////          typ
////        }
//      case _ if instantiation.length == 0 => t
//      case Type.Function(loc, p, r) =>
//        Type.Function(loc, instantiateParameters(instantiation, p), instantiate(instantiation, r))
//      case Type.Variable(_, i) if has(i) => instantiate(instantiation, get(i))
//      case Type.Class(loc, ns, name, args) =>
//        instantiation.get(name) match {
//          case Option.Some(instantiationType) =>
//            if (args.length > 0) {
//              panic("diagnostics.reportHigherKindedType(t)")
//            }
//            instantiationType
//          case Option.None =>
//            Type.Class(loc, ns, name, instantiateList(instantiation, args))
//        }
//      case v: Type.Variable => v
//      case Type.Any => Type.Any
//      case Type.Never => Type.Never
//      case Type.Error => Type.Error
//    }
  }

  def instantiateParameters(
      instantiation: Dictionary[string, Type],
      parameters: List[BoundParameter]
  ): List[BoundParameter] = {
    parameters match {
      case List.Nil => List.Nil
      case List.Cons(parameter, tail) =>
        List.Cons(
          BoundParameter(
            parameter.name,
            instantiate(instantiation, parameter.typ)
          ),
          instantiateParameters(instantiation, tail)
        )
    }
  }

  def instantiateList(
      instantiation: Dictionary[string, Type],
      types: List[Type]
  ): List[Type] = {
    types match {
      case List.Nil => List.Nil
      case List.Cons(typ, tail) =>
        List.Cons(
          instantiate(instantiation, typ),
          instantiateList(instantiation, tail)
        )
    }
  }

  def instantiateGenerics(
      generics: List[GenericTypeParameter],
      dict: Dictionary[string, Type]
  ): Dictionary[string, Type] = {
    generics match {
      case List.Nil => dict
      case List.Cons(generic, tail) =>
        instantiateGenerics(
          tail,
          if (dict.contains(generic.name)) {
            dict
          } else {
            dict.put(generic.name, freshTypeVariable(generic.location))
          }
        )
    }
  }

  def instantiateGenericsFromTypes(
      types: List[Type],
      generics: List[GenericTypeParameter],
      dict: Dictionary[string, Type]
  ): Dictionary[string, Type] = {
    types match {
      case List.Nil => instantiateGenerics(generics, dict)
      case List.Cons(head, tail) =>
        head match {
//          case Type.GenericType(_, moreGenerics, _, _) =>
//            instantiateGenericsFromTypes(tail, ListModule.concat(generics, moreGenerics), dict)
          case _ =>
            instantiateGenericsFromTypes(tail, generics, dict)
        }
    }
  }

  def unifyConstraints(constraints: List[Constraint]): unit = {
    constraints match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        unifyConstraint(instantiateConstraint(head))
        unifyConstraints(tail)
    }
  }

  def instantiateConstraint(constraint: Constraint): Constraint = {
    constraint match {
      case Constraint.Equality(param, arg) =>
        Constraint.Equality(
          instantiate(DictionaryModule.empty(), param),
          instantiate(DictionaryModule.empty(), arg)
        )
//      case Constraint.SubType(param, arg) =>
//        Constraint.SubType(
//          instantiate(DictionaryModule.empty(), param),
//          instantiate(DictionaryModule.empty(), arg)
//        )
    }
  }

  /** Infer a function type given a list of constraints
    *
    * @param constraints
    *   list of constraints
    * @param typ
    *   should already be an instantiated type
    * @return
    */
  def inferFunction(
      constraints: List[Constraint],
      typ: Type.Function
  ): Type.Function = {
    unifyConstraints(constraints)

    Type.Function(
      typ.location,
      substituteParameters(typ.parameters),
      substitute(typ.returnType)
    )
  }

  /** Infer a function type given a list of constraints
    *
    * @param constraints
    *   list of constraints
    * @param typ
    *   should already be an instantiated type
    * @return
    */
  def inferNamed(constraints: List[Constraint], typ: Type.Class): Type.Class = {
    unifyConstraints(constraints)

    Type.Class(typ.location, typ.ns, typ.name, substituteList(typ.args))
  }
}
