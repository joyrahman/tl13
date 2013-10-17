package edu.utsa.tl13

import Parse._

/** Type checking functionality */
object TypeCheck {

  /** Lookup table for node types */
  type TypeMap = Map[Node,Type]

  object TypeMap {
    /** Creates a [[TypeMap]] */
    def apply(ps: (Node,Type)*) = Map[Node,Type](ps:_*)
  }

  /** Lookup table for nodes being well typed */
  type TypeOkMap = Map[Node,Boolean]

  object TypeOkMap {
    /** Creates a [[TypeOkMap]] */
    def apply(ps: (Node,Boolean)*) = Map[Node,Boolean](ps:_*)
  }

  /** Calculates the type of a node
    *
    * @param decls The [[Program]]'s declarations
    * @param node The node
    * @return The node's [[Type]]
    */
  private def nodeType(decls: Decls)(node: Node): Type = node match {
      case Decl(_,t)                                     => t
      case Op(o,_,_) if o.matches("=|!=|<|>|<=|>=")      => TL13Bool()
      case Op(o,_,_) if o.matches("\\+|\\-|\\*|div|mod") => TL13Int()
      case _: Num                                        => TL13Int()
      case _: BoolLit                                    => TL13Bool()
      case Ident(v)                                      =>
        decls.decls.foldLeft(TL13None().asInstanceOf[Type]) { (t,d) => if (d.value == v) d.typ else t }
      case _                                             => TL13None()
    }

  /** Determines if a node is well-typed
    *
    * @param node The node
    * @param okMap Map containing necessary child information
    * @param typeMap Map containing types of nodes
    * @param decls The [[Parse.Program]]'s declarations
    * @return true if the node is well-typed
    */
  private def typeOk(node: Node, okMap: TypeOkMap, typeMap: TypeMap, decls: Decls): Boolean = node match {
      case ReadInt(i)      => okMap(i) && typeMap(i) == TL13Int()
      case WriteInt(e)     => okMap(e) && typeMap(e) == TL13Int()
      case If(e, _, _)     => okMap(e) && typeMap(e) == TL13Bool()
      case While(e, _)     => okMap(e) && typeMap(e) == TL13Bool()
      case Program(ds, ss) => ds.decls.forall(okMap(_)) && ss.stmts.forall(okMap(_))
      case AsgnExpr(i,e)   => okMap(i) && okMap(e) && typeMap(i) == typeMap(e)
      case ds: Decls       => ds.decls.map(_.value).distinct.size == ds.decls.size
      case Op(_, e1, e2)   => okMap(e1) && okMap(e2) && typeMap(e1) == TL13Int() && typeMap(e2) == TL13Int()
      case Ident(v)        => decls.decls.map(_.value).exists(_ == v)
      case Num(v)          => if (v.toInt >= 0 && v.toInt <= 2147483647) true else false
      case _: BoolLit      => true
      case _: Type         => true
      case _: Decl         => true
      case _: StatementSeq => true
      case _               => false
    }

  /** Builds a [[TypeMap]] from a [[Parse.Program]] */
  private def mkTypeMap(program: Program): TypeMap =
    program.prewalk(TypeMap()) { (m,n) => m + (n -> nodeType(program.decls)(n)) }

  /** Builds a [[TypeOkMap]] from a [[Parse.Program]] */
  private def mkTypeOkMap(program: Program, typeMap: TypeMap): TypeOkMap =
    program.postwalk(Map[Node,Boolean]()) { (m,n) => m + (n -> typeOk(n, m, typeMap, program.decls)) }

  /** Determines if a [[Parse.Program]] is well-typed
    *
    * @param program The [[Parse.Program]]
    * @return A Right with the [[TypeMap]] and [[TypeOkMap]] if the program is well-typed,
              otherwise a Left with the same information
    */
  def typeCheck(program: Program): Either[(TypeMap,TypeOkMap),(TypeMap,TypeOkMap)] = {
    val typeMap = mkTypeMap(program)
    val okMap = mkTypeOkMap(program, typeMap)
    okMap.forall(kv => kv._2 == true) match {
      case true  => Right(typeMap, okMap)
      case false => Left(typeMap, okMap)
    }
  }

}
