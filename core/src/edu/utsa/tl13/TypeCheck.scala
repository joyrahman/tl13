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
    * @param node The node
    * @param decls The [[Program]]'s declarations
    * @return The node's [[Type]]
    */
  private def nodeType(node: Node, decls: Decls): Type = node match {
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
    * @param decls The [[Parse.Program]]'s declarations
    * @param typeMap Map containing types of nodes
    * @param okMap Map containing necessary child information
    * @return true if the node is well-typed
    */
  private def typeOk(node: Node, decls: Decls, typeMap: TypeMap, okMap: TypeOkMap): Boolean = node match {
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
    program.prewalk(TypeMap()) { (m,n) => m + (n -> nodeType(n, program.decls)) }

  /** Builds a [[TypeOkMap]] from a [[Parse.Program]] */
  private def mkTypeOkMap(program: Program, typeMap: TypeMap): TypeOkMap =
    program.postwalk(Map[Node,Boolean]()) {
      (okMap,node) => okMap + (node -> typeOk(node, program.decls, typeMap, okMap))
    }

  /** Returns type information about a [[Parse.Program]]
    *
    * @param program The program
    * @return The program's [[TypeMap]] and [[TypeOkMap]]
    */
  def typeCheck(program: Program): (TypeMap,TypeOkMap) = {
    val typeMap = mkTypeMap(program)
    val okMap = mkTypeOkMap(program, typeMap)
    (typeMap, okMap)
  }

  /** Determines if a program is well-typed
    *
    * @param okMap The program's [[TypeOkMap]]
    * @return true if the program is well-typed
    */
  def isWellTyped(okMap: TypeOkMap): Boolean = okMap.forall(kv => kv._2 == true)

}
