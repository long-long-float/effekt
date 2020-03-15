package effekt
package namer

/**
 * In this file we fully qualify source types, but use symbols directly
 */
import effekt.source
import effekt.source.{ Id, IdRef, IdDef, Tree, Def }
import effekt.source.traversal._

import effekt.symbols._

import effekt.util.scopes._
import effekt.util.messages.{ MessageBuffer, ErrorReporter }

import org.bitbucket.inkytonik.kiama.util.{ Memoiser }

// we could additionally store the scopes / Environments in a separate table

case class Environment(terms: Map[String, TermSymbol], types: Map[String, TypeSymbol])

// Maybe use kiama attributes later...
// TODO make very clear that there is a distinction between:
//   - resolving: that is looking up symbols (might include storing the result into the symbolTable)
//   - binding: that is adding a binding to the environment (lexical.Scope)
class Namer(driver: Driver, config: EffektConfig) { namer =>

  /**
   * The output of this phase: a mapping from source identifier to symbol
   *
   * It contains both, TermSymbols and TypeSymbols
   */
  val symbolTable = new SymbolsDB()

  // Brings the extension methods of assertions into scope
  given Assertions

  def run(path: String, module: source.ModuleDecl, buffer: MessageBuffer): Environment = {

    val topLevelTerms = toplevel[String, TermSymbol](builtins.rootTerms)
    val topLevelTypes = toplevel[String, TypeSymbol](builtins.rootTypes)

    val (terms, types) = module.imports.foldLeft((topLevelTerms, topLevelTypes)) {
      case ((terms, types), source.Import(path)) =>
        val Left(cu) = driver.resolve(path, config)
        (terms.enterWith(cu.exports.terms), types.enterWith(cu.exports.types))
    }

    Context(path, module, module, terms.enter, types.enter, buffer) in {
      resolve(module)
      Environment(Context.terms.bindings.toMap, Context.types.bindings.toMap)
    }
  }

  /**
   * An effectful traversal with two side effects:
   * 1) the passed environment is enriched with definitions
   * 2) names are resolved using the environment and written to the table
   */
  val resolve: Traversal[Tree, Context] = focusing {

    // (1) === Binding Occurrences ===
    case source.ModuleDecl(path, imports, decls) =>
      decls foreach { resolveDef(true) }
      Context scoped { resolveAll(decls) }

    case source.DefStmt(d @ source.ValDef(id, annot, binding), rest) =>
      val tpe = annot.map(resolveValueType)
      resolve(binding)
      Context scoped {
        id := ValBinder(id.localName, tpe, d)
        resolve(rest)
      }

    case source.DefStmt(d @ source.VarDef(id, annot, binding), rest) =>
      val tpe = annot.map(resolveValueType)
      resolve(binding)
      Context scoped {
        id := VarBinder(id.localName, tpe, d)
        resolve(rest)
      }

    case source.DefStmt(d, rest) =>
      Context scoped {
        resolveDef(false)(d)
        resolve(d)
        resolve(rest)
      }

    case source.ValueParam(id, tpe) =>
      id := ValueParam(id.localName, tpe.map(resolveValueType))

    case source.BlockParam(id, tpe) =>
      id := BlockParam(id.localName, resolveBlockType(tpe))

    // FunDef and EffDef have already been resolved as part of the module declaration
    case source.FunDef(id, tparams, params, ret, body) =>
      val funSym = symbolTable(id).asFun
      Context scoped {
        funSym.tparams.foreach { Context.bind }
        bind(funSym.params)
        resolve(body)
      }

    case source.EffDef(id, tparams, params, ret) => ()
    case source.DataDef(id, tparams, ctors) => ()
    case source.ExternType(id, tparams) => ()
    case source.ExternEffect(id, tparams) => ()
    case source.ExternFun(pure, id, tparams, params, ret, body) => ()
    case source.ExternInclude(path) => ()

    case source.TryHandle(body, clauses) =>
      resolve(body)
      Context scoped {
        // we should introduce one ResumeParam for *each* of the clauses to
        // have a separate symbol
        resolveAll(clauses)
      }

    case source.OpClause(op, params, body, resumeId) =>
      Context at op in { op.resolveTerm() }
      val ps = params.map(resolveValueParams)
      Context scoped {
        bind(ps)
        resumeId := ResumeParam()
        resolve(body)
      }

    case source.Clause(op, params, body) =>
      Context at op in { op.resolveTerm() }
      val ps = params.map(resolveValueParams)
      Context scoped {
        bind(ps)
        resolve(body)
      }

    case source.BlockArg(params, stmt) =>
      val ps = resolveValueParams(source.ValueParams(params)) // TODO drop wrapping after refactoring
      Context scoped {
        bind(List(ps))
        resolve(stmt)
      }

    // (2) === Bound Occurrences ===

    case source.Call(id, targs, args) =>
      id.resolveTerm() match {
        case b: BlockParam => ()
        case ResumeParam() => ()
        case f: Fun => ()
        case _ => Context.error("Expected callable")
      }
      targs foreach resolveValueType
      resolveAll(args)

    case source.Var(id) => id.resolveTerm() match {
      case b : BlockParam => Context.error("Blocks have to be fully applied and can't be used as values.")
      case other => other
    }

    // THIS COULD ALSO BE A TYPE!
    case id : IdRef => id.resolveTerm()
    case id : IdDef => Context.abort(s"Compiler error: unresolve binder ${id}")

    case other => resolveAll(other)
  }

  val resolveAll: Traversal[Any, Context] = all(resolve)

  /**
   * Resolve Parameters as part of resolving function signatures
   *
   * Since we annotate functions and effect declarations with resolved types, we need to
   * resolve the parameters.
   *
   * Importantly, resolving them will *not* add the parameters as binding occurence in the current scope.
   * This is done separately by means of `bind`
   */
  def resolveParamSection(params: source.ParamSection)(given Context): List[ValueParam] | BlockParam = params match {
    case ps : source.ValueParams => resolveValueParams(ps)
    case source.BlockParam(id, tpe) =>
      val sym = BlockParam(id.localName, resolveBlockType(tpe))
      symbolTable.put(id, sym)
      sym
  }
  def resolveValueParams(ps: source.ValueParams)(given Context): List[ValueParam] =
    ps.params map { p =>
      val sym = ValueParam(p.id.localName, p.tpe.map(resolveValueType))
      symbolTable.put(p.id, sym)
      sym
    }

  // TODO consider setting owner, instead of this qualify hack
  def resolveDef(qualify: Boolean): Traversal[Def, Context] = {
    def name(id: Id) = if (qualify) id.qualifiedName else id.localName
    focusing {
      case f @ source.FunDef(id, tparams, params, annot, body) =>
        val sym = Context scoped {
          // we create a new scope, since resolving type params introduces them in this scope
          UserFunction(name(id), tparams map resolveTypeParam, params map resolveParamSection, annot map resolveEffectful, f)
        }
        id := sym

      case e @ source.EffDef(id, tparams, params, ret) =>
        // we use the localName for effects, since they will be bound as capabilities
        val effectSym = UserEffect(id.localName, Nil)
        val opSym = Context scoped {
          val tps = tparams map resolveTypeParam
          val tpe = Effectful(resolveValueType(ret), Effects(List(effectSym)))
          EffectOp(id.localName, tps, params map resolveValueParams, Some(tpe), effectSym)
        }
        effectSym.ops = List(opSym)
        id := effectSym
        Context.bind(opSym)

      case d @ source.DataDef(id, tparams, ctors) =>
        val (typ, tps) = Context scoped {
          val tps = tparams map resolveTypeParam
          (DataType(name(id), tps), tps)
        }
        id := typ
        val cs = ctors map {
          case source.Constructor(id, ps) =>
            val sym = Context scoped {
              tps.foreach { Context.bind }
              Constructor(name(id), ps map resolveValueParams, typ)
            }
            id := sym
            sym
        }
        typ.ctors = cs

      case d @ source.ExternType(id, tparams) =>
        id := Context scoped {
          val tps = tparams map resolveTypeParam
          BuiltinType(name(id), tps)
        }

      case d @ source.ExternEffect(id, tparams) =>
        id := Context scoped {
          val tps = tparams map resolveTypeParam
          BuiltinEffect(name(id), tps)
        }

      case d @ source.ExternFun(pure, id, tparams, params, ret, body) =>
        id := Context scoped {
          val tps = tparams map resolveTypeParam
          val ps: Params = params map resolveParamSection
          val tpe = resolveEffectful(ret)
          BuiltinFunction(name(id), tps, ps, Some(tpe), pure, body)
        }

      case d @ source.ExternInclude(path) =>
        d.contents = driver.resolveInclude(Context.path, path, config)
        ()
    }
  }

  def bind(params: List[List[ValueParam] | BlockParam])(given Context): Unit =
    params flatMap {
      case b : BlockParam => List(b)
      case l : List[ValueParam] => l
    } foreach { Context.bind }

  /**
   * Resolve Types
   *
   * resolving a type means reconstructing the composite type (e.g. Effectful, ...) from
   * symbols, instead of trees.
   */

  def resolveValueType(tpe: source.Type)(given Context): ValueType = tpe match {
    case source.TypeApp(id, args) =>
      val data = id.resolveType().asDataType
      if (data.tparams.size != args.size) { Context.error("Wrong number of arguments to " + data) }
      TypeApp(data, args.map(resolveValueType))
    case source.TypeVar(id) => id.resolveType().asValueType
  }

  def resolveBlockType(tpe: source.BlockType)(given Context): BlockType =
    BlockType(Nil, List(tpe.params.map(resolveValueType)), resolveEffectful(tpe.ret))

  def resolveEffect(tpe: source.Effect)(given Context): Effect =
    tpe.id.resolveType().asEffect

  def resolveEffects(tpe: source.Effects)(given Context): Effects =
    Effects(tpe.effs.map(resolveEffect))

  def resolveEffectful(e: source.Effectful)(given Context): Effectful =
    Effectful(resolveValueType(e.tpe), resolveEffects(e.eff))

  def resolveTypeParam(t: Id)(given Context): TypeVar = {
    val sym = TypeVar(t.localName)
    t := sym
    sym
  }

  /**
   * Environment Utils -- we use a mutable cell to express adding definitions more easily
   * The kiama environment uses immutable binding since they thread the environment through
   * their attributes.
   */
  case class Context(
      path: String,
      module: source.ModuleDecl,
      focus: Tree,
      terms: Scope[String, TermSymbol],
      types: Scope[String, TypeSymbol],
      buffer: MessageBuffer
  ) extends ErrorReporter {

    def (id: Id) := (s: TermSymbol): Unit = {
      symbolTable.put(id, s)
      terms.define(id.name, s)
    }

    def (id: Id) := (s: TypeSymbol): Unit = {
      symbolTable.put(id, s)
      types.define(id.name, s)
    }

    def bind(s: TermSymbol): Unit =
      terms.define(s.name.name, s)

    def bind(s: TypeSymbol): Unit =
      types.define(s.name.name, s)

    // lookup and resolve the given id from the environment and
    // store a binding in the symbol table
    def (id: Id) resolveTerm(): TermSymbol = {
      val sym = terms.lookup(id.name, abort(s"Could not resolve term ${id.name}"))
      symbolTable.put(id, sym)
      sym
    }

    def (id: Id) resolveType(): TypeSymbol = {
      val sym = types.lookup(id.name, abort(s"Could not resolve type ${id.name}"))
      symbolTable.put(id, sym)
      sym
    }

    def scoped[R](f: (given Context) => R): R =
      this.copy(terms = terms.enter, types = types.enter) in { f }

    def focusOn(t: Tree) = copy(focus = t)

    def at(node: Tree): Context = copy(focus = node)

    def (id: Id) qualifiedName: Name = QualifiedName(module.path, id.name)
    def (id: Id) localName: Name = LocalName(id.name)
  }
  def Context(given ctx: Context): Context = ctx

  /**
   * Sets the given tree into focus for error reporting
   *
   * Also catches runtime exceptions and turns them into messages
   *
   * TODO drop aborting after refactoring Assertions
   */
  def focusing[T <: Tree, R](f: (given Context) => T => R)(given Context): T => R = t =>
    Context aborting { Context at t in { f(t) } }
}
