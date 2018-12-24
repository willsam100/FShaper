namespace CsToFs
open Microsoft.FSharp.Compiler.Ast

[<NoEquality; NoComparison;RequireQualifiedAccess>]
type Expr  = 

    /// F# syntax: (expr)
    ///
    /// Paren(expr, leftParenRange, rightParenRange, wholeRangeIncludingParentheses)
    ///
    /// Parenthesized expressions. Kept in AST to distinguish A.M((x,y))
    /// from A.M(x,y), among other things.
    | Paren of expr:SynExpr

    /// F# syntax: 1, 1.3, () etc.
    | Const of constant:SynConst

    /// F# syntax: expr : type
    | Typed of  expr:SynExpr

    /// F# syntax: e1, ..., eN
    | Tuple of  exprs:SynExpr list 

    ///// F# syntax: struct (e1, ..., eN)
    //| StructTuple of  exprs:SynExpr list

    ///// F# syntax: [ e1; ...; en ], [| e1; ...; en |]
    //| ArrayOrList of  isList:bool * exprs:SynExpr list * range:range

    /// F# syntax: new C(...)
    /// The flag is true if known to be 'family' ('protected') scope
    | New of isProtected:bool * typeName:SynType
    
    /// F# syntax: 'while ... do ...'
    | While of whileSeqPoint:SequencePointInfoForWhileLoop * whileExpr:SynExpr * doExpr:SynExpr

    /// F# syntax: 'for i = ... to ... do ...'
    | For of forSeqPoint:SequencePointInfoForForLoop * ident:Ident * identBody:SynExpr * bool * toBody:SynExpr * doBody:SynExpr

    /// SynExpr.ForEach (spBind, seqExprOnly, isFromSource, pat, enumExpr, bodyExpr, mWholeExpr).
    ///
    /// F# syntax: 'for ... in ... do ...'
    | ForEach of forSeqPoint:SequencePointInfoForForLoop * seqExprOnly:SeqExprOnly * isFromSource:bool * pat:SynPat * enumExpr:SynExpr * bodyExpr:SynExpr

    /// F# syntax: [ expr ], [| expr |]
    | ArrayOrListOfSeqExpr of isArray:bool * expr:SynExpr

    /// CompExpr(isArrayOrList, isNotNakedRefCell, expr)
    ///
    /// F# syntax: { expr }
    | CompExpr of isArrayOrList:bool * isNotNakedRefCell:bool ref * expr:SynExpr

    /// First bool indicates if lambda originates from a method. Patterns here are always "simple"
    /// Second bool indicates if this is a "later" part of an iterated sequence of lambdas
    ///
    /// F# syntax: fun pat -> expr
    | Lambda of  fromMethod:bool * inLambdaSeq:bool * args:SynSimplePats * body:SynExpr

    ///// F# syntax: function pat1 -> expr | ... | patN -> exprN
    //| MatchLambda of isExnMatch:bool * range * SynMatchClause list * matchSeqPoint:SequencePointInfoForBinding * range:range

    ///// F# syntax: match expr with pat1 -> expr | ... | patN -> exprN
    //| Match of  matchSeqPoint:SequencePointInfoForBinding * expr:SynExpr * clauses:SynMatchClause list * isExnMatch:bool * range:range (* bool indicates if this is an exception match in a computation expression which throws unmatched exceptions *)

    ///// F# syntax: do expr
    //| Do of  expr:SynExpr * range:range

    /// F# syntax: assert expr
    | Assert of expr:SynExpr

    /// App(exprAtomicFlag, isInfix, funcExpr, argExpr, m)
    ///  - exprAtomicFlag: indicates if the application is syntactically atomic, e.g. f.[1] is atomic, but 'f x' is not
    ///  - isInfix is true for the first app of an infix operator, e.g. 1+2 becomes App(App(+,1),2), where the inner node is marked isInfix
    ///      (or more generally, for higher operator fixities, if App(x,y) is such that y comes before x in the source code, then the node is marked isInfix=true)
    ///
    /// F# syntax: f x
    | App of ExprAtomicFlag * isInfix:bool * funcExpr:SynExpr * argExpr:SynExpr

    /// TypeApp(expr, mLessThan, types, mCommas, mGreaterThan, mTypeArgs, mWholeExpr)
    ///     "mCommas" are the ranges for interstitial commas, these only matter for parsing/design-time tooling, the typechecker may munge/discard them
    ///
    /// F# syntax: expr<type1,...,typeN>
    //| TypeApp of expr:SynExpr * LESSrange:range * typeNames:SynType list * commaRanges:range list * GREATERrange:range option * typeArgsRange:range * range:range

    /// LetOrUse(isRecursive, isUse, bindings, body, wholeRange)
    ///
    /// F# syntax: let pat = expr in expr
    /// F# syntax: let f pat1 .. patN = expr in expr
    /// F# syntax: let rec f pat1 .. patN = expr in expr
    /// F# syntax: use pat = expr in expr
    | LetOrUse of isRecursive:bool * isUse:bool * bindings:SynBinding list * body:SynExpr

    /// F# syntax: try expr with pat -> expr
    | TryWith of tryExpr:SynExpr * withCases:SynMatchClause list * trySeqPoint:SequencePointInfoForTry * withSeqPoint:SequencePointInfoForWith

    /// F# syntax: try expr finally expr
    | TryFinally of tryExpr:SynExpr * finallyExpr:SynExpr * trySeqPoint:SequencePointInfoForTry * finallySeqPoint:SequencePointInfoForFinally

    /// F# syntax: lazy expr
    | Lazy of SynExpr

    /// Seq(seqPoint, isTrueSeq, e1, e2, m)
    ///  isTrueSeq: false indicates "let v = a in b; v"
    ///
    /// F# syntax: expr; expr
    | Sequential of seqPoint:SequencePointInfoForSeq * isTrueSeq:bool * expr1:SynExpr * expr2:SynExpr

    ///  IfThenElse(exprGuard,exprThen,optionalExprElse,spIfToThen,isFromErrorRecovery,mIfToThen,mIfToEndOfLastBranch)
    ///
    /// F# syntax: if expr then expr
    /// F# syntax: if expr then expr else expr
    | IfThenElse of ifExpr:SynExpr * thenExpr:SynExpr * elseExpr:SynExpr option * spIfToThen:SequencePointInfoForBinding * isFromErrorRecovery:bool

    /// F# syntax: ident
    /// Optimized representation, = SynExpr.LongIdent(false,[id],id.idRange)
    | Ident of string

    /// F# syntax: ident.ident...ident
    /// LongIdent(isOptional, longIdent, altNameRefCell, m)
    ///   isOptional: true if preceded by a '?' for an optional named parameter
    ///   altNameRefCell: Normally 'None' except for some compiler-generated variables in desugaring pattern matching. See SynSimplePat.Id
    | LongIdent of isOptional:bool * longDotId:LongIdentWithDots * altNameRefCell:SynSimplePatAlternativeIdInfo ref option

    /// F# syntax: ident.ident...ident <- expr
    | LongIdentSet of longDotId:LongIdentWithDots * expr:SynExpr
    /// DotGet(expr, rangeOfDot, lid, wholeRange)
    ///
    /// F# syntax: expr.ident.ident
    | DotGet of expr:SynExpr

    /// F# syntax: expr.ident...ident <- expr
    | DotSet of SynExpr * longDotId:LongIdentWithDots * SynExpr

    /// F# syntax: expr <- expr
    | Set of SynExpr * SynExpr

    /// F# syntax: expr.[expr,...,expr]
    | DotIndexedGet of SynExpr * SynIndexerArg list

    /// DotIndexedSet (objectExpr, indexExprs, valueExpr, rangeOfLeftOfSet, rangeOfDot, rangeOfWholeExpr)
    ///
    /// F# syntax: expr.[expr,...,expr] <- expr
    | DotIndexedSet of objectExpr:SynExpr * indexExprs:SynIndexerArg list * valueExpr:SynExpr

    /// F# syntax: Type.Items(e1) <- e2 , rarely used named-property-setter notation, e.g. Foo.Bar.Chars(3) <- 'a'
    | NamedIndexedPropertySet of longDotId:LongIdentWithDots * SynExpr * SynExpr

    /// F# syntax: expr.Items(e1) <- e2 , rarely used named-property-setter notation, e.g. (stringExpr).Chars(3) <- 'a'
    //| DotNamedIndexedPropertySet of SynExpr * longDotId:LongIdentWithDots * SynExpr * SynExpr * range:range

    /// F# syntax: expr :? type
    | TypeTest of  expr:SynExpr * typeName:SynType

    /// F# syntax: expr :> type
    | Upcast of  expr:SynExpr * typeName:SynType 

    /// F# syntax: expr :?> type
    | Downcast of  expr:SynExpr * typeName:SynType 
    /// F# syntax: upcast expr
    | InferredUpcast of  expr:SynExpr 

    /// F# syntax: downcast expr
    | InferredDowncast of  expr:SynExpr 

    /// F# syntax: null
    | Null 

    /// F# syntax: &expr, &&expr
    | AddressOf of  isByref:bool * SynExpr 

    /// F# syntax: ((typar1 or ... or typarN): (member-dig) expr)
    | TraitCall of SynTypar list * SynMemberSig * SynExpr 

    /// F# syntax: ... in ...
    /// Computation expressions only, based on JOIN_IN token from lex filter
    //| JoinIn of SynExpr * SynExpr 

    /// F# syntax: <implicit>
    /// Computation expressions only, implied by final "do" or "do!"
    //| ImplicitZero of range:range

    /// F# syntax: yield expr
    /// F# syntax: return expr
    /// Computation expressions only
    //| YieldOrReturn   of (bool * bool) * expr:SynExpr * range:range

    /// F# syntax: yield! expr
    /// F# syntax: return! expr
    /// Computation expressions only
    //| YieldOrReturnFrom  of (bool * bool) * expr:SynExpr * range:range

    /// SynExpr.LetOrUseBang(spBind, isUse, isFromSource, pat, rhsExpr, bodyExpr, mWholeExpr).
    ///
    /// F# syntax: let! pat = expr in expr
    /// F# syntax: use! pat = expr in expr
    /// Computation expressions only
    | LetOrUseBang    of bindSeqPoint:SequencePointInfoForBinding * isUse:bool * isFromSource:bool * SynPat * SynExpr * SynExpr

    /// F# syntax: match! expr with pat1 -> expr | ... | patN -> exprN
    //| MatchBang of  matchSeqPoint:SequencePointInfoForBinding * expr:SynExpr * clauses:SynMatchClause list * isExnMatch:bool * range:range (* bool indicates if this is an exception match in a computation expression which throws unmatched exceptions *)

    /// F# syntax: do! expr
    /// Computation expressions only
    | DoBang      of expr:SynExpr
    /// Only used in FSharp.Core
    //| LibraryOnlyILAssembly of ILInstr array *  SynType list * SynExpr list * SynType list * range:range (* Embedded IL assembly code *)

    /// 'use x = fixed expr'
    | Fixed of expr:SynExpr

//type Line = 
    //| Line of string 
    //| Expr of Expr

//module Line = 

    //let append (Line l) (s:string)  = 
    //    l + s |> Line

    //let prepend (s:string) (Line l) = 
    //    s + l |> Line

    //let join sep (Line l) (s:string) =
    //    sprintf "%s%s%s" l sep s |> Line

    //let concat sep lines = 
        //lines 
        //|> Seq.map (fun (Line l) -> l)
        //|> String.concat sep
        //|> Line

type Line = 
    | Line of string

module Line = 
    let concat a b = Line ""
    let append a b = Line "" 
    let prepend a b = Line "" 


type Parameter = {
    Type:string
    Name:string
}

type Ctor = {
    Body:Line
    Parameters: Parameter list
    SubclassArgs: string list
}

type Method = {
    Name:string
    Parameters:Parameter list
    Body: Line
    ReturnType:string
    IsVirtual:bool
    IsAsync:bool
    IsPrivate: bool
    IsOverride:bool
    Accessibility:SynAccess option
}

type Prop = {
    Name:string
    Type:string
    Get: Line
    Set: Line
}

type ClassName = {
    Name:string
    Generics:string list
}

type Field = {
    IsPublic: bool // this syntax is not supported in F# 
    Name:string
    Type: string
    Initilizer:string option
}

type Attribute = {
    Name:string
    Parameters:string option
}

type Class = {
    Name:ClassName
    Constructors:Ctor list
    Fields: Field list
    Methods: Method list
    Properties: Prop list
    Attributes: Attribute list
    BaseClass: string option
    ImplementInterfaces: string list
    TypeParameters:string list
}

type Interface = {
    Name:string
    // TDOO Generics
    Methods:string list
}

type UsingStatement = {
    Namespace:string
}

type Namespace = {
    Name:string
    Interfaces: Interface list
    Classes: Class list
}

type File = {
    UsingStatements:UsingStatement list
    Namespaces:Namespace list
}

type FsharpSyntax = 
    | File of File
    | UsingStatement of UsingStatement
    | Namespace of Namespace
    | Interface of Interface
    | Class of Class
    | Field of Field seq
    | Prop of Prop
    | Method of Method
    | Empty
