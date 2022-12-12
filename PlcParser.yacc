%%

%name PlcParser

%pos int

%term BOOL | ELSE | END | FALSE | FN | FUN
	| HD | IF | INT | ISE | MATCH | NIL | PRINT
	| REC | THEN | TL | TRUE | VAR | WITH | UNDERLINE
	| PLUS | MINUS | MULT | DIV | EQ | NOTEQ | LTH
	| LTEQ | COLON | DBCOLON | SEMICOL | COMMA | NOT
	| AND | LBRACE | RBRACE | LCURBR | RCURBR | LPAR
	| RPAR | ASSIGN | ARROW | PIPE
	| NAME of string | CINT of int
	| EOF


%nonterm Prog of expr
	| Expr of expr
	| Decl of expr
	| AtomicExpr of expr
	| Comps of expr list
	| AppExpr of expr
	| Const of expr
	| CondExpr of expr option
	| Params of (plcType * string) list
	| Args of (plcType * string) list
	| MatchExpr of (expr option * expr) list
	| TypedVar of plcType * string
	| Type of plcType
	| AtomicType of plcType
	| Types of plcType list

%right SEMICOL ARROW
%nonassoc IF
%left ELSE
%left AND
%left EQ NOTEQ
%left LTH LTEQ
%right DBCOLON
%left PLUS MINUS
%left MULT DIV
%nonassoc NOT HD TL ISE PRINT FUN
%left LBRACE

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
	| Decl (Decl)

Decl: VAR NAME EQ Expr SEMICOL Prog (Let(NAME, Expr, Prog))
	| FUN NAME Args EQ Expr SEMICOL Prog (Let(NAME, makeAnon(Args, Expr), Prog))
	| FUN REC NAME Args COLON Type EQ Expr SEMICOL Prog (makeFun(NAME, Args, Type, Expr, Prog))

Expr: AtomicExpr(AtomicExpr)
    | AppExpr(AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | HD Expr (Prim1("hd", Expr))
    | TL Expr (Prim1("tl", Expr))
    | ISE Expr (Prim1("ise", Expr))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | PRINT Expr (Prim1("print", Expr))
    | MINUS Expr (Prim1("-", Expr))
    | NOT Expr (Prim1("!", Expr))
    | Expr PLUS Expr(Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr(Prim2("-", Expr1, Expr2))
    | Expr MULT Expr(Prim2("*", Expr1, Expr2))
    | Expr DIV Expr(Prim2("/", Expr1, Expr2))
    | Expr EQ Expr(Prim2("=", Expr1, Expr2))
    | Expr NOTEQ Expr(Prim2("!=", Expr1, Expr2))
    | Expr LTH Expr(Prim2("<", Expr1, Expr2))
    | Expr LTEQ Expr(Prim2("<=", Expr1, Expr2))
    | Expr DBCOLON Expr(Prim2("::", Expr1, Expr2))
    | Expr SEMICOL Expr(Prim2(";", Expr1, Expr2))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr LBRACE CINT RBRACE (Item(CINT,Expr))

AtomicExpr: Const (Const)
    | NAME (Var(NAME))
    | LCURBR Prog RCURBR (Prog)
    | LPAR Comps RPAR (List(Comps))
    | LPAR Expr RPAR (Expr)
    | FN Args ASSIGN Expr END (makeAnon(Args, Expr))

AppExpr: AtomicExpr AtomicExpr (Call(AtomicExpr1, AtomicExpr2))
	|  AppExpr AtomicExpr (Call(AppExpr, AtomicExpr))

Const: TRUE (ConB true)
    | FALSE (ConB false)
    | CINT (ConI(CINT))
    | LPAR RPAR (List [])
    | LPAR Type LBRACE RBRACE RPAR (ESeq(Type))

CondExpr: UNDERLINE (NONE)
    | Expr (SOME Expr)

Comps: Expr COMMA Expr (Expr1::Expr2::[])
    | Expr COMMA Comps (Expr::Comps)

MatchExpr: END ([])
    | PIPE CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

Params: TypedVar (TypedVar::[])
    | TypedVar COMMA Params (TypedVar::Params)

Args: LPAR RPAR ([])
    | LPAR Params RPAR (Params)

TypedVar: Type NAME ((Type, NAME))

Type: AtomicType (AtomicType)
    | LPAR Types RPAR (ListT(Types))
    | LBRACE Type RBRACE (SeqT(Type))
    | Type ARROW Type (FunT (Type1, Type2))

AtomicType: NIL (ListT [])
    | BOOL (BoolT)
    | INT (IntT)
    | LPAR Type RPAR (Type)

Types: Type COMMA Type (Type1::Type2::[])
    | Type COMMA Types (Type::Types)
