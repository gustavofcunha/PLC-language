(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Convert string to int *)
fun stoi p = case (Int.fromString p) of SOME i => i
                                      | NONE => raise Fail ("Error to convert string '" ^ p ^ "' to int")


(* Identify reserved words *)
fun reservedWord (p, x, y) = case p of "true" => TRUE(x, y)
                                    |  "false" => FALSE(x, y)
                                    |  "if" => IF(x, y)
                                    |  "then" => THEN(x, y)
                                    |  "else" => ELSE(x, y)
                                    |  "Bool" => BOOL(x, y)
                                    |  "Int" => INT(x, y)
                                    |  "var" => VAR(x, y)
                                    |  "Nil" => NIL(x, y)
                                    |  "hd" => HD(x, y)
                                    |  "tl" => TL(x, y)
                                    |  "with" => WITH(x, y)
                                    |  "fun" => FUN(x, y)
                                    |  "fn" => FN(x, y)
                                    |  "ise" => ISE(x, y)
                                    |  "match" => MATCH(x, y)
                                    |  "print" => PRINT(x, y)
                                    |  "rec" => REC(x, y)
                                    |  "end" => END(x, y)
                                    |  "_" => UNDERLINE(x, y)
                                    |  _ => NAME(p, x, y)

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()

%%

%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));

digit = [0-9];
ws = [\ \t];
letter = [A-Za-z];
name = [a-zA-Z_][a-zA-Z_0-9]*;

%%

\n       => (lineNumber := (!lineNumber) + 1; lex());
{ws}+    => (lex());
{digit}+ => (CINT(stoi(yytext), yypos, yypos));
{name} => (reservedWord(yytext, yypos, yypos));
"+" => (PLUS(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"*" => (MULT(yypos, yypos));
"/" => (DIV(yypos, yypos));
"=" => (EQ(yypos, yypos));
"!=" => (NOTEQ(yypos, yypos));
"<" => (LTH(yypos, yypos));
"<=" => (LTEQ(yypos, yypos));
":" => (COLON(yypos, yypos));
"::" => (DBCOLON(yypos, yypos));
";" => (SEMICOL(yypos, yypos));
"," => (COMMA(yypos, yypos));
"!" => (NOT(yypos, yypos));
"&&" => (AND(yypos, yypos));
"[" => (LBRACE(yypos, yypos));
"]" => (RBRACE(yypos, yypos));
"{" => (LCURBR(yypos, yypos));
"}" => (RCURBR(yypos, yypos));
"(" => (LPAR(yypos, yypos));
")" => (RPAR(yypos, yypos));
"=>" => (ASSIGN(yypos, yypos));
"->" => (ARROW(yypos, yypos));
"|" => (PIPE(yypos, yypos));
. => (error ("ignoring bad character"); lex());
