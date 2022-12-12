(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun run exp =
    let
        val eType = teval exp []
        val eRe = eval exp []
    in
        val2string(eRe) ^ " : " ^ type2string(eType)
    end

    handle EmptySeq =>  "Plc Checker (EmptySeq): A sequência de entrada não contém nenhum elemento."
    | UnknownType => "Plc Checker (UnknownType): Operador desconhecido."
    | NotEqTypes =>  "Plc Checker (NotEqTypes): Os tipos comparados são diferentes."
    | WrongRetType =>  "Plc Checker (WrongRetType): Tipo de retorno incorreto para função definida."
    | DiffBrTypes =>  "Plc Checker (DiffBrTypes): Os tipos da expressões dos possíveis caminhos de 'if' divergem."
    | IfCondNotBool =>  "Plc Checker (IfCondNotBool): A condição de 'if' não é booleana."
    | NoMatchResults =>  "Plc Checker (NoMatchResults): Não há resultados definidos para 'match'."
    | MatchResTypeDiff =>  "Plc Checker (MatchResTypesDiff): Pelo menos um dos tipos dos casos de 'match' difere dos demais."
    | MatchCondTypesDiff =>  "Plc Checker (MatchCondTypesDiff): O tipo das opções de 'match' difere do tipo da expressão passada para Match"
    | CallTypeMisM =>  "Plc Checker (CallTypesMisM): Type mismatch in function call."
    | NotFunc =>  "teval Call (NotFunc): Você está tentando chamar algo que não é uma função."
    | ListOutOfRange =>  "Plc Checker (ListOutOfRange): Tentativa de acessar um elemento fora dos limites da lista."
    | OpNonList =>  "Tentativa de acessar um elemento em uma expressão que não é uma lista."
    | Impossible => "Plc Interp (Impossible): Impossível avaliar expressão."
    | HDEmptySeq =>   "Plc Interp (HDEmptySeq): argumento para 'hd' é uma sequência vazia."
    | TLEmptySeq =>  "Plc Interp (TLEmptySeq): argumento para 'tl' é uma sequência vazia."
    | ValueNotFoundInMatch =>  "Plc Interp (ValueNotFoundInMatch): valor não encontrado em 'match'."
    | NotAFunc =>  "Plc Interp (NotAFunc): eval Call: não é uma função."
    | SymbolNotFound => "(SymbolNotFound): símbolo não permitido ou não encontrado."
    | _ => "Erro desconhecido."