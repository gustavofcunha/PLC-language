(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc


fun eval (ConI int) _ = IntV int
    | eval (ConB bool) _ = BoolV bool
    | eval (ESeq emptySeq) _ = SeqV []
    | eval (Var var) (env:plcVal env) = lookup env var
    | eval (Item (index, exp)) (env:plcVal env) =
        let
            fun getElementI (index, []) = raise Impossible
                | getElementI (index, (hd::[])) = if index = 1 then hd else raise Impossible
                | getElementI (index, (hd::tl)) = if index = 1 then hd else getElementI (index - 1, tl)
            val value = eval exp env
        in
            case value of ListV list => getElementI (index, list)
            | SeqV s => getElementI (index, s)
            | _ => raise Impossible
        end
    | eval (Prim1 (opr, exp)) (env:plcVal env) =
        let
            val value = eval exp env
        in
            case value of IntV int => 
                    let in
                        case opr of
                            "-" => IntV (~ int)
                        | "print" => 
                            let 
                                val value = IntV int
                                val ignore = print(val2string(value) ^ "\n")
                            in
                                ListV []
                            end
                        | _ => raise Impossible
                    end
            | BoolV bool =>
                let in
                    case opr of "!" => BoolV (not bool)
                    | "print" => 
                        let 
                            val value = BoolV bool
                            val ignore = print(val2string(value) ^ "\n")
                        in
                            ListV []
                        end
                    | _ => raise Impossible
                end
            | SeqV s =>
                let in
                    case opr of 
                        "print" => 
                            let 
                                val ignore = print(list2string(val2string, s) ^ "\n")
                            in
                                ListV []
                            end
                        | "tl" => let in let in SeqV (tl s) end handle Empty => raise TLEmptySeq end
                        | "hd" => let in let in hd s end handle Empty => raise HDEmptySeq end
                        | "ise" =>
                            let in
                                case s of [] => BoolV true
                                | _ => BoolV false
                            end 
                        | _ => raise Impossible
                end
            | ListV list =>
                let in
                    case opr of "print" => 
                            let 
                                val ignore = print(list2string(val2string, list) ^ "\n")
                            in
                                ListV []
                            end
                    | _ => raise Impossible
                end
            | _ => raise Impossible
        end
    | eval (Prim2 (opr, e1, e2)) (env:plcVal env) =
        if opr = ";" then
            let
                val ignore = eval e1 env
            in
                eval e2 env
            end
        else
            let
                val value1 = eval e1 env
                val value2 = eval e2 env
            in
                case (value1, value2) of
                    (IntV int1, IntV int2) => 
                        let in
                            case opr of 
                                "+" => IntV (int1 + int2)
                                | "-" => IntV (int1 - int2)
                                | "*" => IntV (int1 * int2)
                                | "/" => IntV (int1 div int2)
                                | "=" => BoolV (int1 = int2)
                                | "!=" => BoolV (int1 <> int2)
                                | "<" => BoolV (int1 < int2)
                                | "<=" => BoolV (int1 <= int2)
                                | _ => raise Impossible
                        end
                | (BoolV bool1, BoolV bool2) => 
                    let in
                        case opr of 
                            "&&" => BoolV (bool1 andalso bool2)
                            | "=" => BoolV (bool1 = bool2)
                            | "!=" => BoolV (bool1 <> bool2)
                            | _ => raise Impossible
                    end
                | (IntV int1, SeqV s) => 
                    let in
                        case opr of
                            "::" => SeqV (IntV int1 :: s)
                            | _ => raise Impossible
                    end
                | (BoolV bool1, SeqV s) => 
                    let in
                        case opr of
                            "::" => SeqV (BoolV bool1 :: s)
                            | _ => raise Impossible
                    end
                | (ListV l1, SeqV s) => 
                    let in
                        case opr of
                            "::" => SeqV (ListV l1 :: s)
                            | _ => raise Impossible
                    end
                | _ => raise Impossible
            end
    | eval (Match (e1, matchL)) (env:plcVal env) = 
        let 
            val evalMatchV = eval e1 env 
            fun checkMatch (matchV, hd::[]) env =
                    let in
                        case hd of 
                            (SOME e2, e3) => 
                                if matchV = eval e2 env then e3 
                                else raise ValueNotFoundInMatch
                            | (NONE, e3) => e3
                    end
            | checkMatch (matchV, hd::tl) env =  let in
                case hd of 
                    (SOME e2, e3) => 
                        if matchV = eval e2 env then e3 
                        else checkMatch (matchV, tl) env
                    | (NONE, e3) => raise Impossible
                end
            | checkMatch (matchV, _ ) env = raise Impossible
        in
            eval (checkMatch (evalMatchV, matchL) env) env
        end
    | eval (Let (var, e1, e2)) (env:plcVal env) =
        let
            val nEnv = (var, eval e1 env) :: env
        in
            eval e2 nEnv
        end
    | eval (Anon (t, x, e)) (env:plcVal env) = Clos ("", x, e, env) 
    | eval (Call (e1, e2)) (env:plcVal env) = 
        let
            fun getArgs (List (hd::[])) = [eval hd env]
                | getArgs (List (hd::tl)) = [eval hd env] @ getArgs (List tl)
                | getArgs (exp) = [eval exp env]
            val nEnv = [("$list", ListV (getArgs e2))] @ env
            val f = eval e1 env
        in
            case f of
                Clos(name, var, exp, cEnv) =>
                    let
                        val ev = eval e2 nEnv
                        val fEnv = (var, ev)::(name, f)::cEnv
                    in
                        eval exp fEnv
                    end
            | _ => raise NotAFunc
        end
    | eval (Letrec (fName, argT, arg, funT, e1, e2)) (env:plcVal env) =
        let
            val nEnv = (fName, Clos(fName, arg, e1, env)) :: env
        in
            eval e2 nEnv
        end
    | eval (List []) (env:plcVal env) = ListV []
    | eval (List list) (env:plcVal env) = 
        let
            fun unroll (hd::[]) = eval hd env :: []
                | unroll (hd::tl) = eval hd env :: unroll tl
                | unroll _ = raise Impossible;
        in
            ListV (unroll list)
        end
    | eval (If (e1, e2, e3)) (env:plcVal env) = 
        let in
            case eval e1 env of 
                BoolV true => eval e2 env
                | BoolV false => eval e3 env
                | _ => raise Impossible
        end
;