(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList


fun teval (ConI _) _ = IntT
    | teval (ConB _) _ = BoolT
    | teval (ESeq seq) _ =
        let in
            case seq of
                SeqT sequenceT => SeqT sequenceT
            | _ => raise EmptySeq
        end
    | teval (Var var) (env:plcType env) = lookup env var
    | teval (Item (index, exp)) (env:plcType env) =
        let
            fun checkItem (i, []) = raise ListOutOfRange
                | checkItem (i, (hd::[])) = if i = 1 then hd else raise ListOutOfRange
                | checkItem (i, (hd::tl)) = if i = 1 then hd else checkItem (i - 1, tl)
            val vT = teval exp env
        in
            case vT of
                ListT listType => checkItem(index, listType)
            | _ => raise OpNonList
        end
    | teval (Prim1(opr, exp)) (env:plcType env) =
        let
            val expType = teval exp env
        in
            case opr of 
                "!" => 
                    if expType = BoolT then BoolT else raise UnknownType
                | "-" => if expType = IntT then IntT else raise UnknownType
                | "print" => ListT []
                | "tl" => let in
                        case expType of
                            SeqT sequenceType => SeqT sequenceType
                        | _ => raise UnknownType
                    end
                | "hd" => let in
                        case expType of
                            SeqT sequenceType => sequenceType
                        | _ => raise UnknownType
                    end
                | "ise" => let in
                        case expType of
                            SeqT sequenceType => BoolT
                        | _ => raise UnknownType
                    end
                | _ => raise UnknownType
        end
    | teval (Prim2(opr, e1, e2)) (env:plcType env) =
        let
            val e1Type = teval e1 env
            val e2Type = teval e2 env
        in
            case opr of 
                "&&" => if e1Type = BoolT andalso e2Type = BoolT then BoolT else raise UnknownType
                | "+" => if e1Type = IntT andalso e2Type = IntT then IntT else raise UnknownType
                | "-" => if e1Type = IntT andalso e2Type = IntT then IntT else raise UnknownType
                | "*" => if e1Type = IntT andalso e2Type = IntT then IntT else raise UnknownType
                | "/" => if e1Type = IntT andalso e2Type = IntT then IntT else raise UnknownType
                | "=" => if e1Type = e2Type andalso (e1Type = IntT orelse e1Type = BoolT) then BoolT else raise NotEqTypes
                | "!=" => if e1Type = e2Type andalso (e1Type = IntT orelse e1Type = BoolT) then BoolT else raise NotEqTypes
                | "<" => if e1Type = IntT andalso e2Type = IntT then BoolT else raise UnknownType
                | "<=" => if e1Type = IntT andalso e2Type = IntT then BoolT else raise UnknownType
                | "::" => 
                    let in
                        case (e1Type, e2Type) of (IntT, ListT []) => SeqT IntT
                        | (IntT, SeqT sequenceType) => if sequenceType = IntT then SeqT sequenceType else raise NotEqTypes
                        | (BoolT, ListT []) => SeqT BoolT
                        | (BoolT, SeqT sequenceType) => if sequenceType = BoolT then SeqT sequenceType else raise NotEqTypes
                        | (ListT t, ListT []) => SeqT (ListT t)
                        | (ListT t, SeqT sequenceType) => if sequenceType = ListT t then SeqT sequenceType else raise NotEqTypes
                        | _ => raise UnknownType
                    end
                | ";" => e2Type
                | _ => raise UnknownType
        end
    | teval (Match(e1, e2)) (env:plcType env) =
        if null e2 then raise NoMatchResults else
            let
                val initialCond = teval e1 env
                val res = (#2 (hd e2))
                val resType = teval res env
                fun find (Match(e1, e2)) (env:plcType env) =
                        let in
                            case e2 of hd::[] => let in
                                        case hd of
                                            (SOME e2, e3) => 
                                                if (teval e3 env) = resType then
                                                    if initialCond = (teval e2 env) then 
                                                        teval e3 env 
                                                    else raise MatchCondTypesDiff
                                                else raise MatchResTypeDiff
                                        | (NONE, e3) => if (teval e3 env) = resType then resType else raise MatchResTypeDiff
                                    end
                            | hd::tl => let in
                                    case hd of (SOME e2, e3) => 
                                            if (teval e3 env) = resType then
                                                if initialCond = (teval e2 env) then
                                                    find (Match(e1, tl)) env 
                                                else raise MatchCondTypesDiff
                                            else raise MatchResTypeDiff
                                    | _ => raise UnknownType
                                end
                        end
                    | find _ _ = raise UnknownType
            in
                find (Match(e1, e2)) env
            end

    | teval (Let(v, e1, e2)) (env:plcType env) =
        let
            val e1Type = teval e1 env
            val nEnv = (v, e1Type) :: env
        in
            teval e2 nEnv
        end
    | teval (Anon(t, x, e)) (env:plcType env) = 
        let
            val nEnv = (x, t) :: env
            val expType = teval e nEnv
        in
            FunT (t, expType)
        end
    | teval (Call(e2, e1)) (env:plcType env) =
        let
            val e1Type = teval e1 env
            val e2Type = teval e2 env
        in
            case e2Type of FunT (argType, resultType) => 
                    if e1Type = argType then resultType else raise CallTypeMisM
            | _ => raise NotFunc
        end

    | teval (Letrec(funName, argT, arg, funT, e1, e2)) (env:plcType env) =
        let
            val recEnv = (funName, FunT (argT, funT))
            val argEnv = (arg, argT)
            val e1Type = teval e1 (recEnv :: argEnv :: env)
            val e2Type = teval e2 (recEnv :: env)
        in
            if e1Type = funT then e2Type else raise WrongRetType
        end

    | teval (List list1) (env:plcType env) =
        let
            fun checkList (hd::[]) = (teval hd env)::[]
                | checkList (hd::tl) = (teval hd env)::checkList tl
                | checkList _ = []
            val list = checkList list1
        in
            ListT list
        end
    | teval (If(condition, e1, e2)) (env:plcType env) =
        let
            val condType = teval condition env
            val e1Type = teval e1 env
            val e2Type = teval e2 env
        in
            case condType of
                BoolT => if e1Type = e2Type then e1Type else raise DiffBrTypes
            | _ => raise IfCondNotBool
        end


