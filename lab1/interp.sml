structure Interp = struct

    structure S = SLP
    (* Uzyj S.CompoundStm, S.Plus, S.IdExp, etc. w odniesieniu do
     * rzeczy zdefiniowanych w strukturze SLP (zobacz slp.sml) *)
    
    type table = (S.id * int) list

    fun update(tbl, ident, newval) = (ident, newval)::tbl
    (*nalezy zalozyc, ze pierwsze wystapienie dowolnego id jest wazniejsze
      od dowolnych pozniejszych wystapien w tabeli*)

    exception UndefinedVariable of string

    fun lookup (tbl:table, ident) = 
          case tbl of 
            nil => raise UndefinedVariable(ident)
          | (x, xval)::xs => if ident=x then xval
			     else lookup(xs, ident)

    exception InterpUnimplemented

    fun interpOp S.Plus  = Int.+
      | interpOp S.Minus = Int.-
      | interpOp S.Times = Int.*
      | interpOp S.Div   = Int.div

    fun interpStm (s:S.stm, tbl:table) = 
      case s of
        S.CompoundStm (s1, s2) => interpStm (s2, (interpStm (s1, tbl)))
      | S.AssignStm (id, value) => update (tbl, id, interpExp (value, tbl))
      | S.PrintStm elist => (printList (elist, tbl); tbl)

    and interpExp (e:S.exp, tbl:table) = 
      case e of
        S.IdExp id => lookup (tbl, id)
      | S.NumExp num => num
      | S.OpExp (exp1, operator, exp2) => (interpOp operator) (interpExp ((exp1, tbl)), (interpExp (exp2, tbl)))
      | S.EseqExp (stm, exp) => (interpStm (stm, tbl); interpExp (exp, tbl))

    and printList (elist: S.exp list, tbl:table) =
      let
        fun interpExpA es = interpExp (es, tbl)
        val numeric = List.map interpExpA elist
        val stringified = List.map Int.toString numeric
        val final = (String.concatWith " " stringified)
      in
        (print (final ^ "\n"); ())
      end
        
    fun interp s = 
          (interpStm(s, nil); ())
end
