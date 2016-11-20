structure MaxArg = struct

    structure S = SLP
    (* Uzyj S.CompoundStm, S.Plus, S.IdExp, etc. w odniesieniu do
     * rzeczy zdefiniowanych w strukturze SLP (zobacz slp.sml) *)

    exception MaxUnimplemented

    fun maxarg s = 
      case s of
        S.CompoundStm(s1, s2) =>
          let val args1 = maxarg s1
	          val args2 = maxarg s2
    	  in Int.max (args1, args2)
	      end
      | S.AssignStm(x, e) => maxExpArg e
      | S.PrintStm elist =>
          case elist of
            [] => 0
          | e::es => Int.max((maxExpArg e), 1 + (maxarg (S.PrintStm es)))
          
    and maxExpArg e =
      case e of
        S.IdExp _ => 0
      | S.NumExp _ => 1
      | S.OpExp (e1, _, e2) => Int.max(maxExpArg e1, maxExpArg e2)
      | S.EseqExp (s, e) =>  Int.max(maxarg s, maxExpArg e)
end
