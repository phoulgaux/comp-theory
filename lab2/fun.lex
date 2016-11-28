type pos = ErrorMsg.pos
type svalue = Tokens.svalue
type ('svalue,'pos) token = ('svalue,'pos) Tokens.token
type lexresult  = (svalue,ErrorMsg.pos) token

val newLine = ErrorMsg.newLine

fun make_pos (yypos,yytext) : ErrorMsg.pos2
    = (yypos, yypos + String.size(yytext) - 1)

fun make_pos_int (yypos,yytext) : int * int * int
    = (valOf (Int.fromString yytext), yypos, yypos + String.size(yytext) - 1)

fun make_pos_proj (yypos,yytext) : int * int * int =
  ((Option.valOf (Int.fromString  (String.extract(yytext, 1, NONE)))), yypos, (yypos + String.size(yytext) - 1))

fun make_pos_str (yypos,yytext) : string * int * int
    = (yytext, yypos, yypos + String.size(yytext) - 1)

(* Obsluga EOF. Nalezy zwrocic uwage, ze funkcja zwraca wadliwe file-position dla
   end-of-file. Z racji konstrukcji ML-Lex'a jest mozliwe,
   aczkolwiek skomplikowa, uzyskac dostep do wlasciwego file-position. Mozna to zrobic
   z wykorzystaniem %arg w ML-Lex, ale na tym etapie
   nie musicie sie tym zbytnio przejmowac
*)
fun eof () =
     Tokens.EOF(0,0)

%%

%s COMMENT;
%header (functor FunLexFun(structure Tokens: Fun_TOKENS));

alpha = [A-Za-z];
digit = [0-9];

%%
<INITIAL>"fun"   => (Tokens.FUN(make_pos(yypos,yytext)));
<INITIAL>\n      => (newLine yypos; continue ());
<INITIAL>"->"    => (Tokens.ARROW(make_pos(yypos,yytext)));
<INITIAL>"in"    => (Tokens.IN(make_pos(yypos,yytext)));
<INITIAL>"let"   => (Tokens.LET(make_pos(yypos,yytext)));
<INITIAL>"else"  => (Tokens.ELSE(make_pos(yypos,yytext)));
<INITIAL>"then"  => (Tokens.THEN(make_pos(yypos,yytext)));
<INITIAL>"if"    => (Tokens.IF(make_pos(yypos,yytext)));
<INITIAL>":="    => (Tokens.ASSIGN(make_pos(yypos,yytext)));
<INITIAL>"!"     => (Tokens.BANG(make_pos(yypos,yytext)));
<INITIAL>"ref"   => (Tokens.REF(make_pos(yypos,yytext)));
<INITIAL>"do"    => (Tokens.DO(make_pos(yypos,yytext)));
<INITIAL>"while" => (Tokens.WHILE(make_pos(yypos,yytext)));
<INITIAL>"||"    => (Tokens.OR(make_pos(yypos,yytext)));
<INITIAL>"not"   => (Tokens.NOT(make_pos(yypos,yytext)));
<INITIAL>"&"     => (Tokens.AND(make_pos(yypos,yytext)));
<INITIAL>">"     => (Tokens.GT(make_pos(yypos,yytext)));
<INITIAL>"="     => (Tokens.EQ(make_pos(yypos,yytext)));
<INITIAL>"<"     => (Tokens.LT(make_pos(yypos,yytext)));
<INITIAL>"#"[0-9]+ => (Tokens.PROJ(make_pos_proj(yypos,yytext)));
<INITIAL>"*"     => (Tokens.TIMES(make_pos(yypos,yytext)));
<INITIAL>"-"     => (Tokens.MINUS(make_pos(yypos,yytext)));
<INITIAL>"+"     => (Tokens.PLUS(make_pos(yypos,yytext)));
<INITIAL>")"     => (Tokens.RPAREN(make_pos(yypos,yytext)));
<INITIAL>"("     => (Tokens.LPAREN(make_pos(yypos,yytext)));
<INITIAL>":"     => (Tokens.COLON(make_pos(yypos,yytext)));
<INITIAL>";"     => (Tokens.SEMICOLON(make_pos(yypos,yytext)));
<INITIAL>","     => (Tokens.COMMA(make_pos(yypos,yytext)));
<INITIAL>{alpha}+   => (Tokens.ID(make_pos_str(yypos,yytext)));
<INITIAL>-?{digit}+ => (Tokens.INT(make_pos_int(yypos,yytext)));
<INITIAL>.       => (continue ());
