type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1 (p2)

datatype state = INIT | COMM | STR
val curr = ref INIT

val comm_count = ref 0
fun clean (s :string) =
  let fun init [] = []
        | init (c::r) = case c of #"\\" => backslash(r)
                                | #"\"" => init(r)
                                | _ => c::init(r)
      and backslash (c::r) =
                case c of #"n" => #"\n"::init(r)
                        | #"t" => #"\t"::init(r)
                        | (#"\"" | #"\\") => c::init(r)
                        | #"^" => control(r)
                        | _ => if (#"0" <= c andalso c <= #"9") then
                                  digit(c::r) else ignore(r)
      and control (c::r) = (chr (ord c - ord #"@"))::init(r)
      and digit (a::b::c::r) = (chr (valOf (Int.fromString(implode (a::b::c::nil)))))::init(r)
      and ignore (c::r) = if c = #"\\" then init(r) else ignore(r)
  in
    implode (init (explode s))
  end

val eof = fn () => let
                    val pos = hd(!linePos)
                   in
                   case !curr of
                     INIT => Tokens.EOF(pos,pos)
                   | COMM => (err(pos, "Unclosed comment");
                                 Tokens.EOF(pos,pos))
                   | STR => (err(pos, "Unclosed string");
                                   Tokens.EOF(pos,pos))
                   end
%%
digits=[0-9]+;
alphanum=[a-zA-Z0-9_];
ws = [\ \t];
%s COMMENT STRING;
%%
<INITIAL>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>{ws}+  =>  (continue());

<INITIAL>var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL>while  	=> (Tokens.WHILE(yypos,yypos+5));
<INITIAL>for  	=> (Tokens.FOR(yypos,yypos+3));
<INITIAL>to  	=> (Tokens.TO(yypos,yypos+2));
<INITIAL>break  	=> (Tokens.BREAK(yypos,yypos+5));
<INITIAL>let  	=> (Tokens.LET(yypos,yypos+3));
<INITIAL>in  	=> (Tokens.IN(yypos,yypos+2));
<INITIAL>end  	=> (Tokens.END(yypos,yypos+3));
<INITIAL>function  	=> (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>type  	=> (Tokens.TYPE(yypos,yypos+4));
<INITIAL>array  	=> (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>if  	=> (Tokens.IF(yypos,yypos+2));
<INITIAL>then  	=> (Tokens.THEN(yypos,yypos+4));
<INITIAL>else  	=> (Tokens.ELSE(yypos,yypos+4));
<INITIAL>do  	=> (Tokens.DO(yypos,yypos+2));
<INITIAL>of  	=> (Tokens.OF(yypos,yypos+2));
<INITIAL>nil  	=> (Tokens.NIL(yypos,yypos+3));

<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>":"	=> (Tokens.COLON(yypos,yypos+1));
<INITIAL>";"	=> (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("	=> (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"	=> (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"["	=> (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"	=> (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"{"	=> (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"	=> (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"."	=> (Tokens.DOT(yypos,yypos+1));
<INITIAL>"+"	=> (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"	=> (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"*"	=> (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"/"	=> (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"="	=> (Tokens.EQ(yypos,yypos+1));
<INITIAL>"<>"	=> (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"<"	=> (Tokens.LT(yypos,yypos+1));
<INITIAL>"<="	=> (Tokens.LE(yypos,yypos+2));
<INITIAL>">"	=> (Tokens.GT(yypos,yypos+1));
<INITIAL>">="	=> (Tokens.GE(yypos,yypos+2));
<INITIAL>"&"	=> (Tokens.AND(yypos,yypos+1));
<INITIAL>"|"	=> (Tokens.OR(yypos,yypos+1));
<INITIAL>":="	=> (Tokens.ASSIGN(yypos,yypos+2));

<INITIAL>"/*"	=> (YYBEGIN COMMENT; curr := COMM;
                  comm_count := !comm_count + 1; continue());
<INITIAL>"*/" => (ErrorMsg.error yypos ("illegal character " ^ yytext);
                  continue());
<COMMENT>"/*"	=> (comm_count := !comm_count + 1; continue());
<COMMENT>"*/"	=> (comm_count := !comm_count - 1;
                    if !comm_count = 0
                    then (YYBEGIN INITIAL; curr := INIT)
                    else curr := COMM;
                    continue());
<COMMENT>.|\n	=> (continue());

<INITIAL>[a-zA-Z]{alphanum}*  => (Tokens.ID(yytext, yypos, yypos + size yytext));
<INITIAL>{digits} => (Tokens.INT(valOf (Int.fromString yytext),
                                 yypos, yypos + size yytext));

<INITIAL>\" => (YYBEGIN STRING; curr := STR; continue());
<STRING>([ !#-\[\]-~]|\\n|\\t|\\\^[@-_]|\\[0-9]{3}|\\\"|\\\\|\\[ \t\n\f]*\\)*\"
            => (YYBEGIN INITIAL; curr := INIT;
                Tokens.STRING(clean(yytext), yypos - 1,
                              yypos - 1 + size yytext));
<STRING>.|\n =>
(ErrorMsg.error yypos ("illegal character inside string" ^ yytext); continue());

<INITIAL>. => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
