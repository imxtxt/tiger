%{
  open Ast
%}


%token <string> ID
%token <string> TY
%token <Int64.t> INT
%token <string> STR
%token DOT         "."
%token LBRACK      "["
%token RBRACK      "]"
%token LBRACE      "{"
%token RBRACE      "}"
%token NIL         "nil"
%token LPAREN      "("
%token RPAREN      ")"
%token COMMA       ","
%token PLUS        "+"
%token MINUS       "-"
%token TIMES       "*"
%token DIVIDE      "/"
%token EQ          "="              
%token NEQ         "<>"             
%token LT          "<"              
%token LE          "<="             
%token GT          ">"              
%token GE          ">="             
%token AND         "&"              
%token OR          "|"             
%token SEMICOLON   ";"             
%token ASSIGN      ":="             
%token IF          "if"             
%token THEN        "then"             
%token ELSE        "else"             
%token WHILE       "while"             
%token DO          "do"        
%token BREAK       "break"        
%token OF          "of"        
%token VAR         "var"        
%token COLON       ":"   
%token LET         "let"   
%token IN          "in"   
%token END         "end"
%token FUNCTION    "function"
%token TYPE        "type"
%token ARRAY       "array"
%token EOF

%nonassoc "of" "do"
%nonassoc WITHOUT_ELSE 
%nonassoc "else"
%nonassoc ":="
%left "|"
%left "&"
%nonassoc "=" "<>" "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/"

%start <Ast.exp> start


%%
start:
| e = exp EOF { e }

exp:
| var = var                                                         { Var var                  }
| "nil"                                                             { Nil                      }
| num = INT                                                         { Int num                  }
| str = STR                                                         { String str               }
| id = ID "(" es = separated_list(",", exp) ")"                     { Call (id, es)            }
| e1 = exp aop = aop e2 = exp                                       { Aop (e1, aop, e2)        }
| e1 = exp cop = cop e2 = exp                                       { Cop (e1, cop, e2)        }
| e1 = exp "&" e2 = exp                                             { If3 (e1, e2, Int 0L)     }
| e1 = exp "|" e2 = exp                                             { If3 (e1, Int 1L, e2)     }
| ty = TY "{" fs = separated_list(",", exp_field) "}"               { Record (ty, fs)          }
| "(" ")"                                                           { Seq ([])                 }
| "(" e1 = exp  ")"                                                 { e1                       }
| "(" e1 = exp ";" es = separated_nonempty_list(";", exp) ")"       { Seq (e1 :: es)           }
| var = var ":=" e1 = exp                                           { Assign (var, e1)         }
| "if" e1 = exp "then" e2 = exp "else" e3 = exp                     { If3 (e1, e2, e3)         }
| "if" e1 = exp "then" e2 = exp %prec WITHOUT_ELSE                  { If2 (e1, e2)             }
| "while" e1 = exp "do" e2 = exp                                    { While (e1, e2 )          }
| "break"                                                           { Break                    }
| ty = TY "[" e1 = exp "]" "of" e2 = exp                            { Array (ty, e1, e2) : exp }
| "let" ds = list(dec) "in" es = separated_list(";", exp) "end"     { Let (ds, Seq es)         }

var:
| id = ID                   { Simple id          }
| var = var "." f = ID      { Field (var, f)     }
| var = var "[" e = exp "]" { Subscript (var, e) }

exp_field:
| id = ID "=" exp = exp { id, exp }

%inline aop:
| "+"  { Plus   }
| "-"  { Minus  }
| "*"  { Times  }
| "/"  { Divide }

%inline cop:
| "="  { Eq }
| "<>" { Ne }
| ">"  { Gt }
| ">=" { Ge }
| "<"  { Lt }
| "<=" { Le }

dec:
| fs = fundec  { Func_dec [fs] }
| vd = vardec  { Var_dec vd    }
| td = typedec { Type_dec [td] }

fundec:
| "function" name = ID "(" params = separated_list(",", param) ")" ret_ty = type_anon? "=" body = exp { { name; params; ret_ty; body } }

param:
| name = ID ":" ty = TY { { name; escape = ref false; ty} }

vardec:
| "var" name = ID ty = type_anon? ":=" init = exp { { name; escape = ref false; ty; init } }

type_anon:
| ":" ty = TY { ty }

typedec:
| "type" name = TY "=" ty_desc = ty_desc { { name; ty_desc } }

ty_desc:
| name = TY                                     { Name name          }
| "{" fs = separated_list(",", type_field) "}"  { Record fs          }
| "array" "of" ty = TY                          { Array ty : ty_desc }

type_field:
| id = ID ":" ty = TY { id, ty }
