{
  open Parser
}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']

let id_prefix = lower
let id_suffix = (lower | upper | digit | '_')
let typ_prefix = upper
let typ_suffix = id_suffix

rule token = parse
  | [' ' '\t' '\r' '\n']          { token lexbuf                         }
  | "."                           { DOT                                  }
  | "["                           { LBRACK                               }
  | "]"                           { RBRACK                               }
  | "{"                           { LBRACE                               }
  | "}"                           { RBRACE                               }
  | "nil"                         { NIL                                  }
  | "("                           { LPAREN                               }
  | ")"                           { RPAREN                               }
  | ","                           { COMMA                                }
  | "+"                           { PLUS                                 }
  | "-"                           { MINUS                                }
  | "*"                           { TIMES                                }
  | "/"                           { DIVIDE                               }
  | "="                           { EQ                                   }
  | "<>"                          { NEQ                                  }
  | "<"                           { LT                                   }
  | "<="                          { LE                                   }
  | ">"                           { GT                                   }
  | ">="                          { GE                                   }
  | "&"                           { AND                                  }
  | "|"                           { OR                                   }
  | ";"                           { SEMICOLON                            }
  | ":="                          { ASSIGN                               }
  | "if"                          { IF                                   }
  | "then"                        { THEN                                 }
  | "else"                        { ELSE                                 }
  | "while"                       { WHILE                                }
  | "do"                          { DO                                   }
  | "break"                       { BREAK                                }
  | "of"                          { OF                                   }
  | "var"                         { VAR                                  }
  | ":"                           { COLON                                }
  | "let"                         { LET                                  }
  | "in"                          { IN                                   }
  | "end"                         { END                                  }
  | "function"                    { FUNCTION                             }
  | "type"                        { TYPE                                 }
  | "array"                       { ARRAY                                }
  | id_prefix id_suffix* as lxm   { ID lxm                               }
  | typ_prefix typ_suffix* as lxm { TY lxm                               }
  | digit+ as lxm                 { INT (Int64.of_string lxm)            }
  | '"'                           { str (Buffer.create 10) lexbuf        }
  | eof                           { EOF                                  }

and str buf = parse
  | '\\' 'n'                { Buffer.add_char buf '\n' ; str buf lexbuf   }
  | '"'                     { STR (Buffer.contents buf)                   }
  | _ as c                  { Buffer.add_char buf c; str buf lexbuf       }
