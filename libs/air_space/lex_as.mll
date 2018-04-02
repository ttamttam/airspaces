{
  open Lexing
  open Pars_as_line
}

let white = [' ' '\t']
let digit = ['0'-'9']
let sign = ['+' '-']
let int = sign? digit+
let float = sign? digit+ '.' digit+

rule general = parse
| white* eof                { VOID                      }
| white+                    { general lexbuf            }
| '*' (_+ as c)             { COMMENT (String.trim c)   }
| "AC" (_+ as c)            { AIRCLASS (String.trim c)  }
| "AN" (_+ as s)            { ACNAME (String.trim s)    }
| "SP"                      { PEN                       }
| "SB"                      { BRUSH                     }
| "DC"                      { CIRCLE                    }
| "AH"                      { ALTHIGHT                  }
| "AL"                      { ALTLOW                    }
| "V"                       { VALUE                     }
| "DP"                      { POINT                     }
| "DB"                      { ARC                       }
| "AT"                      { LABEL                     }
| 'M'                       { METER                     }
| ','                       { COMMA                     }
| ':'                       { COLON                     }
| '+'                       { POSITIVE                  }
| '-'                       { NEGATIVE                  }
| '='                       { EQUAL                     }
| (float as f)              { FLOAT (float_of_string f) }
| (int as i)                { INT (int_of_string i)     }
| 'F' | "FT" | "ft"         { FEET                      }
| "FL"                      { FLIGHT_LEVEL              }
| "AMSL"                    { AMSL                      }
| "AAGL" | "AGL" | "ASFC"   { AGL                       }
| "SFC" | "GND"             { SFC                       }
| 'N'                       { NORTH                     }
| 'S'                       { SOUTH                     }
| 'E'                       { EAST                      }
| 'W' | 'w'                 { WEST                      }
| 'X'                       { CENTER                    }
| 'D'                       { DIRECTION                 }
| "UNL"                     { UNLIMITED                 }
