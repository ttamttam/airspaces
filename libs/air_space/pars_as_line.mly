%{
    open AirSpace
%}

%token <string> COMMENT, AIRCLASS, ACNAME
%token <float> FLOAT
%token <int> INT
%token PEN, BRUSH, CIRCLE, ALTHIGHT, ALTLOW, VALUE, POINT, ARC, COMMA, COLON
%token POSITIVE, NEGATIVE, EQUAL, FEET, AMSL, AGL, SFC, NORTH, SOUTH, EAST
%token WEST, CENTER, DIRECTION, FLIGHT_LEVEL , METER , UNLIMITED , LABEL , VOID
%start <AirSpace.line> line

%%

line:
| a=AIRCLASS                              { match
                                              airclass_of_string a
                                            with
                                            | Some ac -> AirClass ac
                                            | None -> assert false                           }
| a=ACNAME                                { AirClassName a                                  }
| a=COMMENT                               { Comment a                                       }
| PEN st=INT COMMA width=INT COMMA c=rgb  { PenStyle { style =
                                                         ( match line_style_of_enum st with
                                                           | Some ls -> ls
                                                           | None -> assert false);
                                                       width;
                                                       color= c }                           }
| BRUSH c=rgb                             { BrushStyle ( brush_style_of_rgb c )             }
| CIRCLE r=int_or_float                   { CircleRadius r                                  }
| ALTHIGHT a=alt+ VOID                    { AltH a                                          }
| ALTLOW a=alt+   VOID                    { AltL a                                          }
| POINT p=pt                              { EdgePoint p                                     }
| ARC start=pt COMMA stop=pt              { ArcPoints (start, stop)                         }
| VALUE CENTER EQUAL p=pt                 { SetCenter p                                     }
| VALUE DIRECTION EQUAL d=dir             { SetDir d                                        }
| LABEL p=pt                              { SetLabel p                                      }
| VOID                                    { Void                                            }

dir:
| POSITIVE                                { true                                            }
| NEGATIVE                                { false                                           }

int_or_float:
| v=INT                                   { float v                                         }
| v=FLOAT                                 { v                                               }

alt:
| a=altv AMSL                             { Altitude a                                      }
| a=altv AGL                              { Hauteur a                                       }
| SFC                                     { Hauteur 0                                       }
| FLIGHT_LEVEL alt=INT                    { FlightLevel (alt * 30)                          }
| UNLIMITED                               { Unlimited                                       }

altv:
| alt=INT METER                           { alt                                             }
| alt=INT FEET?                           { truncate (float alt *. 0.3)                     }

pt:
| la=coord lo=coord                       { Gps.of_deg ~lat:la ~lon:lo                    }

coord:
| d=INT COLON m=INT COLON s=INT g=sens    { float (d*3600+m*60+s) /. 3600. *. g             }

sens:
| NORTH                                   {1.                                               }
| EAST                                    {1.                                               }
| SOUTH                                   {-1.                                              }
| WEST                                    {-1.                                              }

rgb:
| r=INT COMMA g=INT COMMA b=INT           { (r, g, b)                                       }
