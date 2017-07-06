(*!m2iso*) (* Copyright (c) 2017 Modula-2 Software Foundation *)

IMPLEMENTATION MODULE M2Meter;

(* Modula-2 Source Code Metrics *)

IMPORT Infile, String;

FROM String IMPORT StringT; (* alias for String.String *)


CONST
  NUL   = CHR(0);
  TAB   = CHR(9);
  LF    = CHR(10);
  VT    = CHR(11);
  SPACE = CHR(32);
  DEL   = CHR(127);
  SINGLEQUOTE = CHR(39);
  DOUBLEQUOTE = CHR(34);


(* ---------------------------------------------------------------------------
 * procedure Measure(infile)
 * ---------------------------------------------------------------------------
 * Measures file infile and passes back metrics in out-parameter metrics.
 * ------------------------------------------------------------------------ *)

PROCEDURE Measure ( infile : InfileT; VAR metrics : Metric );

VAR
  next : CHAR;
  isSloc : BOOLEAN;

BEGIN
  (* init flag *)
  isSloc := FALSE;
  
  (* init counters *)
  metrics.lines := 0;
  metrics.slocs := 0;
  metrics.comments := 0;
  metrics.semicolons := 0;
  metrics.procedures := 0;
  
  (* TO DO : add code to count procedure declarations *)
  
  (* read chars from infile until EOF *)
  WHILE NOT Infile.eof(infile) DO
    (* all decisions based on lookahead *)
    next := Infile.lookahead(infile);
    
    CASE next OF
    (* control chars *)
      NUL .. TAB :
        (* consume *)
        Infile.ReadChar(infile, next)
      
    (* newline *)
    | LF :        
        (* consume *)
        Infile.ReadChar(infile, next);
        
        (* add to total *)
        metrics.lines := metrics.line + 1;
        
        (* add to slocs if sloc *)
        IF isSloc THEN
          metrics.slocs := metrics.slocs + 1
        END; (* IF *)
        
        (* reset flag *)
        isSloc := FALSE
        
    (* control chars and whitespace *)
      VT .. SPACE :
        (* consume *)
        Infile.ReadChar(infile, next)
      
    (* quoted literal *)
    | DOUBLEQUOTE, SINGLEQUOTE :
        (* consume *)
        SkipQuotedLiteral(infile);
        isSloc := TRUE     
        
    (* m2 comment *)
    | '(' :
      (* consume *)
      Infile.ReadFile(infile, next);
      next := Infile.lookahead(infile, next);
      
      IF next = '*' THEN
      (* consume *)
        Infile.ReadFile(infile, next);
        next := Infile.lookahead(infile, next);
        
        (* if not PIM pragma nor m2pp directive, count it *)
        IF (next # '$') AND (next # '?') THEN
          metrics.comments := metrics.comments + 1
        ELSE (* count it as code *)
          isSloc := TRUE
        END; (* IF *)
        
        (* consume comment *)
        SkipM2Comment(infile, metrics.lines)
        
      ELSE (* sole parenthesis *)
        isSloc := TRUE
      END; (* IF *)
          
    (* m2pp comment *)
    | '/' :
      IF Infile.la2Char(infile) = '*' THEN
        (* consume m2pp comment *)
        SkipPPComment(infile, metrics.lines)
      
      ELSE (* sole slash *)
        (* consume *)
        Infile.ReadChar(infile, next);
        isSloc := TRUE
      END (* IF *)
      
    (* semicolon *)
      ';' :
        (* consume and count *)
        Infile.ReadChar(infile, next);
        metrics.semicolons := metrics.semicolons + 1;
        isSloc := TRUE
      
    (* control char *)
      DEL :
        (* consume *)
        Infile.ReadChar(infile, next)
      
    (* any other chars *)
    ELSE
      (* consume *)
      Infile.ReadChar(infile, next);
      isSloc := TRUE
    END (* CASE *)
  END (* WHILE *)  
END Measure;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* ---------------------------------------------------------------------------
 * private procedure SkipQuotedLiteral(infile)
 * ---------------------------------------------------------------------------
 * Reads a quoted literal from infile and consumes it.
 * ------------------------------------------------------------------------ *)

PROCEDURE SkipQuotedLiteral ( infile : InfileT );
  
VAR
  next, delimiter : CHAR;

BEGIN
  (* consume and write delimiter *)
  Infile.ReadChar(infile, delimiter);
  
  REPEAT
    (* consume and write *)
    Infile.ReadChar(infile, next)
  UNTIL (next = delimiter) OR (next = LF) OR (Infile.eof(infile))
END SkipQuotedLiteral;


(* ---------------------------------------------------------------------------
 * private procedure SkipM2Comment(infile, lineCounter)
 * ---------------------------------------------------------------------------
 * Reads a Modula-2 comment from infile and consumes it.  Increments
 * parameter lineCounter for every newline encountered within the comment.
 * ------------------------------------------------------------------------ *)

PROCEDURE SkipM2Comment ( infile : InfileT; VAR lineCounter : CARDINAL );

VAR
  next : CHAR;
  delimiterFound : BOOLEAN;

BEGIN (* opening delimiter "(*" has already been consumed *)
  delimiterFound := FALSE;
  
  (* consume chars until closing delimiter *)
  WHILE NOT delimiterFound DO
    (* get next char *)
    ReadChar(infile, next);
    
    (* check for newline *)
    IF next = LF THEN
      lineCounter := lineCounter + 1
    
    (* check for closing delimiter *)
    ELSIF (next = '*') AND (Infile.la2Char(infile) = ')') THEN
      delimiterFound := TRUE;
      
      (* consume closing delimiter *)
      ReadChar(infile, next);
      ReadChar(infile, next)
    END (* IF *)
  END (* WHILE *)
END SkipM2Comment;


(* ---------------------------------------------------------------------------
 * private procedure SkipPPComment(infile, lineCounter)
 * ---------------------------------------------------------------------------
 * Reads a Preprocessor comment from infile and consumes it.  Increments
 * parameter lineCounter for every newline encountered within the comment.
 * ------------------------------------------------------------------------ *)

PROCEDURE SkipPPComment ( infile : InfileT; VAR lineCounter : CARDINAL );

VAR
  next : CHAR;
  delimiterFound : BOOLEAN;

BEGIN (* opening delimiter "/*" has already been consumed *)
  delimiterFound := FALSE;
  
  (* consume chars until closing delimiter *)
  WHILE NOT delimiterFound DO
    (* get next char *)
    ReadChar(infile, next);
    
    (* check for newline *)
    IF next = LF THEN
      lineCounter := lineCounter + 1
    
    (* check for closing delimiter *)
    ELSIF (next = '*') AND (Infile.la2Char(infile) = '/') THEN
      delimiterFound := TRUE;
      
      (* consume closing delimiter *)
      ReadChar(infile, next);
      ReadChar(infile, next)
    END (* IF *)
  END (* WHILE *)
END SkipPPComment;


END M2Meter.