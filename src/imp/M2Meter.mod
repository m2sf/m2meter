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


TYPE SymType = (Procedure, Other);


VAR
  rwElsif, rwElse, rwEnd, rwProc, rwUntil : StringT;


(* ---------------------------------------------------------------------------
 * procedure Measure(infile)
 * ---------------------------------------------------------------------------
 * Measures file infile and passes back metrics in out-parameter metrics.
 * ------------------------------------------------------------------------ *)

PROCEDURE Measure ( infile : InfileT; VAR metrics : Metric );

VAR
  next : CHAR;
  isSloc : BOOLEAN;
  lastSym : SymType;
  identOrRW : StringT;

BEGIN
  (* init local vars *)
  isSloc := FALSE;
  lastSym := Other;
  identOrRW := NIL;
  
  (* init counters *)
  metrics.lines := 0;
  metrics.slocs := 0;
  metrics.comments := 0;
  metrics.semicolons := 0;
  metrics.procedures := 0;
    
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
    | VT .. SPACE :
        (* consume *)
        Infile.ReadChar(infile, next)
      
    (* quoted literal *)
    | DOUBLEQUOTE, SINGLEQUOTE :
        (* consume *)
        SkipQuotedLiteral(infile);
        lastSym := Other;
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
          lastSym := Other;
          isSloc := TRUE
        END; (* IF *)
        
        (* consume comment *)
        SkipM2Comment(infile, metrics.lines)
        
      ELSE (* sole parenthesis *)
        lastSym := Other;
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
        lastSym := Other;
        isSloc := TRUE
      END (* IF *)
      
    (* semicolon *)
    | ';' :
        (* consume and count *)
        Infile.ReadChar(infile, next);
        metrics.semicolons := metrics.semicolons + 1;
        lastSym := Other;
        isSloc := TRUE
      
    (* reserved word or identifier *)
    | 'A' .. 'Z', 'a' .. 'z' :
        identOrRW := stdIdent(infile);
        
        (* PROCEDURE *)
        IF identOrRW = rwProc THEN
          lastSym := Procedure;
          isSloc := TRUE
        
        (* ELSIF | ELSE | END | UNTIL *)
        ELSIF
         (identOrRW = rwElsif) OR (identOrRW = rwElse) OR
         (identOrRW = rwEnd) OR (identOrRW = rwUntil) THEN
          
          (* count ELSIF, ELSE, END and UNTIL as semicolons *)
          metrics.semicolons := metrics.semicolons + 1;
          lastSym := Other;
          isSloc := TRUE
        
        (* any other *)
        ELSIF lastSym = Procedure THEN
          metrics.procedures := metrics.procedures + 1;
          lastSym := Other;
          isSloc := TRUE
        END (* IF *)
      
    (* vertical bar *)
    | '|' :
        (* consume and count as semicolon *)
        Infile.ReadChar(infile, next);
        metrics.semicolons := metrics.semicolons + 1;
        lastSym := Other;
        isSloc := TRUE
      
    (* control char *)
    | DEL :
        (* consume *)
        Infile.ReadChar(infile, next)
      
    (* any other chars *)
    ELSE
      (* consume *)
      Infile.ReadChar(infile, next);
      lastSym := Other;
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
 * procedure stdIdent(infile)
 * ---------------------------------------------------------------------------
 * Reads and consumes a standard identifier from infile and returns it.
 * ------------------------------------------------------------------------ *)

PROCEDURE stdIdent ( infile : InfileT ) : StringT;

VAR
  next : CHAR;

BEGIN
  (* mark identifier *)
  Infile.MarkChar(infile);
  
  REPEAT
    Infile.ReadChar(infile, next);
    next := Infile.lookahead(infile)
  UNTIL (* not alpha-numeric *)
    (next < '0') OR
    ((next > '9') AND (next < 'A')) OR
    ((next > 'Z') AND (next < 'a')) OR
    ((next > 'z');
    
  (* return identifier *)
  RETURN Infile.lexeme(infile)
END stdIdent;


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

BEGIN (* opening delimiter has already been consumed *)
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

BEGIN (* opening delimiter has already been consumed *)
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


(* ---------------------------------------------------------------------------
 * private procedure InitReswords
 * ---------------------------------------------------------------------------
 * Initialises string variables rwElsif, rwElse, rwEnd, rwProc and rwUntil.
 * ------------------------------------------------------------------------ *)

PROCEDURE InitReswords;

BEGIN
  rwElsif := String.forArray("ELSIF");
  rwElse  := String.forArray("ELSE");
  rwEnd   := String.forArray("END");
  rwProc  := String.forArray("PROCEDURE");
  rwUntil := String.forArray("UNTIL")
END InitReswords;


BEGIN
  InitReswords
END M2Meter.