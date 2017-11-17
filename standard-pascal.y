%{
/*
 * grammar.y
 *
 * Pascal grammar in Yacc format, based originally on BNF given
 * in "Standard Pascal -- User Reference Manual", by Doug Cooper.
 * This in turn is the BNF given by the ANSI and ISO Pascal standards,
 * and so, is PUBLIC DOMAIN. The grammar is for ISO Level 0 Pascal.
 * The grammar has been massaged somewhat to make it LALR, and added
 * the following extensions.
 *
 * constant expressions
 * otherwise statement in a case
 * productions to correctly match else's with if's
 * beginnings of a separate compilation facility
 */
 
 
 #include <stdio.h>

     /* Called by yyparse on error.  */
     void
     yyerror (char const *s)
     {
        extern char *yytext;
        extern int line_no;
        fprintf (stderr, "%s: at line %d symbol'%s'\n", s,line_no,yytext);
     }
 

%}

%token AND ARRAY ASSIGNMENT CASE CHARACTER_STRING COLON COMMA CONST DIGSEQ
%token DIV DO DOT DOTDOT DOWNTO ELSE END EQUAL EXTERNAL FOR FORWARD FUNCTION
%token GE GOTO GT ID ID IF INTEGER IN LABEL LBRAC LE LPAREN LT MINUS MOD NIL NOT NUM
%token NOTEQUAL OF OR OTHERWISE PACKED PBEGIN PFILE PLUS PROCEDURE PROGRAM RBRAC
%token REAL REALNUMBER OTHERREALNUMBER RECORD REPEAT RPAREN SEMICOLON SET SLASH STAR STARSTAR THEN
%token TO TYPE UNTIL UPARROW VAR WHILE WITH SIGN STRING WRITELN DU UNLEGAL LEGAL

%%
file : program |
	   { fprintf(stderr, "Reduction: file -> program\n");}	
 ;

// prog    ::= PROGRAM id ( identifier_list ) ;
//   	    declarations
// 	    subprogram_declarations
// 	    compound_statement
// 	    .

program : PROGRAM ID LPAREN identifier_list RPAREN SEMICOLON 
		  declarations subprogram_declarations compound_statement
		  DOT { fprintf(stderr, "PROGRAM id ( identifier_list ) ; declarations subprogram_declarations compound_statement .\n");}
 ;
  

 
 // identifier_list ::= id
	// | identifier_list , id


identifier_list : identifier_list COMMA ID
				 { fprintf(stderr, "Reduction: identifier_list -> identifier_list , id\n");}
				| ID
				{ fprintf(stderr, "Reduction: identifier_list -> id\n");}
 ;


 // declarations ::= declarations VAR identifier_list : type ;
	// | lambda
 
 
declarations :  declarations VAR identifier_list COLON type SEMICOLON
				 { fprintf(stderr, "Reduction: declarations -> declarations VAR identifier_list : type ;\n");}
			  |
			   { fprintf(stderr, "Reduction: declarations -> λ\n");}
 ;
 
// type ::= standard_type
// 	| ARRAY [ num .. num ] OF type


 type : standard_type
		 { fprintf(stderr, "Reduction: type -> standard_type\n");}
		|  ARRAY LBRAC NUM DOTDOT NUM RBRAC OF type
		 { fprintf(stderr, "Reduction: type -> ARRAY [ num .. num ] OF type\n");}
 ;
 
// standard_type ::= INTEGER
// 	| REAL
//         | STRING



standard_type : INTEGER
      { fprintf(stderr, "Reduction: standard_type -> INTEGER\n");}
	| REAL 
      { fprintf(stderr, "Reduction: standard_type -> REAL\n");}
	| CHARACTER_STRING	  
	  { fprintf(stderr, "Reduction: standard_type -> STRING\n");};
	  

//constant
/*
 constant : INTEGER
			 { fprintf(stderr, "Use rule 11.\n");}
			| REAL
			 { fprintf(stderr, "Use rule 12.\n");}
			| CHARACTER_STRING
			 { fprintf(stderr, "Use rule 13.\n");}
;
*/ 


// subprogram_declarations ::=
// 	subprogram_declarations subprogram_declaration ;
// 	| lambda

subprogram_declarations : subprogram_declarations subprogram_declaration SEMICOLON
						{ fprintf(stderr, "Reduction: subprogram_declarations -> subprogram_declarations subprogram_declaration\n");}
						|
						{ fprintf(stderr, "Reduction: subprogram_declarations -> λ\n");}

; 


// subprogram_declaration ::=
// 	subprogram_head 
// 	declarations 			
//         compound_statement 
 
subprogram_declaration : subprogram_head declarations compound_statement
						{ fprintf(stderr, "Reduction: subprogram_declaration -> subprogram_head declarations compound_statement \n");}
 ;
 

// subprogram_head ::= FUNCTION id arguments : standard_type ;
// 	| PROCEDURE id arguments ;
 
subprogram_head : FUNCTION ID arguments COLON standard_type SEMICOLON
				{ fprintf(stderr, "Reduction: subprogram_head -> FUNCTION id arguments : standard_type ;\n");}
				| PROCEDURE ID arguments SEMICOLON
				{ fprintf(stderr, "Reduction: subprogram_head -> PROCEDURE id arguments ;\n");}
;
 
// arguments ::= ( parameter_list )
// 	| lambda 
arguments : LPAREN parameter_list RPAREN
			{ fprintf(stderr, "Reduction: arguments -> ( parameter_list )\n");}
			|
			{ fprintf(stderr, "Reduction: arguments -> λ\n");}
;

// parameter_list ::= optional_var identifier_list : type
// 	| optional_var identifier_list : type ; parameter_list
parameter_list : optional_var identifier_list COLON type			
				{ fprintf(stderr, "Reduction: parameter_list -> optional_var identifier_list : type\n");}
				| optional_var identifier_list COLON type SEMICOLON parameter_list
				{ fprintf(stderr, "Reduction: parameter_list -> optional_var identifier_list : type ; parameter_list\n");}
; 

// optional_var   ::= VAR
//         | lambda

optional_var :
				VAR{fprintf(stderr, "Reduction: optional_var -> VAR\n");}
			|		{fprintf(stderr, "Reduction: optional_var -> λ\n");}
;

// compound_statement ::= begin
// 		       optional_statements
// 		       end
compound_statement : PBEGIN optional_statements END			
				    { fprintf(stderr, "Reduction: compound_statement -> begin optional_statements end\n");}
;

// optional_statements ::= statement_list
// 	| lambda
optional_statements : statement_list
					{ fprintf(stderr, "Reduction: optional_statements -> string_list\n");}
					// |
					// { fprintf(stderr, "Use rule 33.\n");}
; 
 
// statement_list ::= statement
// 	| statement_list ; statement 
statement_list : statement
				{ fprintf(stderr, "Reduction: string_list -> statement\n");}
				| statement_list SEMICOLON statement
				{ fprintf(stderr, "Reduction: string_list -> statement_list ; statement\n");}
;


// statement ::= variable := expression
// 	| procedure_statement
// 	| compound_statement
// 	| IF expression THEN statement ELSE statement
// 	| WHILE expression DO statement
// 	| lambda
 
statement : variable ASSIGNMENT expression 
			{ fprintf(stderr, "Reduction: statement -> variable := expression\n");}
			| procedure_statement
			{ fprintf(stderr, "Reduction: statement -> procedure_statement\n");}
			| compound_statement
			{ fprintf(stderr, "Reduction: statement -> compound_statement\n");}
			| IF expression THEN statement ELSE statement
			{ fprintf(stderr, "Reduction: statement -> IF expression THEN statement ELSE statement\n");}
			| WHILE expression DO statement
			{ fprintf(stderr, "Reduction: statement -> WHILE expression DO statement\n");}
			|
			{ fprintf(stderr, "Reduction: statement -> λ\n");}
			// | WRITELN LPAREN CHARACTER_STRING RPAREN SEMICOLON
			// { fprintf(stderr, "Use rule 79._PRINT STRING\n");}


			// | variable ASSIGNMENT string_list
			// { fprintf(stderr, "Use rule 00.\n");}
			// | WRITELN LPAREN string_list RPAREN  
			// { fprintf(stderr, "Use rule 00.\n");}
			
;

 
// string_list : term_list
// 		{ fprintf(stderr, "Use rule 00.\n");}
// ;	


// term_list :
// 		LEGAL term_list
// 		{ fprintf(stderr, "Use rule 00.\n");}
// 		|UNLEGAL term_list
// 		{ fprintf(stderr, "Use rule 00.\n");}
// 		|
// 		{ fprintf(stderr, "Use rule 00.\n");}
// ;


// variable ::= id tail
variable : ID tail
			{ fprintf(stderr, "Reduction: variable -> id tail\n");}
;

// tail     ::= [ expression ] tail
// 	| lambda 
tail : LBRAC expression RBRAC tail
	   { fprintf(stderr, "Reduction: tail -> [ expression ] tail\n");}
	  |
	  { fprintf(stderr, "Reduction: tail -> λ\n");}
; 

// procedure_statement ::= id
// 	| id ( expression_list )
procedure_statement : ID
					  { fprintf(stderr, "Reduction: procedure_statement -> id\n");}
					| ID LPAREN expression_list RPAREN
					{ fprintf(stderr, "Reduction: procedure_statement -> id ( expression_list )\n");}
					| WRITELN LPAREN STRING_INPUT RPAREN
					{ fprintf(stderr, "Reduction: procedure_statement -> writeln ( string )\n");}

; 

//added
STRING_INPUT : CHARACTER_STRING
				{fprintf(stderr, "Reduction: string -> character string\n" );}
;


// expression_list ::= expression
// 	| expression_list , expression
expression_list : expression
				  { fprintf(stderr, "Reduction: expression_list -> expression\n");}
				| expression_list COMMA expression
				{ fprintf(stderr, "Reduction: expression_list -> expression_list , expression\n");}
;

// expression ::= simple_expression
// 	| simple_expression relop simple_expression
expression : simple_expression
			{ fprintf(stderr, "Reduction: expression -> simple_expression\n");}
			| simple_expression relop simple_expression
			{ fprintf(stderr, "Reduction: expression -> simple_expression relop simple_expression\n");}
;

// simple_expression ::= term
// 	| simple_expression addop term
simple_expression : term
					{ fprintf(stderr, "Reduction: simple_expression -> term\n");}
					| simple_expression addop term
					{ fprintf(stderr, "Reduction: simple_expression -> simple_expression addop term\n");}
;
		
// term ::= factor
// 	| term mulop factor
term : factor
	   { fprintf(stderr, "Reduction: term -> factor\n");}
	   | term mulop factor
	   { fprintf(stderr, "Reduction: term ->　term mulop factor\n");}
;

// factor ::= id tail
// 	| id ( expression_list )
// 	| num
// 	| ( expression )
// 	| not factor
factor : ID tail
		{ fprintf(stderr, "Reduction: factor -> id tail\n");}
		| ID LPAREN expression_list RPAREN
		{ fprintf(stderr, "Reduction: factor -> id ( expression_list )\n");}
		| NUM
		{ fprintf(stderr, "Reduction: factor -> num\n");}
		| DIGSEQ
		{ fprintf(stderr, "Reduction: factor -> realnumber\n");}
		| SIGN DIGSEQ
		{ fprintf(stderr, "Reduction: factor -> negative realnumber\n");}
		| SIGN NUM
		{ fprintf(stderr, "Reduction: factor -> negative num\n");} //new rule added.!!
		| LPAREN expression RPAREN
		{ fprintf(stderr, "Reduction: factor -> ( expression )\n");}
		| NOT factor
		{ fprintf(stderr, "Reduction: factor -> not factor\n");}
		| CHARACTER_STRING tail
		{ fprintf(stderr, "Reduction: factor -> string tail\n");}
;

//DELETE
// digseq : DIGSEQ
// 		 { fprintf(stderr, "Use rule 11.\n");}
// 		 |SIGN DIGSEQ
// 		  { fprintf(stderr, "Use rule 11.\n");}
// ;
		 
//addop ::= + | -
addop: PLUS
	   { fprintf(stderr, "Reduction: addop -> +\n");}
	  | MINUS
	  { fprintf(stderr, "Reduction: addop -> -\n");}
 ;

//mulop ::= * | /
mulop : STAR
	   { fprintf(stderr, "Reduction: mulop -> *\n");}
	   | DIV
	   { fprintf(stderr, "Reduction: mulop -> /\n");}
 ;

// relop ::= <
// 	| >
// 	| =
// 	| <=
// 	| >=
// 	| !=
relop : LT		
		{ fprintf(stderr, "Reduction: relop -> <\n");}
		| GT
		{ fprintf(stderr, "Reduction: relop -> >\n");}
		| EQUAL
		{ fprintf(stderr, "Reduction: relop -> =\n");}
		| LE
		{ fprintf(stderr, "Reduction: relop -> <=\n");}
		| GE
		{ fprintf(stderr, "Reduction: relop -> >=\n");}
		| NOTEQUAL
		{ fprintf(stderr, "Reduction: relop -> !=\n");}
 ;


 
 %%
 
 int main(int argc, char** argv) {
    int res;
    
    fprintf(stderr, "open the file.\n");
    if (argc>1 && freopen(argv[1],"r",stdin) ==NULL){
        exit(1);
    }
    
    fprintf(stderr, "call yyparse\n");

    res = yyparse();
    fprintf(stderr, "after call yyparse, res = %d.\n", res);
    
    if (res==0)
        fprintf(stderr, "SUCCESS\n");
    else
        fprintf(stderr, "ERROR\n");
}

 #include "lex.yy.c"
 