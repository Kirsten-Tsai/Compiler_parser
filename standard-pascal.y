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
file : program
	   { fprintf(stderr, "Use rule 1.\n");}	
 ;

// prog    ::= PROGRAM id ( identifier_list ) ;
//   	    declarations
// 	    subprogram_declarations
// 	    compound_statement
// 	    .

program : PROGRAM ID LPAREN identifier_list RPAREN SEMICOLON 
		  declarations subprogram_declarations compound_statement
		  DOT { fprintf(stderr, "Use rule 2.\n");}
 ;
  

 
 // identifier_list ::= id
	// | identifier_list , id


identifier_list : identifier_list COMMA ID
				 { fprintf(stderr, "Use rule 4.\n");}
				| ID
				{ fprintf(stderr, "Use rule 5.\n");}
 ;


 // declarations ::= declarations VAR identifier_list : type ;
	// | lambda
 
 
declarations :  declarations VAR identifier_list COLON type SEMICOLON
				 { fprintf(stderr, "Use rule 7.\n");}
			  |
			   { fprintf(stderr, "Use rule 8.\n");}
 ;
 
// type ::= standard_type
// 	| ARRAY [ num .. num ] OF type


 type : standard_type
		 { fprintf(stderr, "Use rule 9.\n");}
		|  ARRAY LBRAC NUM DOTDOT NUM RBRAC OF type
		 { fprintf(stderr, "Use rule 10.\n");}
 ;
 
// standard_type ::= INTEGER
// 	| REAL
//         | STRING



standard_type : INTEGER
      { fprintf(stderr, "Use rule 11.\n");}
	| REAL 
      { fprintf(stderr, "Use rule 12.\n");}
	| CHARACTER_STRING	  
	  { fprintf(stderr, "Use rule 13.\n");};
	  

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
						{ fprintf(stderr, "Use rule 20.\n");}
						|
						{ fprintf(stderr, "Use rule 21.\n");}

; 


// subprogram_declaration ::=
// 	subprogram_head 
// 	declarations 			
//         compound_statement 
 
subprogram_declaration : subprogram_head declarations compound_statement
						{ fprintf(stderr, "Use rule 22.\n");}
 ;
 

// subprogram_head ::= FUNCTION id arguments : standard_type ;
// 	| PROCEDURE id arguments ;
 
subprogram_head : FUNCTION ID arguments COLON standard_type SEMICOLON
				{ fprintf(stderr, "Use rule 23.\n");}
				| PROCEDURE ID arguments SEMICOLON
				{ fprintf(stderr, "Use rule 24.\n");}
;
 
// arguments ::= ( parameter_list )
// 	| lambda 
arguments : LPAREN parameter_list RPAREN
			{ fprintf(stderr, "Use rule 25.\n");}
			|
			{ fprintf(stderr, "Use rule 26.\n");}
;

// parameter_list ::= optional_var identifier_list : type
// 	| optional_var identifier_list : type ; parameter_list
parameter_list : optional_var identifier_list COLON type			
				{ fprintf(stderr, "Use rule 27.\n");}
				| optional_var identifier_list COLON type SEMICOLON parameter_list
				{ fprintf(stderr, "Use rule 28.\n");}
; 

// optional_var   ::= VAR
//         | lambda

optional_var :
				VAR{fprintf(stderr, "Use rule 29.\n");}
			|		{fprintf(stderr, "Use rule 30\n");}
;

// compound_statement ::= begin
// 		       optional_statements
// 		       end
compound_statement : PBEGIN optional_statements END			
				    { fprintf(stderr, "Use rule 31.\n");}
;

// optional_statements ::= statement_list
// 	| lambda
optional_statements : statement_list
					{ fprintf(stderr, "Use rule 32.\n");}
					// |
					// { fprintf(stderr, "Use rule 33.\n");}
; 
 
// statement_list ::= statement
// 	| statement_list ; statement 
statement_list : statement
				{ fprintf(stderr, "Use rule 34.\n");}
				| statement_list SEMICOLON statement
				{ fprintf(stderr, "Use rule 35.\n");}
;


// statement ::= variable := expression
// 	| procedure_statement
// 	| compound_statement
// 	| IF expression THEN statement ELSE statement
// 	| WHILE expression DO statement
// 	| lambda
 
statement : variable ASSIGNMENT expression 
			{ fprintf(stderr, "Use rule 36.\n");}
			| procedure_statement
			{ fprintf(stderr, "Use rule 37.\n");}
			| compound_statement
			{ fprintf(stderr, "Use rule 38.\n");}
			| IF expression THEN statement ELSE statement
			{ fprintf(stderr, "Use rule 39.\n");}
			| WHILE expression DO statement
			{ fprintf(stderr, "Use rule 40.\n");}
			|
			{ fprintf(stderr, "Use rule 41.\n");}
			
;



// variable ::= id tail
variable : ID tail
			{ fprintf(stderr, "Use rule 42.\n");}
;

// tail     ::= [ expression ] tail
// 	| lambda 
tail : LBRAC expression RBRAC tail
	   { fprintf(stderr, "Use rule 43.\n");}
	  |
	  { fprintf(stderr, "Use rule 44.\n");}
; 

// procedure_statement ::= id
// 	| id ( expression_list )
procedure_statement : ID
					  { fprintf(stderr, "Use rule 45.\n");}
					| ID LPAREN expression_list RPAREN
					{ fprintf(stderr, "Use rule 46.\n");}
; 

// expression_list ::= expression
// 	| expression_list , expression
expression_list : expression
				  { fprintf(stderr, "Use rule 47.\n");}
				| expression_list COMMA expression
				{ fprintf(stderr, "Use rule 48.\n");}
;

// expression ::= simple_expression
// 	| simple_expression relop simple_expression
expression : simple_expression
			{ fprintf(stderr, "Use rule 49.\n");}
			| simple_expression relop simple_expression
			{ fprintf(stderr, "Use rule 50.\n");}
;

// simple_expression ::= term
// 	| simple_expression addop term
simple_expression : term
					{ fprintf(stderr, "Use rule 51.\n");}
					| simple_expression addop term
					{ fprintf(stderr, "Use rule 52.\n");}
;
		
// term ::= factor
// 	| term mulop factor
term : factor
	   { fprintf(stderr, "Use rule 53.\n");}
	   | term mulop factor
	   { fprintf(stderr, "Use rule 54.\n");}
;

// factor ::= id tail
// 	| id ( expression_list )
// 	| num
// 	| ( expression )
// 	| not factor
factor : ID tail
		{ fprintf(stderr, "Use rule 55.\n");}
		| ID LPAREN expression_list RPAREN
		{ fprintf(stderr, "Use rule 56.\n");}
		| NUM
		{ fprintf(stderr, "Use rule 57.\n");}
		| LPAREN expression RPAREN
		{ fprintf(stderr, "Use rule 58.\n");}
		| NOT factor
		{ fprintf(stderr, "Use rule 59.\n");}
;
		 
//addop ::= + | -
addop: PLUS
	   { fprintf(stderr, "Use rule 60.\n");}
	  | MINUS
	  { fprintf(stderr, "Use rule 61.\n");}
 ;

//mulop ::= * | /
mulop : STAR
	   { fprintf(stderr, "Use rule 62.\n");}
	   | DIV
	   { fprintf(stderr, "Use rule 63.\n");}
 ;

// relop ::= <
// 	| >
// 	| =
// 	| <=
// 	| >=
// 	| !=
relop : LT		
		{ fprintf(stderr, "Use rule 64.\n");}
		| GT
		{ fprintf(stderr, "Use rule 65.\n");}
		| EQUAL
		{ fprintf(stderr, "Use rule 66.\n");}
		| LE
		{ fprintf(stderr, "Use rule 67.\n");}
		| GE
		{ fprintf(stderr, "Use rule 68.\n");}
		| NOTEQUAL
		{ fprintf(stderr, "Use rule 69.\n");}
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
 