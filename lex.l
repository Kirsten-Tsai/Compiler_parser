%{
/*
 * scan.l
 *
 * lex input file for pascal scanner
 *
 * extensions: two ways to spell "external" and "->" ok for "^".
 */

 #include <stdio.h>
 #include <string.h>
/* 
#define   AND      1
#define   ARRAY      2
#define   CASE      3
#define   CONST      4
#define   DIV      5
#define   DO      6
#define   DOWNTO      7
#define   ELSE      8
#define   END      9

#define   EXTERNAL      10
#define   FOR      11
#define   FORWARD      12
#define   FUNCTION      13
#define   GOTO      14
#define   IF      15
#define   IN      16
#define   LABEL      17
#define   MOD      18
#define   NIL      19
#define   NOT      20
#define   OF       21
#define   OR      22
#define   OTHERWISE      23
#define   PACKED      24
#define   PBEGIN      25
#define   PFILE      26
#define   PROCEDURE      27
#define   PROGRAM      28
#define   RECORD      29
#define   REPEAT      30
#define   SET      31
#define   THEN      32
#define   TO      33
#define   TYPE      34
#define   UNTIL      35
#define   VAR      36
#define   WHILE      37
#define   WITH      38
#define   IDENTIFIER      39

#define   ASSIGNMENT      40
#define   CHARACTER_STRING      41
#define   COLON      42
#define   COMMA      43
#define   DIGSEQ      44
#define   DOT      45
#define   DOTDOT      46
#define   EQUAL      47
#define   GE      48
#define   GT      49
#define   LBRAC      50
#define   LE      51
#define   LPAREN      52
#define   LT      53
#define   MINUS      54
#define   NOTEQUAL      55
#define   PLUS      56
#define   RBRAC      57
#define   REALNUMBER      58
#define   RPAREN      59
#define   SEMICOLON      60
#define   SLASH      61
#define   STAR      62
#define   STARSTAR      63
#define   UPARROW      64


#define   notEQUAL    65

#define   OTHERREALNUMBER 66
#define   STRING  67
*/
#define LIST strcat( buf, yytext )
int line_no = 1;

#define  MAX 256
char buf[MAX];
char strContent[MAX];
int opt = 1; /* token option */
int judge = 0;/*means not minus*/
int judge_s = 0;

//paste Crtl+V
//and

  // judge == 1  -> means     minus
  // judge == 0  -> means not minus
  //find the judge and judge_s meaning and logic
  //then figure out what are the rules 00
  //compare two codes and another view
%}

A [aA]
B [bB]
C [cC]
D [dD]
E [eE]
F [fF]
G [gG]
H [hH]
I [iI]
J [jJ]
K [kK]
L [lL]
M [mM]
N [nN]
O [oO]
P [pP]
Q [qQ]
R [rR]
S [sS]
T [tT]
U [uU]
V [vV]
W [wW]
X [xX]
Y [yY]
Z [zZ]
NQUOTE [^']

DQuotation ["]
Quotation [']
bottomLine [_]

eol \n

space " "

num ([0-9])

ILLEGAL [\!#\$\%\?@\\\^`\{\}\[\]~]

legalstring ([a-zA-Z0-9]|{space}|{ILLEGAL}|","|"("|")"|":"|";"|"."|"["|"]"|"<"|">"|"="|"+"|"-"|"*"|"/")

%x STR
%x Comment

%%

and|AND {
   fprintf(stderr, "and...at line: %d.  %s\n", line_no,yytext);
   return(AND);
} 
array|ARRAY {
    fprintf(stderr, "array...at line: %d.  %s\n", line_no,yytext);
    return(ARRAY);
}  
case|CASE   {
   fprintf(stderr, "case...at line: %d.  %s\n", line_no,yytext);
   return(CASE);
} 
const|CONST   {
   fprintf(stderr, "const...at line: %d.  %s\n", line_no,yytext);
   return(CONST);
} 
div|DIV   {
   fprintf(stderr, "div...at line: %d.  %s\n", line_no,yytext);
   return(DIV);
} 
do|DO    {
   fprintf(stderr, "do...at line: %d.  %s\n", line_no,yytext);
   return(DO);
} 
downto|DOWNTO {
   fprintf(stderr, "downto...at line: %d.  %s\n", line_no,yytext);
   return(DOWNTO);
} 
else|ELSE   {
   fprintf(stderr, "else...at line: %d.  %s\n", line_no,yytext);
   return(ELSE);
} 
end|END   {
   fprintf(stderr, "end...at line: %d.  %s\n", line_no,yytext);
   return(END);
} 
for|FOR  {
   fprintf(stderr, "for...at line: %d.  %s\n", line_no,yytext);
    return(FOR);
} 
function|FUNCTION {
   fprintf(stderr, "function...at line: %d.  %s\n", line_no,yytext);
   return(FUNCTION);
} 
if|IF    {
   fprintf(stderr, "if...at line: %d.  %s\n", line_no,yytext);
   return(IF);
} 
mod|MOD  {
   fprintf(stderr, "mod...at line: %d.  %s\n", line_no,yytext);
   return(MOD);
} 
nil|NIL   {
   fprintf(stderr, "nil...at line: %d.  %s\n", line_no,yytext);
   return(NIL);
} 
not|NOT   {
   fprintf(stderr, "not...at line: %d.  %s\n", line_no,yytext);
   return(NOT);
} 
of|OF  {
   fprintf(stderr, "of...at line: %d.  %s\n", line_no,yytext);
   return(OF);
} 
or|OR    {
   fprintf(stderr, "or...at line: %d.  %s\n", line_no,yytext);
   return(OR);
} 
begin|BEGIN   {
   fprintf(stderr, "begin...at line: %d.  %s\n", line_no,yytext);
   return(PBEGIN);
} 
procedure|PROCEDURE {
   fprintf(stderr, "procedure...at line: %d.  %s\n", line_no,yytext);
   return(PROCEDURE);
} 
program|PROGRAM  {
   fprintf(stderr, "program...at line: %d.  %s\n", line_no,yytext);
   return(PROGRAM);
} 
record|RECORD  {
   fprintf(stderr, "record...at line: %d.  %s\n", line_no,yytext);
   return(RECORD);
} 
repeat|REPEAT  {
   fprintf(stderr, "repeat...at line: %d.  %s\n", line_no,yytext);
   return(REPEAT);
} 
then|THEN  {
   fprintf(stderr, "then...at line: %d.  %s\n", line_no,yytext);
   return(THEN);
} 
to|TO   {
   fprintf(stderr, "to...at line: %d.  %s\n", line_no,yytext);
    return(TO);
} 
type|TYPE  {
   fprintf(stderr, "type...at line: %d.  %s\n", line_no,yytext);
    return(TYPE);
} 
until|UNTIL   {
   fprintf(stderr, "until...at line: %d.  %s\n", line_no,yytext);
   return(UNTIL);
} 
var|VAR   {
   fprintf(stderr, "var...at line: %d.  %s\n", line_no,yytext);
  return(VAR);
} 
while|WHILE   {
   fprintf(stderr, "while...at line: %d.  %s\n", line_no,yytext);
   return(WHILE);
}

integer|INTEGER {
        fprintf(stderr, "integer...at line: %d.  %s\n", line_no,yytext);
        return(INTEGER);
	}
	
real|REAL {
        fprintf(stderr, "real...at line: %d.  %s\n", line_no,yytext);
        return(REAL);
	}
	
writeln|WRITELN {
        fprintf(stderr, "writeln...at line: %d.  %s\n", line_no,yytext);
        return(WRITELN);
	}




[A-Za-z][A-Za-z]* {
	
  judge = 1; // fowllowing: minus
	fprintf(stderr, "id...%d.%s.\n", line_no,yytext);
  return(ID);

  }


":="  {
	judge = 0; //following: - sign
   fprintf(stderr, "assignment...at line: %d.  %s\n", line_no,yytext);
   return(ASSIGNMENT);
}  
'({NQUOTE}|'')+'  {
   fprintf(stderr, "character string...at line: %d.  %s\n", line_no,yytext);
   return(CHARACTER_STRING);
} 
":"    {
   fprintf(stderr, "colon...at line: %d.  %s\n", line_no,yytext);
   return(COLON);
} 
","    {	
   
   fprintf(stderr, "comma...at line: %d.  %s\n", line_no,yytext);
   return(COMMA);
} 
[0-9]+  {
		judge = 1;   //following: minus
		fprintf(stderr, "num...at line: %d.  %s\n", line_no,yytext);
		return(NUM);
		
	
 } 
 [0-9]+"."[0-9]+   {
	judge = 1;   //following:minus
   fprintf(stderr, "digseq...at line: %d.  %s\n", line_no,yytext);
   return(DIGSEQ);
} 

"."    {
   fprintf(stderr, "dot...at line: %d.  %s\n", line_no,yytext);
   return(DOT);
} 
".."    {
   fprintf(stderr, "dotdot...at line: %d.  %s\n", line_no,yytext);
   return(DOTDOT);
} 
"="     {
   fprintf(stderr, "equal...at line: %d.  %s\n", line_no,yytext);
   return(EQUAL);
} 
">="    {
   fprintf(stderr, "ge...at line: %d.  %s\n", line_no,yytext);
   return(GE);
} 
">"   {
   fprintf(stderr, "gt...at line: %d.  %s\n", line_no,yytext);
   return(GT);
} 
"["    {
   fprintf(stderr, "lbrac...at line: %d.  %s\n", line_no,yytext);
   return(LBRAC);
} 
"<="   {
   fprintf(stderr, "le...at line: %d.  %s\n", line_no,yytext);
   return(LE);
} 
"("    {
  judge = 0; //following: sign
   fprintf(stderr, "lparen...at line: %d.  %s\n", line_no,yytext);
   return(LPAREN);
} 
"<"    {
   fprintf(stderr, "lt...at line: %d.  %s\n", line_no,yytext);
   return(LT);
} 
"-"   {
	if(judge == 1){    //minus
	fprintf(stderr, "minus...at line: %d.  %s\n", line_no,yytext);
    return(MINUS);
	}
	else{	             //sign
		fprintf(stderr, "sign...at line: %d.  %s\n", line_no,yytext);
		return(SIGN);
	}
		
		
} 

"<>" {
   fprintf(stderr, "notequal...at line: %d.  %s\n", line_no,yytext);
   return(NOTEQUAL);
} 
 "!="   {
   fprintf(stderr, "notequal...at line: %d.  %s\n", line_no,yytext);
   return(NOTEQUAL);
} 
"+"    {
   fprintf(stderr, "plus...at line: %d.  %s\n", line_no,yytext);
   return(PLUS);
} 
"]"    {	
   fprintf(stderr, "rbrac...at line: %d.  %s\n", line_no,yytext);
   return(RBRAC);
} 

[0-9]+"."[0-9]+[eE] {
   fprintf(stderr, "otherrealnumber...at line: %d.  %s\n", line_no,yytext);
    return(OTHERREALNUMBER);
} 

")"    {
	judge = 0; //following: sign
   fprintf(stderr, "rparen...at line: %d.  %s\n", line_no,yytext);
   return(RPAREN);
} 
";"   {
   fprintf(stderr, "semicolon...at line: %d.  %s\n", line_no,yytext);
    return(SEMICOLON);
} 
"*"    {
   fprintf(stderr, "star...at line: %d.  %s\n", line_no,yytext);
   return(STAR);
} 
"^"    {
   fprintf(stderr, "uparrow...at line: %d.  %s\n", line_no,yytext);
   return(UPARROW);
} 
"/"         {
   fprintf(stderr, "div...at line: %d.  %s\n", line_no,yytext);
   return(DIV);
}     

[0-9][0-9]* { 
    fprintf(stderr, "num...%d.%s.\n", line_no,yytext);
    return(NUM);
  } 
  

"//"			{ 
				  LIST;
				  BEGIN Comment;
				}

<Comment>[^\n]+		{
				  LIST;
				}

<Comment>"\n"		{
				  LIST;
				  if(opt)
				  	printf("This is C++comment on line %d: %s",  line_no,  buf);
				  line_no++;
				  buf[0] = '\0';
				  BEGIN INITIAL;
				}
 

"\""				{ 
				  
				  return(DU);
				}


				
"\'"				{ 		  
				  return(DU);
		}


				


"(*"   | 
"{"    { register int c;
     while ((c = input()))
     {
      if (c == '}')
       break;
      else if (c == '*')
      {
       if ((c = input()) == ')')
        break;
       else
        unput (c);
      }
      else if (c == '\n')
       line_no++;
      else if (c == 0)
        fprintf (stderr, "Unexpected EOF inside comment at line %d\n",line_no);
     }
    }

\n { 
	fprintf(stderr, "eol...%d\n", line_no);
    line_no++; 
	judge = 0; //following: sign
	judge_s = 0;
  }
  
[ \t\f]    ;

\r         ;

	
.    {
		fprintf(stderr,"Lexical analyzer error at line %d : %s\n",line_no,yytext);
		return(UNLEGAL);
		}

%%
/*
int main(int argc, char** argv) {
  int i, tok;
  yyin = fopen(argv[1], "r");
  for (i = 0; 1==1; i ++) {
    tok = yylex();
    fprintf(stderr, "%3d-th token(type:%d) on line %d : %s\n", i, tok,line_no, yytext);
	memset( buf, '\0', MAX );
  }
  return(0);
} */
/*
yywrap() {
  fclose(yyin);
  exit(1);
}*/

