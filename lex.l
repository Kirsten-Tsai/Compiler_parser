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
   fprintf(stderr, "Shift: %s\n",yytext);
   return(AND);
} 
array|ARRAY {
    fprintf(stderr, "Shift: %s\n",yytext);
    return(ARRAY);
}  
case|CASE   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(CASE);
} 
const|CONST   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(CONST);
} 
div|DIV   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(DIV);
} 
do|DO    {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(DO);
} 
downto|DOWNTO {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(DOWNTO);
} 
else|ELSE   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(ELSE);
} 
end|END   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(END);
} 
for|FOR  {
   fprintf(stderr, "Shift: %s\n",yytext);
    return(FOR);
} 
function|FUNCTION {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(FUNCTION);
} 
if|IF    {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(IF);
} 
mod|MOD  {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(MOD);
} 
nil|NIL   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(NIL);
} 
not|NOT   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(NOT);
} 
of|OF  {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(OF);
} 
or|OR    {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(OR);
} 
begin|BEGIN   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(PBEGIN);
} 
procedure|PROCEDURE {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(PROCEDURE);
} 
program|PROGRAM  {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(PROGRAM);
} 
record|RECORD  {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(RECORD);
} 
repeat|REPEAT  {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(REPEAT);
} 
then|THEN  {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(THEN);
} 
to|TO   {
   fprintf(stderr, "Shift: %s\n",yytext);
    return(TO);
} 
type|TYPE  {
   fprintf(stderr, "Shift: %s\n",yytext);
    return(TYPE);
} 
until|UNTIL   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(UNTIL);
} 
var|VAR   {
   fprintf(stderr, "Shift: %s\n",yytext);
  return(VAR);
} 
while|WHILE   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(WHILE);
}

integer|INTEGER {
        fprintf(stderr, "Shift: %s\n",yytext);
        return(INTEGER);
  }
  
real|REAL {
        fprintf(stderr, "Shift: %s\n",yytext);
        return(REAL);
  }
  
writeln|WRITELN {
        fprintf(stderr, "Shift: %s\n",yytext);
        return(WRITELN);
  }




[A-Za-z][A-Za-z]* {
  
  judge = 1; // fowllowing: minus
  fprintf(stderr, "Shift: %s.\n",yytext);
  return(ID);

  }


":="  {
  judge = 0; //following: - sign
   fprintf(stderr, "Shift: %s\n",yytext);
   return(ASSIGNMENT);
}  
'({NQUOTE}|'')+'  {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(CHARACTER_STRING);
} 
":"    {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(COLON);
} 
","    {  
   
   fprintf(stderr, "Shift: %s\n",yytext);
   return(COMMA);
} 
[0-9]+  {
    judge = 1;   //following: minus
    fprintf(stderr, "Shift: %s\n",yytext);
    return(NUM);
    
  
 } 
 [0-9]+"."[0-9]+   {
  judge = 1;   //following:minus
   fprintf(stderr, "Shift: %s\n",yytext);
   return(DIGSEQ);
} 

"."    {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(DOT);
} 
".."    {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(DOTDOT);
} 
"="     {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(EQUAL);
} 
">="    {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(GE);
} 
">"   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(GT);
} 
"["    {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(LBRAC);
} 
"<="   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(LE);
} 
"("    {
  judge = 0; //following: sign
   fprintf(stderr, "Shift: %s\n",yytext);
   return(LPAREN);
} 
"<"    {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(LT);
} 
"-"   {
  if(judge == 1){    //minus
  fprintf(stderr, "Shift: %s\n",yytext);
    return(MINUS);
  }
  else{              //sign
    fprintf(stderr, "Shift: %s\n",yytext);
    return(SIGN);
  }
    
    
} 

"<>" {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(NOTEQUAL);
} 
 "!="   {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(NOTEQUAL);
} 
"+"    {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(PLUS);
} 
"]"    {  
   fprintf(stderr, "Shift: %s\n",yytext);
   return(RBRAC);
} 

[0-9]+"."[0-9]+[eE] {
   fprintf(stderr, "Shift: %s\n",yytext);
    return(OTHERREALNUMBER);
} 

")"    {
  judge = 0; //following: sign
   fprintf(stderr, "Shift: %s\n",yytext);
   return(RPAREN);
} 
";"   {
   fprintf(stderr, "Shift: %s\n",yytext);
    return(SEMICOLON);
} 
"*"    {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(STAR);
} 
"^"    {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(UPARROW);
} 
"/"         {
   fprintf(stderr, "Shift: %s\n",yytext);
   return(DIV);
}     

[0-9][0-9]* { 
    fprintf(stderr, "Shift: %s.\n",yytext);
    return(NUM);
  } 
  

"//"      { 
          LIST;
          BEGIN Comment;
        }

<Comment>[^\n]+   {
          LIST;
        }

<Comment>"\n"   {
          LIST;
          if(opt)
            printf("Shift comment: %s",,  buf);
        ++;
          buf[0] = '\0';
          BEGIN INITIAL;
        }
 

"\""        { 
          //LIST; memset( buf, '\0', MAX );
          
          return(DU);
        
          //   if(judge_s == 1){
          // judge_s = 0;
          // return(DU);
          // }
         // else {judge_s = 1;
         // return(DU);
         // }
          
         // BEGIN STR; 
        }


        
"\'"        { 
          
          return(DU);
         //  if(judge_s == 1){
          // judge_s = 0;
          // return(DU);
          // }
         // else {judge_s = 1;
         // return(DU);
         // }
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

