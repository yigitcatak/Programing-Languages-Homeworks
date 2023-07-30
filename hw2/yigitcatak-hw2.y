%{
#include <stdio.h>
void yyerror (const char *msg) { return; } 
%}

%token tSTRING tNUM tIDENT
%token tEQUALITY tGT tLT tGEQ tLEQ tADD tSUB tMUL tDIV tINC tDEC
%token tPRINT tGET tSET tFUNCTION tIF tRETURN

%%
program:        '[' statements ']';

statements:     /* EMPTY TERMINAL */
                | statements statement;
        
statement:      set
                | if
                | print
                | alter
                | return
                | expression;

expression:     tNUM
                | tSTRING
                | get
                | function
                | operation
                | condition;

condoperator:   tLEQ
                | tGEQ
                | tLT
                | tGT
                | tEQUALITY;

operator:       tADD
                | tSUB
                | tDIV
                | tMUL;

/* SEPARATE EMPTY TERMINAL FROM LISTS SO THIS IS NOT ALLOWED IN GRAMMAR 
-> " _EMPTY_ , EXPRESSION/IDENT ... " */

explist:        /* EMPTY TERMINAL */
                | cs-expressions;

paramlist:      /* EMPTY TERMINAL */
                | cs-parameter;

cs-expressions: expression
                | cs-expressions ',' expression;

cs-parameter:   tIDENT
                | cs-parameter ',' tIDENT;

function:       '[' tFUNCTION ',' '[' paramlist ']' ',' '[' statements ']' ']'

get:            '[' tGET ',' tIDENT ']'
                | '[' tGET ',' tIDENT ',' '[' explist ']' ']'

set:            '[' tSET ',' tIDENT ',' expression ']';

if:             '[' tIF ',' condition ',' '[' statements ']' else ']';

else:           /* EMPTY TERMINAL */
                | '[' statements ']';

alter:          '[' tINC ',' tIDENT ']'
                | '[' tDEC ',' tIDENT ']';

condition:      '[' condoperator ',' expression ',' expression ']';

operation:      '[' operator ',' expression ',' expression ']';

print:          '[' tPRINT ',' expression ']';

return:         '[' tRETURN ']'
                | '[' tRETURN ',' expression ']';
%%

int main(){
    if (yyparse())
    {
    // parse error
        printf("ERROR\n");
        return 1;
    }
    else
    {
    // successful parsing
        printf("OK\n");
        return 0;
    }
}