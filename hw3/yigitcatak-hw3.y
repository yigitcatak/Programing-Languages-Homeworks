%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "yigitcatak-hw3.h"

enum operation {opAdd, opSub, opMul, opDiv};

typedef struct LogNode {
	char * message;
	DataNode * owner;
} LogNode;

unsigned int log_size = 100, log_index = 0;
LogNode ** Log;

DataNode * CopyData(DataNode);
DataNode * CopyDataPtr(DataNode *);
DataNode * Operation(DataNode *, DataNode *, enum operation);
char * NumToStr(double, char);
void LogIt(char *, DataNode *);

void yyerror (const char *s) {}
%}

%union{
    DataNode dataNode;
	DataNode * dataNodePtr;
}

%token tADD tSUB tMUL tDIV tPRINT tGET tSET tFUNCTION tRETURN tIDENT tEQUALITY tIF tGT tLT tGEQ tLEQ tINC tDEC
%token <dataNode> tNUM 
%token <dataNode> tSTRING

%type <dataNodePtr> expr
%type <dataNodePtr> operation
%start prog

%%

operation:	'[' tADD ',' expr ',' expr ']' {
					$$ = NULL;
					if (($4 != NULL) && ($6 != NULL)) {
						$$ = Operation($4, $6, opAdd);
					}
				}
			| '[' tSUB ',' expr ',' expr ']' {
					$$ = NULL;
					if (($4 != NULL) && ($6 != NULL)) {
						$$ = Operation($4, $6, opSub);
					}
				}
			| '[' tMUL ',' expr ',' expr ']' {
					$$ = NULL;
					if (($4 != NULL) && ($6 != NULL)) {
						$$ = Operation($4, $6, opMul);
					}
				}
			| '[' tDIV ',' expr ',' expr ']' {
					$$ = NULL;
					if (($4 != NULL) && ($6 != NULL)) {
						$$ = Operation($4, $6, opDiv);
					}
				}
;

expr:		tNUM { $$ = CopyData($1); }
			| tSTRING { $$ = CopyData($1); } 
			| operation { $$ = CopyDataPtr($1); }
			| getExpr { $$ = NULL; } | function { $$ = NULL; } | condition { $$ = NULL; }
;

prog:		'[' stmtlst ']'
;

stmtlst:	stmtlst stmt |
;

stmt:		setStmt | if | print | unaryOperation | expr | returnStmt
;

getExpr:	'[' tGET ',' tIDENT ',' '[' exprList ']' ']'
			| '[' tGET ',' tIDENT ',' '[' ']' ']'
			| '[' tGET ',' tIDENT ']'
;

setStmt:	'[' tSET ',' tIDENT ',' expr ']'
;

if:		'[' tIF ',' condition ',' '[' stmtlst ']' ']'
		| '[' tIF ',' condition ',' '[' stmtlst ']' '[' stmtlst ']' ']'
;

print:		'[' tPRINT ',' expr ']'
;

unaryOperation: '[' tINC ',' tIDENT ']'
				| '[' tDEC ',' tIDENT ']'
;

function:	 '[' tFUNCTION ',' '[' parametersList ']' ',' '[' stmtlst ']' ']'
		| '[' tFUNCTION ',' '[' ']' ',' '[' stmtlst ']' ']'
;

condition:	'[' tEQUALITY ',' expr ',' expr ']'
			| '[' tGT ',' expr ',' expr ']'
			| '[' tLT ',' expr ',' expr ']'
			| '[' tGEQ ',' expr ',' expr ']'
			| '[' tLEQ ',' expr ',' expr ']'
;

returnStmt:	'[' tRETURN ',' expr ']'
			| '[' tRETURN ']'
;

parametersList: parametersList ',' tIDENT | tIDENT
;

exprList:	exprList ',' expr | expr
;

%%
DataNode * CopyData(DataNode data){ // this is a fresh expression node from token
    DataNode * newNode = (DataNode *)malloc(sizeof(DataNode));
    newNode->value = data.value;
	newNode->dtype = data.dtype;
	newNode->lineNum = data.lineNum;
	newNode->parent2 = newNode->parent1 = NULL;
    return newNode;
}

DataNode * CopyDataPtr(DataNode * ptr){ // this is used when an expression or operation is coppied to expression
	if (!ptr) return NULL;
    DataNode * newNode = (DataNode *)malloc(sizeof(DataNode));
    newNode->value = ptr->value;
	newNode->dtype = ptr->dtype;
	newNode->lineNum = ptr->lineNum;
	newNode->parent1 = ptr->parent1;
	newNode->parent2 = ptr->parent2;
    return newNode;
}

char * NumToStr(double num, char dtype) {
	unsigned int length = (dtype == dint) ? snprintf(NULL, 0, "%.0f", num) : snprintf(NULL, 0, "%.1f", num); // get the length of the number as string
	char * str = malloc(length + 1); // allocate space for the string, plus one for the null terminator
	if (str == NULL ) return NULL; // allocation failed
	(dtype == dint) ? snprintf(str, length + 1, "%.0f", num) : snprintf(str, length + 1, "%.1f", num); // format the number as a string and store it in the char array
	return str;
}

DataNode * Operation(DataNode * node1, DataNode * node2, enum operation op) {
	// (int:0, float:1, string:2)
	char message[100]; // assuming a message won't be longer than 100 characters

	if (((op == opAdd) && (node1->dtype ^ node2->dtype) > 1) // if only 1 of the expressions is  string 
		|| (((op == opSub) || (op == opDiv)) && ((node1->dtype == dstring) || (node2->dtype == dstring))) // if either of the operrands is string
		|| ((op == opMul) && ((node1->dtype == dstring) || ((node1->dtype ^ node2->dtype) == 3)))) {  // if first operrand is string or first operrand is float and second one is string
		
		snprintf(message, sizeof(message), "Type mismatch on %d\n", node1->lineNum);
		LogIt(message, NULL);
		return NULL;
	}
	DataNode * newNode = (DataNode *)malloc(sizeof(DataNode));
	
	if (node2->dtype == dstring) { 
		if (node1->dtype == dstring) { // both expressions are strings, it must be add based on previous checks
			char * result = (char *)malloc(strlen(node1->value) + strlen(node2->value) + 1); // +1 for the null terminator
			strcpy(result, node1->value);
			strcat(result, node2->value);						
			newNode->value = result;
			
		} else { // only second expression is string, it must be multiplication based on previous checks
			int val1 = atoi(node1->value);

			if (val1>-1) {
				char * result = (char *)malloc( val1 * strlen(node2->value) + 1); // +1 for the null terminator

				if (val1 == 0) result[0] = '\0';
				else {
					strcpy(result, node2->value);
					for (; val1>1; val1--)
						strcat(result, node2->value);
				}					
				newNode->value = result;
				
			} else { // negative int and string multiplication
				snprintf(message, sizeof(message), "Type mismatch on %d\n", node1->lineNum);
				LogIt(message, NULL);
				return NULL;
			}
		}
		newNode->dtype = dstring;
		snprintf(message, sizeof(message), "Result of expression on %d is (%s)\n", node1->lineNum, newNode->value);

	} else { // num to num operation
		char dtype = ((node1->dtype == dfloat) || (node2->dtype == dfloat)) ? dfloat : dint;
		double result;
		char * sresult;

		if (op == opAdd) result = atof(node1->value) + atof(node2->value);
		else if (op == opSub) result = atof(node1->value) - atof(node2->value);
		else if (op == opMul) result = atof(node1->value) * atof(node2->value);
		else result = atof(node1->value) / atof(node2->value);		

		if (dtype == dint) {
			snprintf(message, sizeof(message), "Result of expression on %d is (%.0f)\n", node1->lineNum, result);
			sresult = NumToStr(result, dint);
		} else {
			snprintf(message, sizeof(message), "Result of expression on %d is (%.1f)\n", node1->lineNum, result);
			sresult = NumToStr(result, dfloat);
		}

		newNode->value = sresult;
		newNode->dtype = dtype;
	}

	newNode->lineNum = node1->lineNum;

	// if the operation is operation of 2 literals (parents of nodes are NULL), give NULL to parent for now it will be used in LogIt 
	// I didn't want to create a new field in strcut for this as the solution was easy
	(!(node1->parent1) && !(node1->parent2)) ? (newNode->parent1 = NULL) : (newNode->parent1 = node1);
	(!(node2->parent1) && !(node2->parent2)) ? (newNode->parent2 = NULL) : (newNode->parent2 = node2);

	LogIt(message, newNode);
	newNode->parent1 = node1; // for the aforementioned literal operation revert the parents to nodes instead of NULLS
	newNode->parent2 = node2;
	return newNode;
}

void LogIt(char * message, DataNode * owner){
	LogNode * temp = (LogNode *)malloc(sizeof(LogNode));
	temp->message = malloc(strlen(message)+1);
	strcpy(temp->message, message);
	temp->owner = owner;
	
	// if the log is caused by typemismatch there is no owner, do not delete its parent's log either
	if(owner) {
		// if there exist a parent operation to this operation, they would have logged their messages just before this
		// if the log is caused by a lowest level operation literal + literal, there are no parents
		// delete number of last messages equal to number of parents
		char pc = 0;
		if(owner->parent1) pc++;
		if(owner->parent2) pc++;
		for(; pc>0; pc--) {
			free(Log[--log_index]->message);
			Log[log_index] = NULL;
		}
	}

	Log[log_index++] = temp;

	if(log_index == log_size) {
		log_size *= 2;
		Log = realloc(Log, log_size * sizeof(LogNode **));
	}
}
int main ()
{
	Log = (LogNode **)malloc(log_size*sizeof(LogNode *));

	if (yyparse()) { // parse error
		printf("ERROR\n");
		return 1;
	}

	unsigned int i=0;
	for (; i<log_index; i++) {
		printf("%s",Log[i]->message);
	}

	return 0;	
}
