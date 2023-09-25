%{

#include<stdio.h>
#include<string.h>

int oc_cc=0;
char* nameOfToken(int x);

#define KW_AND                      1
#define KW_OR                       2
#define KW_NOT                      3
#define KW_EQUAL                    4
#define KW_LESS                     5
#define KW_NIL                      6
#define KW_LIST			            7
#define KW_APPEND                   8
#define KW_CONCAT                   9
#define KW_SET                      10
#define KW_DEFFUN                   11
#define KW_FOR                		12
#define KW_IF                      	13
#define KW_EXIT                     14
#define KW_LOAD                     15
#define KW_DISP						16
#define KW_TRUE						17
#define KW_FALSE					18
#define OP_PLUS						19
#define OP_MINUS 					20
#define OP_DIV 						21
#define OP_DBLMULT 					22
#define OP_MULT 					23
#define OP_OP 						24
#define OP_CP 						25
#define OP_OC 						26
#define OP_CC						27
#define OP_COMMA 					28
#define COMMENT 					29
#define IDENTIFIER 					30
#define VALUEI 		   				31
#define NEWLINE 					32
#define WHITESPACE 					33
#define END_OF_FILE					34
#define UNKNOWN						35
#define VALUEF						36
#define VALUESTR					37

%}

%%

[aA][nN][dD]				{return KW_AND;}
[oO][rR]					{return KW_OR;}
[nN][oO][tT]				{return KW_NOT;}
[eE][qQ][uU][aA][lL]		{return KW_EQUAL;}
[lL][eE][sS][sS]			{return KW_LESS;}
[nN][iI][lL]				{return KW_NIL;}
[lL][iI][sS][tT]			{return KW_LIST;}
[aA][pP][pP][eE][nN][dD]	{return KW_APPEND;}
[cC][oO][nN][cC][aA][tT]	{return KW_CONCAT;}
[sS][eE][tT]				{return KW_SET;}
[dD][eE][fF][fF][uU][nN]	{return KW_DEFFUN;}
[fF][oO][rR]				{return KW_FOR;}
[iI][fF]					{return KW_IF;}
[eE][xX][iI][tT]			{return KW_EXIT;}
[lL][oO][aA][dD]			{return KW_LOAD;}
[dD][iI][sS][pP]			{return KW_DISP;}
[tT][rR][uU][eE]			{return KW_TRUE;}
[fF][aA][lL][sS][eE]		{return KW_FALSE;}

[+]							{return OP_PLUS;}
[-] 						{return OP_MINUS;}
[/] 						{return OP_DIV;}
[*][*] 						{return OP_DBLMULT;}
[*] 						{return OP_MULT;}
[(] 						{return OP_OP;}
[)] 						{return OP_CP;}
["] 						{oc_cc %= 2;
							return (OP_OC + oc_cc++);}
[,]							{return OP_COMMA;}
[;][;] 						{int comment = 0;
							while(comment != NEWLINE)
							{comment = yylex();}
							return COMMENT;}

["].*["]					{return VALUESTR;}
[a-zA-Z_][a-zA-Z0-9_]*		{return IDENTIFIER;}
[0]|[1-9][0-9]*				{return VALUEI;}
[1-9][0-9]*f[0-9]*			{return VALUEF;}


[0][0-9]*					{return UNKNOWN;}
[0-9]*[a-zA-Z]*				{return UNKNOWN;}


^\n							{return 0;}
[\n]        				{return NEWLINE;}
[ \t]+						{return WHITESPACE;}
<<EOF>>     				{return END_OF_FILE;}
.							{return UNKNOWN;}

%% 

int yywrap(){}
int main(int argc, char const *argv[]){
	int code=-1;
	if(argc == 1){
		while(code != 0){
			code=yylex();
			if(code != NEWLINE && code != WHITESPACE && code != END_OF_FILE && code!=0){
				printf("%s: %s\n",yytext,nameOfToken(code));
			}
		};
	}
	else{
		FILE *fp_check;
		fp_check = fopen(argv[1],"r");

		if(fp_check == NULL)
			printf("%s is not found.\n", argv[1]);
		else{
			yyin = fp_check;
			while(code != END_OF_FILE){
				code=yylex();
				if(code != NEWLINE && code != WHITESPACE && code != END_OF_FILE && code!=0){
					printf("%s: %s\n",yytext,nameOfToken(code));
				}
			};
		}
	}
	return 0;
}

char* nameOfToken(int x){
	if(x == 1)
		return "KW_AND";
	else if (x == 2)
		return "KW_OR";
	else if (x == 3)
		return "KW_NOT";
	else if (x == 4)
		return "KW_EQUAL";
	else if (x == 5)
		return "KW_LESS";
	else if (x == 6)
		return "KW_NIL";
	else if (x == 7)
		return "KW_LIST";
	else if (x == 8)
		return "KW_APPEND";
	else if (x == 9)
		return "KW_CONCAT";
	else if (x == 10)
		return "KW_SET";
	else if (x == 11)
		return "KW_DEFFUN";
	else if (x == 12)
		return "KW_FOR";
	else if (x == 13)
		return "KW_IF";
	else if (x == 14)
		return "KW_EXIT";
	else if (x == 15)
		return "KW_LOAD";
	else if (x == 16)
		return "KW_DISP";
	else if (x == 17)
		return "KW_TRUE";
	else if (x == 18)
		return "KW_FALSE";
	else if (x == 19)
		return "OP_PLUS";
	else if (x == 20)
		return "OP_MINUS";
	else if (x == 21)
		return "OP_DIV";
	else if (x == 22)
		return "OP_DBLMULT";
	else if (x == 23)
		return "OP_MULT";
	else if (x == 24)
		return "OP_OP";
	else if (x == 25)
		return "OP_CP";
	else if (x == 26)
		return "OP_OC";
	else if (x == 27)
		return "OP_CC";
	else if (x == 28)
		return "OP_COMMA";
	else if (x == 29)
		return "COMMENT";
	else if (x == 30)
		return "IDENTIFIER";
	else if (x == 31)
		return "VALUEI";
	else if (x == 32)
		return "";
	else if (x == 33)
		return "";
	else if (x == 34)
		return "";
	else if (x == 35)
		return "ERROR";
	else if (x == 36)
		return "VALUEF";
	else if (x == 37)
		return "VALUESTR";

	else
		return "";
}
