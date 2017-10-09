
%{
#include<stdio.h>
int num_line=1;
%}
character '(.|\n)'
str \"[^"]*\"
%option noyywrap

%%
\n {num_line++;}
" "			{}
\t			{}
"auto"			{ printf("%d: AUTO\n",num_line); return (AUTO); }
"break"			{ printf("%d: BREAK\n",num_line); return (BREAK); }
"case"			{ printf("%d: CASE\n",num_line); return (CASE); } 
"char"			{ printf("%d: CHAR\n",num_line); return (CHAR); }
"const"			{ printf("%d: CONST\n",num_line); return (CONST); } 
"continue"		{ printf("%d: CONTINUE\n",num_line); return (CONTINUE); }
"default"		{ printf("%d: DEFAULT\n",num_line); return (DEFAULT); }
"do"			{ printf("%d: DO\n",num_line); return (DO); }
"double"		{ printf("%d: DOUUBLE\n",num_line); return (DOUBLE); }
"else"			{ printf("%d: ELSE\n",num_line); return (ELSE); }
"enum"			{ printf("%d: ENUM\n",num_line); return (ENUM); }
"extern"		{ printf("%d: EXTERN\n",num_line); return (EXTERN); }
"float"			{ printf("%d: FLOAT\n",num_line); return (FLOAT); }
"for"			{ printf("%d: FOR\n",num_line); return (FOR); }
"goto"			{ printf("%d: GOTO\n",num_line); return (GOTO); }
"if"			{ printf("%d: IF\n",num_line); return (IF); }
"int"			{ printf("%d: INT\n",num_line); return (INT); }
"long"			{ printf("%d: LONG\n",num_line); return (LONG); }
"register"		{ printf("%d: REGISTER\n",num_line); return (REGISTER); }
"return"		{ printf("%d: RETURN\n",num_line); return (RETURN); }
"short"			{ printf("%d: SHORT\n",num_line); return (SHORT); }
"signed"		{ printf("%d: SIGNED\n",num_line); return (SIGNED); }
"sizeof"		{ printf("%d: SIZEOF\n",num_line); return (SIZEOF); }
"static"		{ printf("%d: STATIC\n",num_line); return (STATIC); }
"struct"		{ printf("%d: STRUCT\n",num_line); return (STRUCT); }
"switch"		{ printf("%d: SWITCH\n",num_line); return (SWITCH); }
"typedef"		{ printf("%d: TYPEDEF\n",num_line); return (TYPEDEF); }
"union"			{ printf("%d: UNION\n",num_line); return (UNION); }
"unsigned"		{ printf("%d: UNSIGNED\n",num_line); return (UNSIGNED); }
"void"			{ printf("%d: VOID\n",num_line); return (VOID); }
"volatile"		{ printf("%d: VOLATILE\n",num_line); return (VOLATILE); }
"while"			{ printf("%d: WHILE\n",num_line); return (WHILE); }
[A-Za-z_][A-Za-z0-9_]*  { printf("%d: ID %s\n",num_line,yytext);strcpy (yylval.name2, yytext); return (ID);}
[0-9]+[["."][0-9]+]? {printf("%d: REAL_NUMBER %s\n",num_line,yytext);strcpy (yylval.name2, yytext); return REAL_NUMBER; }
[0-9]+[["."][0-9]+]?[[Ee][+-]?[0-9]+[["."][0-9]+]?]? {printf("%d: EXP_NUMBER %s\n",num_line,yytext);strcpy (yylval.name2, yytext); return EXP_NUMBER;}
[0-9]+ {printf("%d: NUMBER %s\n",num_line,yytext);strcpy (yylval.name2, yytext); return NUMBER; }
{character}		{ printf("%d: CHAR_CONSTANT: %s\n",num_line,yytext); strcpy (yylval.name2, yytext); return CHAR_CONSTANT; }
{str}			{ printf("%d: STRING: %s\n",num_line,yytext); return STRING; }

"+"			{ printf("%d: PLUS\n",num_line); return PLUS; }
"-"			{ printf("%d: MINUS\n",num_line); return MINUS; }
"*"			{ printf("%d: MULT\n",num_line); return MULT; }
"/"			{ printf("%d: DIVIDE\n",num_line); return DIVIDE; }
"%"			{ printf("%d: MOD\n",num_line); return MOD; }
"++"			{ printf("%d: INC_OP\n",num_line); return INC_OP; }
"--"			{ printf("%d: DEC_OP\n",num_line); return DEC_OP; }
"=="			{ printf("%d: EQ_OP\n",num_line); return EQ_OP; }
"!="			{ printf("%d: NE_OP\n",num_line); return NE_OP; }
">"			{ printf("%d: GT_OP\n",num_line); return GT_OP; }
"<"			{ printf("%d: LT_OP\n",num_line); return LT_OP; }
"<="			{ printf("%d: LE_OP\n",num_line); return LE_OP; }
">="		    	{ printf("%d: GE_OP\n",num_line); return GE_OP; }
"&&"			{ printf("%d: AND_OP\n",num_line); return AND_OP; }
"||"			{ printf("%d: OR_OP\n",num_line); return OR_OP; }
"!"			{ printf("%d: NOT\n",num_line); return NOT; }
"&"			{ printf("%d: BIT_AND\n",num_line); return BIT_AND; }		
"|"			{ printf("%d: BIT_OR\n",num_line); return BIT_OR; }
"^"			{ printf("%d: XOR\n",num_line); return XOR; }
"~"			{ printf("%d: BIT_COMPLEMENT\n",num_line); return BIT_COMPLEMENT; }
">>"			{ printf("%d: RIGHT_ASSIGN\n",num_line); return RIGHT_OP; }
"<<"			{ printf("%d: LEFT_OP\n",num_line); return LEFT_OP; }
"="			{ printf("%d: ASSIGN\n",num_line); return ASSIGN; }
"+="			{ printf("%d: ADD_ASSIGN\n",num_line); return ADD_ASSIGN; }
"-="			{ printf("%d: SUB_ASSIGN\n",num_line); return SUB_ASSIGN; }
"*="			{ printf("%d: MUL_ASSIGN\n",num_line); return MUL_ASSIGN; }
"/="			{ printf("%d: DIVIDE_ASSIGN\n",num_line); return DIVIDE_ASSIGN; }
"%="			{ printf("%d: MOD_ASSIGN\n",num_line); return MOD_ASSIGN; }
">>="			{ printf("%d: RIGHT_ASSIGN\n",num_line); return RIGHT_ASSIGN; }
"<<="			{ printf("%d: LEFT_ASSIGN\n",num_line); return LEFT_ASSIGN; }
"&="			{ printf("%d: AND_ASSIGN\n",num_line); return AND_ASSIGN; }
"^="			{ printf("%d: XOR_ASSIGN\n",num_line); return XOR_ASSIGN; }
"|="			{ printf("%d: OR_ASSIGN\n",num_line); return OR_ASSIGN; }
"->"			{ printf("%d: PTR_OP\n",num_line); return PTR_OP; }
","			{ printf("%d: COMMA\n",num_line); return COMMA; }
"["			{ printf("%d: OB\n",num_line); return OB; }
"]"			{ printf("%d: CB\n",num_line); return CB; }
"{"			{ printf("%d: OBR\n",num_line); return OBR; }
"}"			{ printf("%d: CBR\n",num_line); return CBR; }
"("			{ printf("%d: OP\n",num_line); return OP; }
")"			{ printf("%d: CP\n",num_line); return CP; }
"..."                   { printf("%d: ELLIPSIS \n",num_line); return ELLIPSIS;}
";"			{ printf("%d: SC\n",num_line); return SC; }
":"			{ printf("%d: COLON\n",num_line); return COLON; }
"."			{ printf("%d: DOT\n",num_line); return DOT; }
"?"			{ printf("%d: QUES\n",num_line); return QUES; }
.                       { printf("%d: ERROR",num_line);}


%%

