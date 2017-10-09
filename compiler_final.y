%{
     #include<stdio.h>
     #include<string.h>
     #include<stdlib.h>
     extern int lines;
	 char temp[100];
	 FILE *ptr,*fp2;
     struct symbol_table
     {
	char var_name[110];
	int var_type;
	int line_num;
	char var_size[110];
	char var_initval[110];
	int isarray;
	int dec;
	int undec;
        int dimen[100];
        int dimlen;
     }sym_tab[10010];
	 struct quad
	 {
	 char opcode[110];
	 char op1[110];
	 char op2[110];
	 char result[110];
	 }qua[10010];
     int u=0,it,numm,status,st=0,size_flag=0,arflag=0,set_arr=0,dim=0,starttag=0,i,cav=-1,arrstr,notvalidarr=0;
     int counter1=0,counter2=0,counter3=0,counter=0,stktop=-1,cnt=0,temp_top=0;;
	 char base[110],var1[110],lass[110]="x0000",fr[110],la[110];
	 void gen_var(char var[]);
	 void push(char var[]);
	 void generate_temp2();
	 void generate_code();
	 void addtoquad(char* opcode, char* op1, char* op2,char* result);
	 char lab1[11];
	 char label_stack[110][110];
	 char temp_stack[100][100];
	 void pop();
	 int convert(char *str);
	 char v[11],size[3],for_arr[110],mark[110],l1[110],l2[110];
	 
%}
%token AUTO BREAK CASE CHAR CONST CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN FLOAT FOR GOTO IF INT LONG REGISTER RETURN SHORT
%token SIGNED SIZEOF STATIC STRUCT SWITCH TYPEDEF UNION UNSIGNED VOID VOLATILE WHILE 
%token NUMBER REALNUMBER EXPONENTIALNUMBER IDENTIFIER
%token PLUS MINUS DIVIDE MULTIPLY RIGHTSHIFT LEFTSHIFT AND LOGICALAND OR LOGICALOR NOT NOTEQUAL LESSTHAN GREATERTHANEQUALTO
%token LESSTHANEQUALTO EQUALEQUAL GREATERTHAN EQUAL LEFTSHIFTEQUALTO RIGHTSHIFTEQUALTO XOR INCREMENT DECREMENT PLUSEQUALTO
%token MINUSEQUALTO MULTIPLYEQUALTO DIVIDEEQUALTO BITWISENOT MODULO ORBITEQUALTO XORBITEQUALTO REFERENCE MODULOEQUALTO
%token ANDBITEQUALTO COLON DOT SEMICOLON COMMA ROUNDBRACKETOPEN ROUNDBRACKETCLOSE CURLYBRACKETOPEN CURLYBRACKETCLOSE
%token SQUAREBRACKETOPEN SQUAREBRACKETCLOSE CONST_CHAR CONST_STRING QUESTION ELLIPSIS

%union
{
   int value;
   char name[110];
}
%type <name> iteration_statement expression assignment_expression assignment_operator conditional_expression constant_expression logical_or_expression logical_and_expression inclusive_or_expression exclusive_or_expression and_expression equality_expression relational_expression shift_expression additive_expression multiplicative_expression cast_expression unary_expression unary_operator postfix_expression primary_expression argument_expression_list constant 
%%
translation_unit
	: external_declaration {printf("translation_unit: external_declaration\n");}
	| translation_unit external_declaration {printf("translation_unit: translation_unit external_declaration\n");}
	;

external_declaration
	: function_definition {printf("external_declaration: function_definition\n");}
	| declaration {printf("external_declaration: declaration\n");}
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement {printf("function_definition: declaration_specifiers declarator declaration_list compound_statement\n");}
	| declaration_specifiers declarator compound_statement {printf("function_definition: declaration_specifiers declarator compound_statement\n");}
	| declarator declaration_list compound_statement {printf("function_definition: declarator declaration_list compound_statement\n");}
	| declarator compound_statement {printf("function_definition: declarator compound_statement\n");}
	;

declaration
	: declaration_specifiers SEMICOLON   	{printf("declaration: declaration_specifiers\n");}
	| declaration_specifiers init_declarator_list SEMICOLON	
		{
		   counter=counter1+counter2+counter3;
		   for(it=st;it<u;it++) { sym_tab[it].var_type=counter; }
		   counter=0; counter1=0; counter2=0; counter3=0;
		   st=u;
		   printf("declaration:declaration_specifiers init_declarator_list\n");
		}
	;

declaration_list
	: declaration {printf("declaration_list: declaration\n");}
	| declaration_list declaration {printf("declaration_list: declaration_list declaration\n");}
	;

declaration_specifiers
	: storage_class_specifier declaration_specifiers	{printf("declaration_specifiers: storage_class_specifier declaration_specifiers\n");}
	| storage_class_specifier				{printf("declaration_specifiers: storage_class_specifier\n");}
	| type_specifier declaration_specifiers			{printf("declaration_specifiers: type_specifier declaration_specifiers\n");}
	| type_specifier					{printf("declaration_specifiers: type_specifier\n");}
	| type_qualifier declaration_specifiers			{printf("declaration_specifiers: type_qualifier declaration_specifiers\n");}
	| type_qualifier					{printf("declaration_specifiers: type_qualifier\n");}	
	;

storage_class_specifier
	: TYPEDEF	        {printf("storage_class_specifier: TYPEDEF\n");}
	| EXTERN		{printf("storage_class_specifier: EXTERN\n");}
	| STATIC		{printf("storage_class_specifier: STATIC\n");}
	| AUTO			{printf("storage_class_specifier: AUTO\n");}
	| REGISTER		{printf("storage_class_specifier: REGISTER\n");}
	;

type_specifier
	: VOID		{printf("type_specifier: VOID\n");}
	| CHAR	
		{
		   if(counter1>0 || counter3>0) {printf("Invalid Datatype\n");  exit(0);}
		   counter1+=3;
		   printf("type_specifier: CHAR\n");
		}

	| SHORT	
		{
		   if(counter3>0 || counter1==2 || counter1==3 || counter1==4) {printf("Invalid Datatype\n");  exit(0);}
		   counter3+=300;
		   printf("type_specifier: SHORT\n");
		}

	| INT		
		{
		   if(counter1>0) {printf("Invalid Datatype\n");  exit(0);}
		   counter1+=1; 
		   printf("type_specifier: INT\n");
		}

	| LONG	
		{
		   if(counter3>100 || counter1==2 || counter1==3) {printf("Invalid Datatype\n");  exit(0);}
		   counter3+=100;
		   printf("type_specifier: LONG\n");
		}

	| FLOAT	
		{
		   if(counter1>0 || counter2>0 || counter3>0) {printf("Invalid Datatype\n");  exit(0);}
		   counter1+=2;
		   printf("type_specifier: FLOAT\n");
		}

	| DOUBLE
		{
		   if(counter1>0 || counter2>0 || counter3==300) {printf("Invalid Datatype\n");  exit(0);}
		   counter1+=4;
		   printf("type_specifier:DOUBLE\n");
		}

	| SIGNED
		{
		   if(counter1==2 || counter1==4 || counter2>0) {printf("Invalid Datatype\n");  exit(0);}
		   counter2+=10;
		   printf("type_specifier: SIGNED\n");
		}

	| UNSIGNED
		{
		   if(counter1==2 || counter1==4 || counter2>0) {printf("Invalid Datatype\n");  exit(0);}
		   counter2+=40;
		   printf("type_specifier: UNSIGNED\n");
		}
	| struct_or_union_specifier	{printf("type_specifier: struct_or_union_specifier\n");}
	| enum_specifier		{printf("type_specifier: enum_specifier\n");}
	;

type_qualifier
	: CONST		{printf("type_qualifier: CONST\n");}
	| VOLATILE	{printf("type_qualifier: VOLATILE\n");}
	;

struct_or_union_specifier
	: struct_or_union CURLYBRACKETOPEN struct_declaration_list CURLYBRACKETCLOSE      {printf("struct_or_union_specifier: struct_or_union CURLYBRACKETOPEN struct_declaration_list CURLYBRACKETCLOSE\n");}
	| struct_or_union IDENTIFIER CURLYBRACKETOPEN struct_declaration_list CURLYBRACKETCLOSE		{printf("struct_or_union_specifier: struct_or_union IDENTIFIER CURLYBRACKETOPEN struct_declaration_list CURLYBRACKETCLOSE\n");}
	| struct_or_union IDENTIFIER	{printf("struct_or_union_specifier: struct_or_union IDENTIFIER\n");}
	;

struct_or_union
	: STRUCT	{printf("struct_or_union: STRUCT\n");}
	| UNION		{printf("struct_or_union: UNION\n");}
	;

struct_declaration_list
	: struct_declaration		{printf("struct_declaration_list: struct_declaration\n");}
	| struct_declaration_list struct_declaration	{printf("struct_declaration_list: struct_declaration_list struct_declaration\n");}
	;

init_declarator_list
	: init_declarator   {printf("init_declarator_list: init_declarator\n");}
	| init_declarator_list COMMA init_declarator   {printf("init_declarator_list: init_declarator_list init_declarator\n");}
	;

init_declarator
	: declarator EQUAL initializer		{printf("init_declarator: declarator initializer\n");}
	| declarator					{printf("init_declarator: declarator\n");}
	;

struct_declaration
	: specifier_qualifier_list struct_declarator_list SEMICOLON	{printf("struct_declaration: specifier_qualifier_list struct_declarator_list\n");}
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list	{printf("specifier_qualifier_list: type_specifier specifier_qualifier_list\n");}
	| type_specifier				{printf("specifier_qualifier_list: type_specifier\n");}
	| type_qualifier specifier_qualifier_list	{printf("specifier_qualifier_list: type_qualifier specifier_qualifier_list\n");}
	| type_qualifier				{printf("specifier_qualifier_list: type_qualifier\n");}
	;

struct_declarator_list
	: struct_declarator		{printf("struct_declarator_list: struct_declarator\n");}
	| struct_declarator_list COMMA struct_declarator	{printf("struct_declarator_list: struct_declarator_list struct_declarator\n");}
	;

struct_declarator
	: COLON constant_expression		{printf("struct_declarator: constant_expression\n");}
	| declarator COLON constant_expression	{printf("struct_declarator: declarator constant_expression\n");}
	| declarator				{printf("struct_declarator: declarator\n");}
	;

enum_specifier
	: ENUM CURLYBRACKETOPEN enumerator_list CURLYBRACKETCLOSE  {printf("enum_specifier: ENUM enumerator_list\n");}
	| ENUM IDENTIFIER CURLYBRACKETOPEN enumerator_list CURLYBRACKETCLOSE {printf("enum_specifier: ENUM IDENTIFIER enumerator_list\n");}
	| ENUM IDENTIFIER {printf("enum_specifier: ENUM IDENTIFIER\n");}
	;

enumerator_list
	: enumerator {printf("enumerator_list: enumerator\n");}
	| enumerator_list COMMA enumerator {printf("enumerator_list: enumerator_list enumerator\n");}
	;

enumerator
	: IDENTIFIER EQUAL constant_expression {printf("enumerator: IDENTIFIER constant_expression\n");}
	| IDENTIFIER {printf("enumerator: IDENTIFIER\n");}
	;

declarator
	: pointer direct_declarator {printf("declarator: pointer direct_declarator\n");}
	| direct_declarator {printf("declarator: direct_declarator\n");}
	;

direct_declarator
	: IDENTIFIER 
                { 
				if(find_id(yylval.name))
                             return yyerror();
		        else
		        {
			       strcpy(sym_tab[u].var_name,yylval.name);
			       if(strcmp(sym_tab[u].var_name,"main")==0) {sym_tab[u].var_type=counter1; counter1=0; counter2=0; counter3=0; }
			       sym_tab[u].line_num=lines;
			       strcpy(sym_tab[u].var_initval,"");
                   u++;
 		           printf("direct_declarator: IDENTIFIER\n");
		        }
				}
	| ROUNDBRACKETOPEN declarator ROUNDBRACKETCLOSE {printf("direct_declarator: declarator\n");}
	| direct_declarator SQUAREBRACKETOPEN SQUAREBRACKETCLOSE 
		{
		   sym_tab[u-1].isarray=1;
		   sym_tab[u-1].dimlen++;
		   printf("direct_declarator: direct_declarator SQUAREBRACKETOPEN SQUAREBRACKETCLOSE\n");
		}

        | direct_declarator SQUAREBRACKETOPEN constant_expression SQUAREBRACKETCLOSE 
		{
		   sym_tab[u-1].isarray=1;
		   strcat(sym_tab[u-1].var_size,temp);
		   numm=toNum(temp);
                   sym_tab[u-1].dimen[sym_tab[u-1].dimlen]=numm;
		   sym_tab[u-1].dimlen++;
                   strcat(sym_tab[u-1].var_size," ");
		   printf("direct_declarator: direct_declarator SQUAREBRACKETOPEN constant_expression SQUAREBRACKETCLOSE\n");
		}
	| direct_declarator ROUNDBRACKETOPEN parameter_type_list ROUNDBRACKETCLOSE {printf("direct_declarator: direct_declarator ROUNDBRACKETOPEN parameter_type_list ROUNDBRACKETCLOSE\n");}
	| direct_declarator ROUNDBRACKETOPEN ROUNDBRACKETCLOSE {printf("direct_declarator: direct_declarator ROUNDBRACKETOPEN ROUNDBRACKETCLOSE\n");}
	| direct_declarator ROUNDBRACKETOPEN identifier_list ROUNDBRACKETCLOSE {printf("direct_declarator: direct_declarator ROUNDBRACKETOPEN identifier_list ROUNDBRACKETCLOSE\n");}
	;

pointer
	: MULTIPLY type_qualifier_list pointer {printf("pointer: MULTIPLY type_qualifier_list pointer\n");}
	| MULTIPLY type_qualifier_list  {printf("pointer: MULTIPLY type_qualifier_list\n");}
	| MULTIPLY pointer  {printf("pointer: MULTIPLY pointer\n");}
	| MULTIPLY  {printf("pointer: MULTIPLY\n");}
	;

type_qualifier_list
	: type_qualifier  {printf("type_qualifier_list: type_qualifier\n");}
	| type_qualifier_list type_qualifier {printf("type_qualifier_list: type_qualifier_list type_qualifier\n");}
	;


parameter_type_list
	: parameter_list COMMA ELLIPSIS  {printf("parameter_type_list: parameter_list COMMA ELLIPSIS\n");} 
	| parameter_list  {printf("parameter_type_list: parameter_list\n");}
	;

parameter_list
	: parameter_declaration {printf("parameter_list: parameter_declaration\n");}
	| parameter_list COMMA parameter_declaration {printf("parameter_list: parameter_list COMMA parameter_declaration\n");}
	;

parameter_declaration
	: declaration_specifiers declarator {printf("parameter_declaration: declaration_specifiers declarator\n");}
	| declaration_specifiers abstract_declarator {printf("parameter_declaration: declaration_specifiers abstract_declarator\n");}
	| declaration_specifiers {printf("parameter_declaration: declaration_specifiers\n");}
	;

identifier_list
	: IDENTIFIER {printf("identifier_list: IDENTIFIER\n");}
	| identifier_list COMMA IDENTIFIER {printf("identifier_list: identifier_list COMMA IDENTIFIER\n");}
	;

initializer
	: assignment_expression 
	  {
	  if(set_arr!=1)
		  {
		  strcat(sym_tab[u-1].var_initval," ");
		  strcat(sym_tab[u-1].var_initval,temp);
		  }
	  printf("initializer: assignment_expression\n");
	  }
	| CURLYBRACKETOPEN initializer_list CURLYBRACKETCLOSE {set_arr=1;printf("initializer: { initializer_list }\n");}
	| CURLYBRACKETOPEN initializer_list COMMA CURLYBRACKETCLOSE {printf("initializer: {initializer_list , }\n");}
	;

initializer_list
	: initializer  
	  {  
	      /*if(set_arr!=1)
		  {
		  strcat(sym_tab[u-1].var_initval," ");
		  strcat(sym_tab[u-1].var_initval,temp);
		  }*/
	      printf("initializer_list: initializer\n");
	  }
	| initializer_list COMMA initializer   
	  {
	      /*if(set_arr!=1)
		  {
	      strcat(sym_tab[u-1].var_initval," ");
		  strcat(sym_tab[u-1].var_initval,temp);
		  }*/
	      printf("initializer_list: initializer_list COMMA initializer\n");
	  }
	;

type_name
	: specifier_qualifier_list abstract_declarator  {printf("type_name: specifier_qualifier_list abstract_declarator\n");}
	| specifier_qualifier_list  {printf("type_name: specifier_qualifier_list\n");}
	;

abstract_declarator
	: pointer direct_abstract_declarator {printf("abstract_declarator: pointer direct_abstract_declarator\n");}
	| pointer   {printf("abstract_declarator: pointer\n");}
	| direct_abstract_declarator  {printf("abstract_declarator: direct_abstract_declarator\n");}
	;

direct_abstract_declarator
	: ROUNDBRACKETOPEN abstract_declarator ROUNDBRACKETCLOSE {printf("direct_abstract_declarator: ( abstract_declarator )\n");}
	| SQUAREBRACKETOPEN SQUAREBRACKETCLOSE  {printf("direct_abstract_declarator: []\n");}
	| ROUNDBRACKETOPEN ROUNDBRACKETCLOSE {printf("direct_abstract_declarator: ()\n");}
	| ROUNDBRACKETOPEN parameter_type_list ROUNDBRACKETCLOSE {printf("direct_abstract_declarator: (parameter_type_list)\n");}
	| direct_abstract_declarator ROUNDBRACKETOPEN ROUNDBRACKETCLOSE {printf("direct_abstract_declarator: direct_abstract_declarator ()\n");}
	| direct_abstract_declarator ROUNDBRACKETOPEN parameter_type_list ROUNDBRACKETCLOSE {printf("direct_abstract_declarator: direct_abstract_declarator (parameter_type_list)\n");}
	| direct_abstract_declarator SQUAREBRACKETOPEN constant_expression SQUAREBRACKETCLOSE {printf("direct_abstract_declarator: direct_abstract_declarator [constant_expression]\n");}
	| direct_abstract_declarator SQUAREBRACKETOPEN SQUAREBRACKETCLOSE  {printf("direct_abstract_declarator: direct_abstract_declarator []\n");}
	| SQUAREBRACKETOPEN constant_expression SQUAREBRACKETCLOSE {printf("direct_abstract_declarator: [constant_expression]\n");}
	;

statement
	: labeled_statement {printf("statement: labeled_statement\n");}
	| compound_statement  {printf("statement: compound_statement\n");}
	| expression_statement  {printf("statement: expression_statement\n");}
	| selection_statement  {printf("statement: selection_statement\n");}
	| iteration_statement  {printf("statement: iteration_statement\n");}
	| jump_statement {printf("statement: jump_statement\n");}
	;
 
labeled_statement
	: IDENTIFIER COLON statement  {printf("labeled_statement: IDENTIFIER COLON statement\n");}
	| CASE constant_expression COLON statement  {printf("labeled_statement: CASE constant_expression COLON statement\n");}
	| DEFAULT COLON statement  {printf("labeled_statement: DEFAULT COLON statement\n");}
	;

expression_statement
	: SEMICOLON  {arrstr=0;printf("expression_statement: SEMICOLON\n");}
	| expression SEMICOLON  {arrstr=0;printf("expression_statement: expression SEMICOLON\n");}
	;

compound_statement
	: CURLYBRACKETOPEN CURLYBRACKETCLOSE   {printf("compound_statement: {}\n");}
	| CURLYBRACKETOPEN declaration_list statement_list CURLYBRACKETCLOSE   {printf("compound_statement: {declaration_list statement_list}\n");}
	| CURLYBRACKETOPEN statement_list CURLYBRACKETCLOSE  {printf("compound_statement: {statement_list}\n");}
	| CURLYBRACKETOPEN declaration_list CURLYBRACKETCLOSE  {printf("compound_statement: {declaration_list }\n");}
	;

statement_list
        : statement   {printf("statement_list: statement\n");}
	| statement_list statement   {printf("statement_list: statement_list statement\n");}
	;
selection_statement
	: IF {arrstr=1;} ROUNDBRACKETOPEN expression ROUNDBRACKETCLOSE  
	    {
		char var[11];
	    	gen_var(var);
		var[0]='L';
	    	fprintf(ptr,"IF FALSE %s ,goto %s\n",$4,var);
		push(var);
		printf("selection_statement: IF ROUNDBRACKETOPEN expression ROUNDBRACKETCLOSE \n");
		addtoquad("IF",$4,var," ");
		}ss
	   
	| SWITCH ROUNDBRACKETOPEN expression ROUNDBRACKETCLOSE statement  {printf("selection_statement: SWITCH ROUNDBRACKETOPEN expression ROUNDBRACKETCLOSE statement\n");}
	;
ss 
   :statement   
      {
	  pop();
	  fprintf(ptr,"%s:\n",v);
	  printf("ss:statement\n");
	  addtoquad(":",v," "," ");
	  }
   |statement ELSE 
      {
	  char var[11];
	  gen_var(var);
	  var[0]='L';
	  fprintf(ptr,"goto %s\n",var);
	  addtoquad("goto",var," "," ");
	  pop();
	  fprintf(ptr,"%s:\n",v);
	  addtoquad(":",v," "," ");
	  push(var);
	   printf("selection_statement: IF ROUNDBRACKETOPEN expression ROUNDBRACKETCLOSE statement ELSE statement\n");
	  } 
	  statement 
	  {
	  pop();
	  fprintf(ptr,"%s:\n",v);
	  addtoquad(":",v," "," ");
	  }
   ;
iteration_statement
	: WHILE 
	    {arrstr=1;
		char var[11];
	    gen_var(var);
	    var[0]='L';
		fprintf(ptr,"%s:\n",var);
		addtoquad(":",var," "," ");
		push(var);
		}
	    ROUNDBRACKETOPEN expression ROUNDBRACKETCLOSE   
	    {
		char var[11];
	    gen_var(var);
	    var[0]='L';
	    fprintf(ptr,"IF FALSE %s, goto %s\n",$4,var);
		addtoquad("IF",$4,var," ");
		push(var);
		printf("iteration_statement: WHILE ROUNDBRACKETOPEN expression ROUNDBRACKETCLOSE statement\n");
		}
		statement
		{
		pop();
		strcpy(lab1,v);
		pop();
		fprintf(ptr,"goto %s\n",v);
		addtoquad("goto",v," "," ");
		fprintf(ptr,"%s:\n",lab1);
		addtoquad(":",lab1," "," ");
		}
	| DO statement WHILE ROUNDBRACKETOPEN expression ROUNDBRACKETCLOSE SEMICOLON  {printf("iteration_statement: DO statement WHILE ROUNDBRACKETOPEN expression ROUNDBRACKETCLOSE SEMICOLON \n");}
	| FOR ROUNDBRACKETOPEN expression SEMICOLON expression SEMICOLON expression ROUNDBRACKETCLOSE statement  {printf("iteration_statement: FOR ROUNDBRACKETOPEN expression SEMICOLON expression SEMICOLON expression ROUNDBRACKETCLOSE statement\n");}
	| FOR ROUNDBRACKETOPEN expression SEMICOLON expression SEMICOLON ROUNDBRACKETCLOSE statement {printf("iteration_statement: FOR ROUNDBRACKETOPEN expression SEMICOLON expression SEMICOLON ROUNDBRACKETCLOSE statement \n");}
	| FOR ROUNDBRACKETOPEN expression SEMICOLON SEMICOLON expression ROUNDBRACKETCLOSE statement  {printf("iteration_statement: FOR ROUNDBRACKETOPEN expression SEMICOLON SEMICOLON expression ROUNDBRACKETCLOSE statement \n");}
	| FOR ROUNDBRACKETOPEN expression SEMICOLON SEMICOLON ROUNDBRACKETCLOSE statement  {printf("iteration_statement: FOR ROUNDBRACKETOPEN expression SEMICOLON SEMICOLON ROUNDBRACKETCLOSE statement\n");}
	| FOR ROUNDBRACKETOPEN SEMICOLON expression SEMICOLON expression ROUNDBRACKETCLOSE statement  {printf("iteration_statement: FOR ROUNDBRACKETOPEN SEMICOLON expression SEMICOLON expression ROUNDBRACKETCLOSE statement\n");}
	| FOR ROUNDBRACKETOPEN SEMICOLON expression SEMICOLON ROUNDBRACKETCLOSE statement  {printf("iteration_statement: FOR ROUNDBRACKETOPEN SEMICOLON expression SEMICOLON ROUNDBRACKETCLOSE statement\n");}
	| FOR ROUNDBRACKETOPEN SEMICOLON SEMICOLON expression ROUNDBRACKETCLOSE statement  {printf("iteration_statement: FOR ROUNDBRACKETOPEN SEMICOLON SEMICOLON expression ROUNDBRACKETCLOSE statement\n");}
	| FOR ROUNDBRACKETOPEN SEMICOLON SEMICOLON ROUNDBRACKETCLOSE statement	{printf("iteration_statement: FOR ROUNDBRACKETOPEN SEMICOLON SEMICOLON ROUNDBRACKETCLOSE statement\n");}
	;

jump_statement
	: GOTO IDENTIFIER SEMICOLON  {printf("jump_statement: GOTO IDENTIFIER SEMICOLON\n");}
	| CONTINUE SEMICOLON   {printf("jump_statement: CONTINUE SEMICOLON\n");}
	| BREAK SEMICOLON   {printf("jump_statement: BREAK SEMICOLON\n");}
	| RETURN SEMICOLON   {printf("jump_statement: RETURN SEMICOLON\n");}
	| RETURN expression SEMICOLON  {printf("jump_statement: RETURN expression SEMICOLON\n");}
	;

expression
	: assignment_expression		{printf("expression: assignment_expression\n");}
	| expression COMMA assignment_expression	{printf("expression: expression COMMA assignment_expression\n");}
	;

assignment_expression
	: conditional_expression		{printf("assignment_expression: conditional_expression\n");}
	| unary_expression assignment_operator assignment_expression		
	   {
	   starttag=0;
	   dim=0;
	   if(notvalidarr)
	   {
	   fprintf(ptr,"Array access: dimension mis-match @ line %d\nExiting...\n",lines);
	   notvalidarr=0;
	   yyerror();
	   }
	   
	   if(strcmp($2,"=")==0){fprintf(ptr,"%s = %s \n",$1,$3);addtoquad("=",$3," ",$1);}
	   if(strcmp($2,"+=")==0){fprintf(ptr,"%s = %s + %s \n",$1,$1,$3);addtoquad("+",$3,$1,$1);}
	   if(strcmp($2,"*=")==0){fprintf(ptr,"%s = %s * %s \n",$1,$1,$3);addtoquad("*",$3,$1,$1);}
	   if(strcmp($2,"/=")==0){fprintf(ptr,"%s = %s / %s \n",$1,$1,$3);addtoquad("/",$3,$1,$1);}
	   if(strcmp($2,"-=")==0){fprintf(ptr,"%s = %s - %s \n",$1,$1,$3);addtoquad("-",$3,$1,$1);}
	   if(strcmp($2,"^=")==0){fprintf(ptr,"%s = %s ^ %s \n",$1,$1,$3);addtoquad("^",$3,$1,$1);}
	   if(strcmp($2,"&=")==0){fprintf(ptr,"%s = %s & %s \n",$1,$1,$3);addtoquad("&",$3,$1,$1);}
	   if(strcmp($2,"|=")==0){fprintf(ptr,"%s = %s | %s \n",$1,$1,$3);addtoquad("|",$3,$1,$1);}
	   if(strcmp($2,"%=")==0){fprintf(ptr,"%s = %s % %s \n",$1,$1,$3);addtoquad("%",$3,$1,$1);}
	   if(strcmp($2,">>=")==0){fprintf(ptr,"%s = %s >> %s \n",$1,$1,$3);addtoquad(">>",$3,$1,$1);}
	   if(strcmp($2,"<<=")==0){fprintf(ptr,"%s = %s >> %s \n",$1,$1,$3);addtoquad("<<",$3,$1,$1);}
	   printf("assignment_expression: unary_expression assignment_operator assignment_expression\n");
	   }
	;

assignment_operator
	: EQUAL			{ arrstr=1;strcpy($$,"=");printf("assignment_operator: EQUAL\n");}
	| MULTIPLYEQUALTO			{strcpy($$,"*=");printf("assignment_operator: MULTIPLYEQUALTO\n");}
	| DIVIDEEQUALTO			{strcpy($$,"/=");printf("assignment_operator: DIVIDEEQUALTO\n");}
	| MODULOEQUALTO			{strcpy($$,"%=");printf("assignment_operator: MODULOEQUALTO\n");}
	| PLUSEQUALTO			{strcpy($$,"+=");printf("assignment_operator: PLUSEQUALTO\n");}
	| MINUSEQUALTO			{strcpy($$,"-=");printf("assignment_operator: MINUSEQUALTO\n");}
	| LEFTSHIFTEQUALTO			{strcpy($$,"<<=");printf("assignment_operator: LEFTSHIFTEQUALTO\n");}
	| RIGHTSHIFTEQUALTO			{strcpy($$,">>=");printf("assignment_operator: RIGHTSHIFTEQUALTO\n");}
	| ANDBITEQUALTO			{strcpy($$,"&=");printf("assignment_operator: ANDBITEQUALTO\n");}
	| XORBITEQUALTO			{strcpy($$,"^=");printf("assignment_operator: XORBITEQUALTO\n");}
	| ORBITEQUALTO			{strcpy($$,"|=");printf("assignment_operator: ORBITEQUALTO\n");}
	;

conditional_expression
	: logical_or_expression		{printf("conditional_expression: logical_or_expression\n");}
	| logical_or_expression QUESTION expression COLON conditional_expression	{printf("conditional_expression: logical_or_expression QUESTION expression COLON conditional_expression\n");}
	;

constant_expression
	: conditional_expression	{printf("constant_expression: conditional_expression\n");}
	;

logical_or_expression
	: logical_and_expression	{printf("logical_or_expression: logical_and_expression\n");}
	| logical_or_expression LOGICALOR logical_and_expression	
	    {
		char var[11]; 
		gen_var(var); strcpy(temp_stack[temp_top++],var);
		fprintf(ptr,"%s=%s OR %s\n",var,$1,$3);
		addtoquad("||",$1,$3,var);
		strcpy($$,var);
		printf("logical_or_expression: logical_or_expression LOGICALOR logical_and_expression\n");
		}
	;

logical_and_expression
	: inclusive_or_expression	{printf("logical_and_expression: inclusive_or_expression\n");}
	| logical_and_expression LOGICALAND inclusive_or_expression	
	   {
	   char var[11]; 
		gen_var(var); strcpy(temp_stack[temp_top++],var);
		fprintf(ptr,"%s=%s AND %s\n",var,$1,$3);
		strcpy($$,var);
		addtoquad("&&",$1,$3,var);
	   printf("logical_and_expression: logical_and_expression LOGICALAND inclusive_or_expression\n");
	   }
	;

inclusive_or_expression
	: exclusive_or_expression	{printf("inclusive_or_expression: exclusive_or_expression\n");}
	| inclusive_or_expression OR exclusive_or_expression	{printf("inclusive_or_expression: inclusive_or_expression OR exclusive_or_expression\n");}
	;

exclusive_or_expression
	: and_expression	{printf("exclusive_or_expression: and_expression\n");}
	| exclusive_or_expression XOR and_expression	{printf("exclusive_or_expression: exclusive_or_expression XOR and_expression\n");}
	;

and_expression
	: equality_expression		{printf("and_expression: equality_expression\n");}
	| and_expression AND equality_expression	{printf("and_expression: and_expression AND equality_expression\n");}
	;

equality_expression
	: relational_expression		{printf("equality_expression: relational_expression\n");}
	| equality_expression EQUALEQUAL relational_expression	
	    {
		char var[11]; 
		gen_var(var); strcpy(temp_stack[temp_top++],var);
		fprintf(ptr,"%s=%s EQ %s\n",var,$1,$3);
		addtoquad("==",$1,$3,var);
		strcpy($$,var);
		printf("equality_expression: equality_expression EQUALEQUAL relational_expression\n");
		}
	| equality_expression NOTEQUAL relational_expression	
	   {
	   char var[11]; 
	   gen_var(var); strcpy(temp_stack[temp_top++],var);
	   fprintf(ptr,"%s=%s NE %s\n",var,$1,$3);
	   addtoquad("!=",$1,$3,var);
	   strcpy($$,var);
	   printf("equality_expression: equality_expression NOTEQUAL relational_expression\n");
	   }
	;

relational_expression
	: shift_expression		{printf("relational_expression: shift_expression\n");}
	| relational_expression LESSTHAN shift_expression   
	    {
		char var[11]; 
		gen_var(var); strcpy(temp_stack[temp_top++],var);
		fprintf(ptr,"%s=%s LT %s\n",var,$1,$3);
		addtoquad("<",$1,$3,var);
		strcpy($$,var);
		printf("relational_expression: relational_expression LESSTHAN shift_expression\n");
		}
	| relational_expression GREATERTHAN shift_expression		
	   {
	   char var[11]; 
	   gen_var(var); strcpy(temp_stack[temp_top++],var);
	   fprintf(ptr,"%s=%s GT %s\n",var,$1,$3);
	   addtoquad(">",$1,$3,var);
	   strcpy($$,var);
	   printf("relational_expression: relational_expression GREATERTHAN shift_expression\n");
	   }
	| relational_expression LESSTHANEQUALTO shift_expression	
	  {
	  char var[11]; 
	  gen_var(var); strcpy(temp_stack[temp_top++],var);
	  fprintf(ptr,"%s=%s  LE %s\n",var,$1,$3);
	  addtoquad("<=",$1,$3,var);
	  strcpy($$,var);
	  printf("relational_expression: relational_expression LESSTHANEQUALTO shift_expression\n");
	  }
	| relational_expression GREATERTHANEQUALTO shift_expression	
	  {
	  char var[11]; 
	  gen_var(var); strcpy(temp_stack[temp_top++],var);
	  fprintf(ptr,"%s=%s  GE %s\n",var,$1,$3);
	  addtoquad(">=",$1,$3,var);
	  strcpy($$,var);
	  printf("relational_expression: relational_expression GREATERTHANEQUALTO shift_expression\n");
	  }
	;

shift_expression
	: additive_expression			{printf("shift_expression: additive_expression\n");}
	| shift_expression LEFTSHIFT additive_expression	{printf("shift_expression: shift_expression LEFTSHIFT additive_expression\n");}
	| shift_expression RIGHTSHIFT additive_expression	{printf("shift_expression: shift_expression RIGHTSHIFT additive_expression\n");}
	;

additive_expression
	: multiplicative_expression		{printf("additive_expression: multiplicative_expression\n");}
	| additive_expression PLUS multiplicative_expression		
	      {
		  char var[11]; 
		  gen_var(var); strcpy(temp_stack[temp_top++],var);
		  fprintf(ptr,"%s = %s + %s\n",var,$1,$3);
		  addtoquad("+",$1,$3,var);
		  strcpy($$,var);
		  printf("additive_expression: additive_expression PLUS multiplicative_expression\n");
		  }
	| additive_expression MINUS multiplicative_expression		
	     {
		 char var[11]; 
		 gen_var(var); strcpy(temp_stack[temp_top++],var);
		 fprintf(ptr,"%s = %s - %s\n",var,$1,$3);
		 addtoquad("-",$1,$3,var);
		 strcpy($$,var);
		 printf("additive_expression: additive_expression MINUS multiplicative_expression\n");
		 }
	;

multiplicative_expression
	: cast_expression		{printf("multiplicative_expression: cast_expression\n");}
	| multiplicative_expression MULTIPLY cast_expression		
	      {
		  char var[11]; 
		  gen_var(var); strcpy(temp_stack[temp_top++],var);
		  fprintf(ptr,"%s = %s * %s\n",var,$1,$3);
		  addtoquad("*",$1,$3,var);
		  strcpy($$,var);
		  printf("multiplicative_expression: multiplicative_expression MULTIPLY cast_expression\n");
		  }
	| multiplicative_expression DIVIDE cast_expression		
	     {
		 char var[11]; 
		 gen_var(var); strcpy(temp_stack[temp_top++],var);
		 fprintf(ptr,"%s = %s / %s\n",var,$1,$3);
		 addtoquad("/",$1,$3,var);
		 strcpy($$,var);
		 printf("multiplicative_expression: multiplicative_expression DIVIDE cast_expression\n");
		 }
	| multiplicative_expression MODULO cast_expression		{printf("multiplicative_expression: multiplicative_expression MODULO cast_expression\n");}
	;

cast_expression
	: unary_expression	{printf("cast_expression: unary_expression\n");}
	| ROUNDBRACKETOPEN type_name ROUNDBRACKETCLOSE cast_expression	{printf("cast_expression: ROUNDBRACKETOPEN type_name ROUNDBRACKETCLOSE cast_expression\n");}
	;

unary_expression
	: postfix_expression        {printf("unary_expression: postfix_expression\n");}
	| INCREMENT unary_expression   {printf("unary_expression: INCREMENT unary_expression\n");}
	| DECREMENT unary_expression   
	    {
		fprintf(ptr,"%s=%s-1\n",$2,$2);
		addtoquad("-",$2,"1",$2);
		strcpy($$,$2);
		printf("unary_expression: DECREMENT unary_expression\n");
		}
	| unary_operator cast_expression   
	    {
		char var[11]; 
		gen_var(var); strcpy(temp_stack[temp_top++],var);
		fprintf(ptr,"%s = %s%s\n",var,$1,$2);
		addtoquad($1,$2," ",var);
		strcpy($$,var);
		printf("unary_expression: unary_operator cast_expression\n");
		}
	| SIZEOF unary_expression       {printf("unary_expression: SIZEOF unary_expression\n");}
	| SIZEOF ROUNDBRACKETOPEN type_name ROUNDBRACKETCLOSE  {printf("unary_expression: SIZEOF ROUNDBRACKETOPEN type_name ROUNDBRACKETCLOSE\n");}
	;

unary_operator
	: AND                   {printf("unary_operator: AND\n");}
	| MULTIPLY              {printf("unary_operator: MULTIPLY\n");}
	| PLUS			{printf("unary_operator: PLUS\n");}
	| MINUS			{printf("unary_operator: MINUS\n");}
	| BITWISENOT		{printf("unary_operator: BITWISENOT\n");}
	| NOT			{printf("unary_operator: NOT\n");}
	;

postfix_expression
	: primary_expression     {printf("postfix_expression: primary_expression\n");}
	| postfix_expression SQUAREBRACKETOPEN expression SQUAREBRACKETCLOSE       
	    {
		if(starttag==0)
		{
		notvalidarr=1;
		char var[11]; 
		gen_var(var);
		strcpy(temp_stack[temp_top++],var);
		fprintf(ptr,"%s=0\n",var);
		addtoquad("=","0"," ",var);
		strcpy(base,var);
		starttag=1;
		cav=-1;
		for(i=0;i<u;i++)
		{
		if(strcmp(sym_tab[i].var_name,$1)==0)
		cav=i;
		}
		if(cav==-1)
		fprintf(ptr,"%s undeclared\n",$3);
		}
		char var[11]; 
		gen_var(var);
		strcpy(temp_stack[temp_top++],var);
		fprintf(ptr,"%s=%s\n",var,$3);
		addtoquad("=",$3," ",var);
		strcpy(var1,var);
		dim++;
		for(i=dim;i<sym_tab[cav].dimlen;i++)
		 {fprintf(ptr,"%s=%s*%d\n",var,var,sym_tab[cav].dimen[i]);
		 //addtoquad("*",var,sym_tab[cav].dimen[i],var);
		 }
		 
		fprintf(ptr,"%s=%s+%s\n",base,base,var);
		addtoquad("+",base,var,base);
		if(sym_tab[cav].var_type==1)
		strcpy(size,"4");
		if(sym_tab[cav].var_type==2)
		strcpy(size,"8");
		if(sym_tab[cav].var_type==3)
		strcpy(size,"1");
		if(sym_tab[cav].var_type==4)
		strcpy(size,"32");
		
		//fprintf(ptr,"%d %d\n",dim,sym_tab[cav].dimlen);
		if(dim==sym_tab[cav].dimlen && arrstr==0) 
		{
		fprintf(ptr,"%s=%s*%s\n",base,base,size);
		addtoquad("*",base,size,base);
		starttag=0;
		dim=0;
		strcpy($$,$1);
		strcat($$,"[");
		strcat($$,base);
		strcat($$,"]");
		notvalidarr=0;
		}
		//fprintf(ptr,"%d\n",arrstr);
		if(dim==sym_tab[cav].dimlen && arrstr==1)
		{
		fprintf(ptr,"%s=%s*%s\n",base,base,size);
		addtoquad("*",base,size,base);
		char var[11]; 
		gen_var(var);strcpy(temp_stack[temp_top++],var);
		fprintf(ptr,"%s=%s[%s]\n",var,sym_tab[cav].var_name,base);
		strcpy(for_arr,"");
		strcat(for_arr,sym_tab[cav].var_name);
		strcat(for_arr,"[");
		strcat(for_arr,base);
		strcat(for_arr,"]");
		addtoquad("=",for_arr," ",var);
		strcpy($$,var);
		starttag=0;
		notvalidarr=0;
		dim=0;
		}
		printf("postfix_expression: postfix_expression SQUAREBRACKETOPEN expression SQUAREBRACKETCLOSE\n");
		}
	| postfix_expression ROUNDBRACKETOPEN ROUNDBRACKETCLOSE                    {printf("postfix_expression: postfix_expression ROUNDBRACKETOPEN ROUNDBRACKETCLOSE\n");}
	| postfix_expression ROUNDBRACKETOPEN argument_expression_list ROUNDBRACKETCLOSE       {printf("postfix_expression: postfix_expression ROUNDBRACKETOPEN argument_expression_list ROUNDBRACKETCLOSE\n");}
	| postfix_expression DOT IDENTIFIER       {printf("postfix_expression: postfix_expression DOT IDENTIFIER\n");}
	| postfix_expression REFERENCE IDENTIFIER   {printf("postfix_expression: postfix_expression REFERENCE IDENTIFIER\n");}
	| postfix_expression INCREMENT              
	    {
		char var[11]; 
		 gen_var(var);
		strcpy(temp_stack[temp_top++],var); 
		 fprintf(ptr,"%s = %s\n",var,$1);
		 addtoquad("=",$1," ",var);
		 fprintf(ptr,"%s = %s + 1\n",$1,$1);
		 addtoquad("+",$1,"1",$1);
		 strcpy($$,var);
		printf("postfix_expression: postfix_expression INCREMENT\n");
		}
	| postfix_expression DECREMENT              {printf("postfix_expression: postfix_expression DECREMENT\n");}
	;

primary_expression
	: IDENTIFIER   
		{
		   strcpy($$,yylval.name);
		   status=0;
		   for(it=0;it<u;it++)
		   {
			if(strcmp(sym_tab[it].var_name,yylval.name)==0) status=1;
		   }
		   if(status==0) {printf("Not Declared\n"); exit(0);}
		   printf("primary_expression: IDENTIFIER\n");
		}
	| constant                    {printf("primary_expression: constant\n");}
	| CONST_STRING                      {printf("primary_expression: CONST_STRING\n");}
	| ROUNDBRACKETOPEN expression ROUNDBRACKETCLOSE   
	   {
	   strcpy($$,$2);
	   printf("primary_expression: ROUNDBRACKETOPEN expression ROUNDBRACKETCLOSE\n");
	   }
	;

argument_expression_list
	: assignment_expression      {printf("argument_expression_list: assignment_expression\n");}
	| argument_expression_list COMMA assignment_expression      {printf("argument_expression_list: argument_expression_list COMMA assignment_expression\n");}
	;

constant
	: NUMBER
		{
		    
		    set_arr=0;
			strcpy(temp,yylval.name);
		    printf("constant: NUMBER\n");
		    
		}
	
	| REALNUMBER       
		{
		    strcpy(temp,yylval.name);
		    printf("constant: REALNUMBER\n");
		}

	| CONST_CHAR   
		{
		    strcpy(temp,yylval.name);
		    printf("constant: CONST_CHAR\n");
		}
        ;


%%
	#include "lex.yy.c"
	int yyerror ()
	{
        	printf ("ERROR in Line No. : %d\n",lines);
		exit(0);
	}
	int find_id(char str[])
	{
		int i;		
		for(i=0;i<u;i++)
		{
		    if(strcmp(str,sym_tab[i].var_name)==0)
			return 1;
		}
		return 0;
	}
	 void gen_var(char var[])
	{
		static int c=1;
		int i;
		strcpy(var,"T0000000000");
		int cop=c;
		for(i=10;i>=1;i--)
		{
		    var[i]=(char)(cop%10 + 48);
		    cop=cop/10;
		}
		c++;
	}
	/*void gen_lin(char lin[])
	{
		static int c1=1;
		int i;
		strcpy(lin,"L0000000");
		int cop=c1;
		for(i=7;i>=1;i--)
		{
		    lin[i]=(char)(cop%10 + 48);
		    cop=cop/10;
		}
		c1++;
	}*/
	int toNum(char s[])
	{
	    int i,l=strlen(s);
	    int nn=0;
	    for(i=0;i<l;i++) nn=nn*10+(s[i]-'0');
	    return nn;
	}
	void push(char item[])
	{
	    stktop++;
	    strcpy(label_stack[stktop],item);
	}
	void pop()
	{
	    strcpy(v,label_stack[stktop--]);
	}
	int main(int argc, char *argv[])
	{
		
		yyin = fopen(argv[1], "r");
		
		ptr = fopen("out_inter.i", "w");
		yyparse();
		int cont=0,j,r,l,rev,cntr,num,i,len;
		int dig[100];
		char str[110],s1[110],s3[110],s2[1];
		for(it=0;it<u;it++)
		{
		    cntr=0;
			if(!strcmp(sym_tab[it].var_size,""))
			{
			   strcpy(s1,"");strcpy(s2,"");
			   cont=0;
			   strcpy(str,sym_tab[it].var_initval);
			   for(j=0;j<strlen(str);j++)
			   {
			      if(str[j]==' ')
				  cont++;
			   }
			   printf("cont = %d\n",cont);
		       while(cont>0)
		       {
		           r=cont%10;
				   dig[cntr++]=r;
			       cont=cont/10;
		       }
			   strcpy(s3,"");
			   for(j=cntr-1;j>=0;j--) 
			   {
			       switch(dig[j])
				   {
				       case 0: strcat(s3,"0"); break;
					   case 1: strcat(s3,"1"); break;
					   case 2: strcat(s3,"2"); break;
					   case 3: strcat(s3,"3"); break;
					   case 4: strcat(s3,"4"); break;
					   case 5: strcat(s3,"5"); break;
					   case 6: strcat(s3,"6"); break;
					   case 7: strcat(s3,"7"); break;
					   case 8: strcat(s3,"8"); break;
					   case 9: strcat(s3,"9"); break;
				   }
			   }
			   strcpy(sym_tab[it].var_size,s3);
			}
		}
		printf("\n\n\nNumber of Symbols in the Symbol Table is %d\n",u);
		printf("\n\nCONVENTION USED\n\n");
		printf("Group 1: int = 1   float = 2  char = 3   double = 4\n");
		printf("Group 2: signed = 10   unsigned = 40\n");
		printf("Group 3: long = 100   short = 300\n"); 

		printf("___________________________________________________________________________________________________\n");
		printf("\t\t\t\tSYMBOL TABLE\n");
		printf("___________________________________________________________________________________________________\n");
		printf("Variable\tType\t  Size\t Initval\tIs Array\tdimlen\n");
		printf("___________________________________________________________________________________________________\n");
		//for(it=0;it<u;it++) if(sym_tab[it].var_size==0) sym_tab[it].var_size=-1;
		for(it=0;it<u;it++) if(strcmp(sym_tab[it].var_name,"main")==0) sym_tab[it].var_type=0;		
		for(it=0;it<u;it++) if(strcmp(sym_tab[it].var_initval,"")==0) strcpy(sym_tab[it].var_initval,"---");  
		for(it=0;it<u;it++) printf("%8s   %8d  %8s  %8s  %8d %8d\n",sym_tab[it].var_name,sym_tab[it].var_type,sym_tab[it].var_size,sym_tab[it].var_initval,sym_tab[it].isarray,sym_tab[it].dimlen);
		printf("\n");


		/*for(it=0;it<u;it++)
		{   sym_tab[it].dimlen=0; len=strlen(sym_tab[it].var_size);
		    if(sym_tab[it].isarray==1 && len>0)
		    { num=0;
 			for(i=0;i<len;i++)
			{
			    if(sym_tab[it].var_size[i]!=' ')
			    {r=sym_tab[it].var_size[i]-'0';
			    num=num*10+r;}
			    else
			    {
				sym_tab[it].dimen[sym_tab[it].dimlen]=num;
				num=0;
				sym_tab[it].dimlen++;
			    }
			}
		    }
    
		}*/
		
		printf("\n\n\n\n");
                for(it=0;it<u;it++)
		{   len=sym_tab[it].dimlen;
		    printf("%d\n",len);
		    if(len>0)
		    {
 			for(i=0;i<len;i++)
			{
			    printf("%d ",sym_tab[it].dimen[i]);
			}
			printf("\n\n");
		    }
    
		}
		generate_code();
		for(i=0;i<cnt;i++)
		fprintf(ptr,"%s %s %s %s\n",qua[i].opcode,qua[i].op1,qua[i].op2,qua[i].result);
		return 0;
	}
	
	 void addtoquad(char* opcode, char* op1, char* op2,char* result)
	 {
	 strcpy(qua[cnt].opcode,opcode);
	 strcpy(qua[cnt].op1,op1);
	 strcpy(qua[cnt].op2,op2);
	 strcpy(qua[cnt].result,result);
	 cnt++;
	 printf("\n\n\n");
	 }
	 /* Assignment 4: Code Generation */
	 void generate_code()
	 {
	     fp2=fopen("out_assem.s","w");
		 fprintf(fp2,".text\n.globl _main\n_main:\npushl	%%ebp\nmovl	%%esp, %%ebp\nandl	$-16, %%esp\n");
		 int i;
		 for(i=0;i<cnt;i++)
		 {
		     if(strcmp(qua[i].opcode,"=")==0)
			 {
			         if(qua[i].op1[0]>=48 && qua[i].op1[0]<58)
				 {
				     if(convert(qua[i].result)==1)
					 {
					     fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op1);
                         		     fprintf(fp2,"movl %s, %%eax\n",la);
                         		     fprintf(fp2,"movl %%ebx, %s(,%%eax,1)\n",fr);
					 }
					 else
					 {
					     fprintf(fp2,"movl $%s, %s\n",qua[i].op1,qua[i].result);
					 }
				 }
				 else if(convert(qua[i].op1)==1)
				 {
					fprintf(fp2,"movl %s, %%eax\n",la);
					fprintf(fp2,"movl %s(,%%eax,1), %%eax\n",fr);
					fprintf(fp2,"movl %%eax, %s\n",qua[i].result);
				 }
				 else if(convert(qua[i].result)==1)
				 {
					fprintf(fp2,"movl %s, %%ebx\n",qua[i].op1);
					fprintf(fp2,"movl %s, %%eax\n",la);
					fprintf(fp2,"movl %%ebx, %s(,%%eax,1)\n",fr);
				 }
				 else
				 {
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl %%eax, %s\n",qua[i].result);
				 }
			 }
			 
			 else if(strcmp(qua[i].opcode,"+")==0)
			 {
					if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58)
					{
						fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					}
					else
					{
						fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					}
					if(qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
					{
						fprintf(fp2,"addl $%s, %%eax\n",qua[i].op2);
					}
					else
					{
						fprintf(fp2,"addl %s, %%eax\n",qua[i].op2);
					}
					fprintf(fp2,"movl %%eax, %s\n",qua[i].result);
			}
			
			else if(strcmp(qua[i].opcode,"-")==0)
			{
				if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
				}
				else
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
				}
				if(qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"subl $%s, %%eax\n",qua[i].op2);
				}
				else
				{
					fprintf(fp2,"subl %s, %%eax\n",qua[i].op2);
				}
				fprintf(fp2,"movl %%eax, %s\n",qua[i].result);
			}
			
			else if(strcmp(qua[i].opcode,"*")==0)
			{
					if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58)
					{
						fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					}
					else
					{
						fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					}
					if(qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
					{
						fprintf(fp2,"movl $%s, %%ecx\n",qua[i].op2);
						fprintf(fp2,"mull %%ecx\n");
					}
					else
					{
						fprintf(fp2,"movl %s, %%ecx\n",qua[i].op2);
						fprintf(fp2,"mull %%ecx\n",qua[i].op2);
					}
					fprintf(fp2,"movl %%eax, %s\n",qua[i].result);
			}
			
			else if(strcmp(qua[i].opcode,"/")==0)
			{
				if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
				}
				else
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
				}
				if(qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%ecx\n",qua[i].op2);
					fprintf(fp2,"divl %%ecx\n");
				}
				else
				{
					fprintf(fp2,"movl %s, %%ecx\n",qua[i].op2);
					fprintf(fp2,"divl %%ecx\n",qua[i].op2);
				}
				fprintf(fp2,"movl %%eax, %s\n",qua[i].result);
			}
			
			else if(strcmp(qua[i].opcode,"&&")==0)
			{
				if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
				}
				else
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
				}
				if(qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"andl $%s, %%eax\n",qua[i].op2);
				}
				else
				{
					fprintf(fp2,"andl %s, %%eax\n",qua[i].op2);
				}
				fprintf(fp2,"movl %%eax, %s\n",qua[i].result);
			}
			
			else if(strcmp(qua[i].opcode,"||")==0)
			{
				if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
				}
				else
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
				}
				if(qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"orl $%s, %%eax\n",qua[i].op2);
				}
				else
				{
					fprintf(fp2,"orl %s, %%eax\n",qua[i].op2);
				}
				fprintf(fp2,"movl %%eax, %s\n",qua[i].result);
			}
			
			else if(strcmp(qua[i].opcode,"<")==0)
			{
				if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58 && qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op2);
					fprintf(fp2,"cmpl %%ebx, %%eax\n");
				}
				else if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					fprintf(fp2,"cmpl %s, %eax\n",qua[i].op2);
				}
				else if(qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op2);
					fprintf(fp2,"cmpl %%ebx, %%eax\n");
				}
				else 
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"cmpl %s, %%eax\n",qua[i].op2);
				}
				generate_temp2();
				fprintf(fp2,"JL %s\n",lass);
				strcpy(l1,lass);
				fprintf(fp2,"movl $0,%s\n",qua[i].result);
				generate_temp2();
				strcpy(l2,lass);
				fprintf(fp2,"JMP %s\n",lass);
				fprintf(fp2,"%s: movl $1,%s\n",l1,qua[i].result);
				fprintf(fp2,"%s: ",l2);
			}
			else if(strcmp(qua[i].opcode,">")==0)
			{
				if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58 && qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op2);
					fprintf(fp2,"cmpl %%ebx, %%eax\n");
				}
				else if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					fprintf(fp2,"cmpl %s, %%eax\n",qua[i].op2);
				}
				else if(qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op2);
					fprintf(fp2,"cmpl %%ebx, %%eax\n");
				}
				else 
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"cmpl %s, %%eax\n",qua[i].op2);
				}
				generate_temp2();
				fprintf(fp2,"JG %s\n",lass);
				strcpy(l1,lass);
				fprintf(fp2,"movl $0,%s\n",qua[i].result);
				generate_temp2();
				strcpy(l2,lass);
				fprintf(fp2,"JMP %s\n",lass);
				fprintf(fp2,"%s: movl $1,%s\n",l1,qua[i].result);
				fprintf(fp2,"%s: ",l2);
			}
			else if(strcmp(qua[i].opcode,"<=")==0)
			{
				if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58 && qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op2);
					fprintf(fp2,"cmpl %%ebx, %%eax\n");
				}
				else if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					fprintf(fp2,"cmpl %s, %%eax\n",qua[i].op2);
				}
				else if(qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op2);
					fprintf(fp2,"cmpl %%ebx, %%eax\n");
				}
				else 
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"cmpl %s, %%eax\n",qua[i].op2);
				}
				generate_temp2();
				fprintf(fp2,"JLE %s\n",lass);
				strcpy(l1,lass);
				fprintf(fp2,"movl $0,%s\n",qua[i].result);
				generate_temp2();
				strcpy(l2,lass);
				fprintf(fp2,"JMP %s\n",lass);
				fprintf(fp2,"%s: movl $1,%s\n",l1,qua[i].result);
				fprintf(fp2,"%s: ",l2);
			}
			else if(strcmp(qua[i].opcode,">=")==0)
			{
				if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58 && qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op2);
					fprintf(fp2,"cmpl %%ebx, %%eax\n");
				}
				else if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					fprintf(fp2,"cmpl %s, %eax\n",qua[i].op2);
				}
				else if(qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op2);
					fprintf(fp2,"cmpl %%ebx, %%eax\n");
				}
				else 
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"cmpl %s, %%eax\n",qua[i].op2);
				}
				generate_temp2();
				fprintf(fp2,"JGE %s\n",lass);
				strcpy(l1,lass);
				fprintf(fp2,"movl $0,%s\n",qua[i].result);
				generate_temp2();
				strcpy(l2,lass);
				fprintf(fp2,"JMP %s\n",lass);
				fprintf(fp2,"%s: movl $1,%s\n",l1,qua[i].result);
				fprintf(fp2,"%s: ",l2);
			}
			else if(strcmp(qua[i].opcode,"==")==0)
			{
				if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58 && qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op2);
					fprintf(fp2,"cmpl %%eax, %%ebx\n");
				}
				else if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					fprintf(fp2,"cmpl %%eax, %s\n",qua[i].op2);
				}
				else if(qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op2);
					fprintf(fp2,"cmpl %%eax, %%ebx\n");
				}
				else 
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"cmpl %%eax, %s\n",qua[i].op2);
				}
				generate_temp2();
				fprintf(fp2,"JE %s\n",lass);
				strcpy(l1,lass);
				fprintf(fp2,"movl $0,%s\n",qua[i].result);
				generate_temp2();
				strcpy(l2,lass);
				fprintf(fp2,"JMP %s\n",lass);
				fprintf(fp2,"%s: movl $1,%s\n",l1,qua[i].result);
				fprintf(fp2,"%s: ",l2);
			}

			else if(strcmp(qua[i].opcode,"!=")==0)
			{
				if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58 && qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op2);
					fprintf(fp2,"cmpl %%eax, %%ebx\n");
				}
				else if(qua[i].op1[0]>=48 && qua[i].op1[0]<=58)
				{
					fprintf(fp2,"movl $%s, %%eax\n",qua[i].op1);
					fprintf(fp2,"cmpl %%eax, %s\n",qua[i].op2);
				}
				else if(qua[i].op2[0]>=48 && qua[i].op2[0]<=58)
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"movl $%s, %%ebx\n",qua[i].op2);
					fprintf(fp2,"cmpl %%eax, %%ebx\n");
				}
				else 
				{
					fprintf(fp2,"movl %s, %%eax\n",qua[i].op1);
					fprintf(fp2,"cmpl %%eax, %s\n",qua[i].op2);
				}
				generate_temp2();
				fprintf(fp2,"JNE %s\n",lass);
				strcpy(l1,lass);
				fprintf(fp2,"movl $0,%s\n",qua[i].result);
				generate_temp2();
				strcpy(l2,lass);
				fprintf(fp2,"JMP %s\n",lass);
				fprintf(fp2,"%s: movl $1,%s\n",l1,qua[i].result);
				fprintf(fp2,"%s: ",l2);
			}

			else if(strcmp(qua[i].opcode,"IF")==0)
			{
				fprintf(fp2,"movl %s,%%eax\n",qua[i].op1);
				fprintf(fp2,"movl $0,%%ebx\n");
				fprintf(fp2,"cmpl %%eax, %%ebx\n");
				fprintf(fp2,"JZ %s\n",qua[i].op2);
			}

			else if(strcmp(qua[i].opcode,":")==0)
			{
				fprintf(fp2,"%s:",qua[i].op1);
			}

			else if(strcmp(qua[i].opcode,"goto")==0)
			{
				fprintf(fp2,"JMP %s\n",qua[i].op1);
			}
		}
		fprintf(fp2,"movl	result, %%eax\nmovl	%%eax, 4(%%esp)\nmovl	$printtext1, %%eax\nmovl	%%eax, (%%esp)\ncall	_printf\nmovl	size, %%eax\nmovl	%%eax, 4(%%esp)\nmovl	$printtext2, %%eax\nmovl	%%eax, (%%esp)\ncall	_printf\nmovl	$printtext3, %%eax\nmovl	%%eax, (%%esp)\nmovl	$0, %%ecx\nprint_a:\ncmpl	%%ecx, size\njz 	exit\nmovl	a(,%%ecx,4), %%eax\nmovl	%%eax, 4(%%esp)\nmovl	%%ecx, temp_count\ncall	_printf\nmovl	temp_count, %%ecx\naddl	$1, %%ecx\njmp 	print_a\nexit:\n");
		fprintf(fp2,"movl %%ebp, %%esp\npopl %%ebp\nret\n");
		fprintf(fp2,".data\nprinttext1: .ascii \"result = %%d\\n\\0\"\nprinttext2: .ascii \"size = %%d\\n\\0\"\nprinttext3: .ascii \"%%d, \\0\"\ntemp_count: .long 0\n");
		
		for(i=0;i<u;i++)
		{
			if(strcmp(sym_tab[i].var_initval,"---")==0)
			fprintf(fp2,"%s: .long 0\n",sym_tab[i].var_name);
			else
			{
				int lll=strlen(sym_tab[i].var_initval);	
				fprintf(fp2,"%s: .long ",sym_tab[i].var_name);
				int j=0,charcnt=0;
				for(j=0;j<lll;j++)
				{
				    if(sym_tab[i].var_initval[j]!=' ') {fprintf(fp2,"%c",sym_tab[i].var_initval[j]); charcnt++;}
				    else if(j!=(lll-1) && charcnt!=0) fprintf(fp2,","); 
				}
				fprintf(fp2,"\n");
			}
		}
		
		for(i=0;i<temp_top;i++)
		{
		    fprintf(fp2,"%s: .long 0\n",temp_stack[i]);
		}
		
	 }
	 
	 int convert(char *str)
	 {
	    int i,l=strlen(str),ind1=-1,ind2=-1;
		for(i=0;i<l;i++)
		{
		    if(str[i]=='[') ind1=i;
			else if(str[i]==']') ind2=i;
		}
		if(ind1==-1) return 0;
		for(i=0;i<ind1;i++) fr[i]=str[i];
		for(i=ind1+1;i<ind2;i++) la[i-ind1-1]=str[i];
		return 1;
	 }
	 void generate_temp2()
	{
		static int cnt2=1;
		int i=5;
		int no=cnt2;
		while(no!=0)
		{
			int rem=no%10;
			lass[i]=rem+'0';
			i--;
			no=no/10;
		}
		cnt2++;
	}
