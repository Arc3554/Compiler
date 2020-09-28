/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;
	int Address=0;
    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;
    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }
	struct symbol_table
	{
	int index[100];
	char* name[100];
	char* type[100];
	int address[100];
	int lineno[100];
	char* element_type[100];
	int num;
	};

	int scope;
	int lineno;
	int indexx;
	int deep;
	char* tp;
	char* tp2;
	int pt=0;
	int err=0;
	int nn=0;
	int where = -1;
	int HAS_ERROR = 0;
	int cmpnum=0;
	int left=-1;
	int fornum=0;
	int cmprem[20];
	int forrem[20];
	int ifnum=0;
	int right[20];
	int where2[20];

	FILE *fp;

	struct symbol_table st[20];

    /* Symbol table function - you can add new function if needed. */
    static void insert_symbol(char*,char*);
	static void insert_symbol_a(char*,char*,char*);
    static int lookup_symbol(char*);
    static void dump_symbol();
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    /* ... */
}

/* Token without return */
%token VAR INC DEC
%token TRUE FALSE
%token INT FLOAT BOOL STRING
%token NEWLINE LAND LOR
%token GEQ LEQ EQL NEQ
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token PRINT PRINTLN IF ELSE FOR
/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT ID

/* Nonterminal with return, which need to sepcify type */
%type <s_val> Type Expression Literal PrimaryExpr UnaryExpr leftExpression

/* Yacc will start at this nonterminal */
%start Program

%right '='
%left LOR
%left LAND
%left '<' '>' GEQ LEQ EQL NEQ
%left '+''-'
%left '*''/' '%'
%left '!'


/* Grammar section */
%%

Program
    : StatementList
;

StatementList:
	 Statement StatementList
	 | Statement
;

Newline:
	NEWLINE {lineno++;pt=0;err=0;nn=0;}
;
Statement
	: DeclarationStmt Newline
	| SimpleStmt Newline
	| Block Newline
	| IfStmt {fprintf(fp,"ifscope%d:\n",ifnum);ifnum++;} Newline
	| {fprintf(fp,"L_for_begin%d:\n",fornum);} ForStmt Newline
	| PrintStmt Newline {pt=1;}
	| Newline
;

ForStmt:
	FOR Expression ForBlock
	|FOR SimpleStmt';' {fprintf(fp,"L_for_begin%d:\n",fornum);} Condition';'RightStmt ForBlock2
;

RightStmt:
	ID INC {printf("INC\n");where2[scope] = lookup_symbol($1);if(strcmp(tp2,"int32")==0){right[scope]=1;}else if(strcmp(tp2,"float32")==0){right[scope]=2;}}
	|ID DEC {printf("DEC\n");where2[scope] = lookup_symbol($1);if(strcmp(tp2,"int32")==0){right[scope]=3;}else if(strcmp(tp2,"float32")==0){right[scope]=4;}}
;

ForBlock2:
	'{' {forrem[scope]=fornum;cmprem[scope]=cmpnum;scope++;fprintf(fp,"ifeq L_cmp_%d\n",cmpnum);cmpnum++;fornum++;} StatementList 
	
	{
		if(right[scope-1]==1){fprintf(fp,"iload %d\nldc 1\niadd\nistore %d\n",where2[scope-1],where2[scope-1]);}
		else if(right[scope-1]==2){fprintf(fp,"fload %d\nldc 1.000000\nfadd\nfstore %d\n",where2[scope-1],where2[scope-1]);}
		else if(right[scope-1]==3){fprintf(fp,"iload %d\nldc 1\nisub\nistore %d\n",where2[scope-1],where2[scope-1]);}
		else if(right[scope-1]==4){fprintf(fp,"fload %d\nldc 1.000000\nfsub\nfstore %d\n",where2[scope-1],where2[scope-1]);}
	}
	'}'{dump_symbol();fprintf(fp,"goto L_for_begin%d\nL_cmp_%d:\n",forrem[scope],cmprem[scope]);}
;

Condition:
	Expression {if(strcmp($1,"bool")!=0){printf("error:%d: non-bool (type %s) used as for condition\n",yylineno+1,$1);HAS_ERROR=1;}}
;


IfStmt:
	IF Condition IfBlock
	|IF Condition IfBlock ELSE IfStmt
	|IF Condition IfBlock ELSE Block
;

PrintStmt
	: PRINT '(' Expression ')' {
	printf("PRINT ");	 
	printf("%s\n",$3);
	if(strcmp($3,"int32")==0){fprintf(fp,"getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/print(I)V\n");}
	else if(strcmp($3,"float32")==0){fprintf(fp,"getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/print(F)V\n");}
	else if(strcmp($3,"bool")==0){fprintf(fp,"ifne L_cmp_%d\nldc \"false\"\ngoto L_cmp_%d\nL_cmp_%d:\nldc \"true\"\nL_cmp_%d:\ngetstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);cmpnum+=2;}
	else if(strcmp($3,"string")==0){fprintf(fp,"getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");}
	else if(strcmp($3,"array")==0){fprintf(fp,"iload %d\n",where);}
	}
	| PRINTLN '(' Expression ')' 
	{
	printf("PRINTLN ");
	printf("%s\n",$3);
	if(strcmp($3,"int32")==0){fprintf(fp,"getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/println(I)V\n");}
	else if(strcmp($3,"float32")==0){fprintf(fp,"getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/println(F)V\n");}
	else if(strcmp($3,"bool")==0){fprintf(fp,"ifne L_cmp_%d\nldc \"false\"\ngoto L_cmp_%d\nL_cmp_%d:\nldc \"true\"\nL_cmp_%d:\ngetstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);cmpnum+=2;}
	else if(strcmp($3,"string")==0){fprintf(fp,"getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\ninvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");}
	else if(strcmp($3,"array")==0){fprintf(fp,"iload %d\n",where);}
	}
;

DeclarationStmt :
	 VAR ID Type {
	 where = lookup_symbol($2);
	 if(scope==deep&&where>=0){printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",yylineno,$2,st[scope].lineno[indexx]);HAS_ERROR=1;}
	 else{
	 if(strcmp($3,"int32")==0){insert_symbol($2,"int32");where = lookup_symbol($2);fprintf(fp,"ldc 0\nistore %d\n",where);}
	 else if(strcmp($3,"float32")==0){insert_symbol($2,"float32");where = lookup_symbol($2);fprintf(fp,"ldc 0.000000\nfstore %d\n",where);}
	 else if(strcmp($3 ,"bool")==0){insert_symbol($2,"bool");where = lookup_symbol($2);fprintf(fp,"istore %d\n",where);}
	 else if(strcmp($3 ,"string")==0){insert_symbol($2,"string");where = lookup_symbol($2);fprintf(fp,"ldc \"\"\nastore %d\n",where);}
	 }}
	 |VAR ID Type '=' Expression {	 
	 	 where = lookup_symbol($2);
	 if(scope==deep&&where>=0){printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",yylineno,$2,st[scope].lineno[indexx]);HAS_ERROR=1;}
	 else{
	 if(strcmp($3,"int32")==0){insert_symbol($2,"int32");where = lookup_symbol($2);fprintf(fp,"istore %d\n",where);}
	 else if(strcmp($3,"float32")==0){insert_symbol($2,"float32");where = lookup_symbol($2);fprintf(fp,"fstore %d\n",where);}
	 else if(strcmp($3 ,"bool")==0){insert_symbol($2,"bool");where = lookup_symbol($2);fprintf(fp,"istore %d\n",where);}
	 else if(strcmp($3 ,"string")==0){insert_symbol($2,"string");where = lookup_symbol($2);fprintf(fp,"astore %d\n",where);}
	 }}
	 | VAR ID '[' Expression ']'Type {
	 	 where = lookup_symbol($2);
	 if(scope==deep&&where>=0){printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",yylineno,$2,st[scope].lineno[indexx]);HAS_ERROR=1;}
	 else{
	 if(strcmp($6,"int32")==0){insert_symbol_a($2,"array","int32");where = lookup_symbol($2);fprintf(fp,"newarray int\nastore %d\n",where);}
	 else if(strcmp($6,"float32")==0){insert_symbol_a($2,"array","float32");where = lookup_symbol($2);fprintf(fp,"newarray float\nastore %d\n",where);}
	 else if(strcmp($6 ,"bool")==0){insert_symbol_a($2,"array","bool");}
	 else if(strcmp($6 ,"string")==0){insert_symbol_a($2,"array","string");}
	 }}
	 |VAR ID '[' Expression ']'Type '=' Expression {	 
	 	 where = lookup_symbol($2);
	 if(scope==deep&&where>=0){printf("error:%d: %s redeclared in this block. previous declaration at line %d\n",yylineno,$2,st[scope].lineno[indexx]);HAS_ERROR=1;}
	 else{
	 if(strcmp($6,"int32")==0){insert_symbol_a($2,"array","int32");}
	 else if(strcmp($6,"float32")==0){insert_symbol_a($2,"array","float32");}
	 else if(strcmp($6 ,"bool")==0){insert_symbol_a($2,"array","bool");}
	 else if(strcmp($6 ,"string")==0){insert_symbol_a($2,"array","string");}
	 }}
;

Block:
	'{' {scope++;} StatementList '}'{dump_symbol();}
;

IfBlock:
	'{' {cmprem[scope]=cmpnum;scope++;fprintf(fp,"ifeq L_cmp_%d\n",cmpnum);cmpnum++;} StatementList {fprintf(fp,"goto ifscope%d\n",ifnum);} '}'{dump_symbol();fprintf(fp,"L_cmp_%d:\n",cmprem[scope]);}
;

ForBlock:
	'{' {forrem[scope]=fornum;cmprem[scope]=cmpnum;scope++;fprintf(fp,"ifeq L_cmp_%d\n",cmpnum);cmpnum++;fornum++;} StatementList '}'{dump_symbol();fprintf(fp,"goto L_for_begin%d\nL_cmp_%d:\n",forrem[scope],cmprem[scope]);}
;

Type :
	INT {$$ = "int32";}
	|FLOAT {$$ = "float32";}
	|BOOL {$$ = "bool";}
	|STRING {$$ = "string";}
;


SimpleStmt 
	 : AssignmentStmt
	 | ExpressionStmt
	 | IncDecStmt
;

AssignmentStmt:
	leftExpression '=' Expression {
	if(strcmp($1,$3)!=0&&!(strcmp($1,"float32")==0&&strcmp($3,"int32")==0)&&yylineno!=err){printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n",yylineno,$1,$3);HAS_ERROR=1;}
	if(nn==1){printf("error:%d: cannot assign to %s\n",yylineno,$1);HAS_ERROR=1;}
	printf("ASSIGN\n");	
	if(strcmp(tp2,"int32")==0){fprintf(fp,"istore %d\n",left);}
	else if(strcmp(tp2,"float32")==0){fprintf(fp,"fstore %d\n",left);}
	else if(strcmp(tp2,"bool")==0){fprintf(fp,"istore %d\n",left);}
	else if(strcmp(tp2,"string")==0){fprintf(fp,"astore %d\n",left);}
	else if(strcmp(tp2,"array")==0){if(strcmp(tp,"int32")==0){fprintf(fp,"iastore\n");}else if(strcmp(tp,"float32")==0){fprintf(fp,"fastore\n");}}
	}
	|leftExpression ADD_ASSIGN Expression {	if(strcmp($1,$3)!=0&&!(strcmp($1,"float32")==0&&strcmp($3,"int32")==0)&&yylineno!=err){HAS_ERROR=1;printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n",yylineno,$1,$3);}
	if(nn==1){printf("error:%d: cannot assign to %s\n",yylineno,$1);HAS_ERROR=1;}
	printf("ADD_ASSIGN\n");
	if(strcmp(tp2,"int32")==0){fprintf(fp,"iload %d\nswap\niadd\nistore %d\n",left,left);}
	else if(strcmp(tp2,"float32")==0){fprintf(fp,"fload %d\nswap\nfadd\nfstore %d\n",left,left);}
	//else if(strcmp(tp2,"bool")==0){fprintf(fp,"istore %d\n",where);}
	//else if(strcmp(tp2,"string")==0){fprintf(fp,"astore %d\n",where);}
	//else if(strcmp(tp2,"array")==0){if(strcmp(tp,"int32")==0){fprintf(fp,"iastore\n");}else if(strcmp(tp,"float32")==0){fprintf(fp,"fastore\n");}}
	}
	|leftExpression SUB_ASSIGN Expression {	if(strcmp($1,$3)!=0&&!(strcmp($1,"float32")==0&&strcmp($3,"int32")==0)&&yylineno!=err){HAS_ERROR=1;printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n",yylineno,$1,$3);}
	if(nn==1){printf("error:%d: cannot assign to %s\n",yylineno,$1);HAS_ERROR=1;}
	printf("SUB_ASSIGN\n");
	if(strcmp(tp2,"int32")==0){fprintf(fp,"iload %d\nswap\nisub\nistore %d\n",left,left);}
	else if(strcmp(tp2,"float32")==0){fprintf(fp,"fload %d\nswap\nfsub\nfstore %d\n",left,left);}
	//else if(strcmp(tp2,"bool")==0){fprintf(fp,"istore %d\n",where);}
	//else if(strcmp(tp2,"string")==0){fprintf(fp,"astore %d\n",where);}
	//else if(strcmp(tp2,"array")==0){if(strcmp(tp,"int32")==0){fprintf(fp,"iastore\n");}else if(strcmp(tp,"float32")==0){fprintf(fp,"fastore\n");}}
	}
	|leftExpression MUL_ASSIGN Expression {	if(strcmp($1,$3)!=0&&!(strcmp($1,"float32")==0&&strcmp($3,"int32")==0)&&yylineno!=err){HAS_ERROR=1;printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n",yylineno,$1,$3);}
	if(nn==1){printf("error:%d: cannot assign to %s\n",yylineno,$1);HAS_ERROR=1;}
	printf("MUL_ASSIGN\n");
	if(strcmp(tp2,"int32")==0){fprintf(fp,"iload %d\nswap\nimul\nistore %d\n",left,left);}
	else if(strcmp(tp2,"float32")==0){fprintf(fp,"fload %d\nswap\nfmul\nfstore %d\n",left,left);}
	//else if(strcmp(tp2,"bool")==0){fprintf(fp,"istore %d\n",where);}
	//else if(strcmp(tp2,"string")==0){fprintf(fp,"astore %d\n",where);}
	//else if(strcmp(tp2,"array")==0){if(strcmp(tp,"int32")==0){fprintf(fp,"iastore\n");}else if(strcmp(tp,"float32")==0){fprintf(fp,"fastore\n");}}
	}
	|leftExpression QUO_ASSIGN Expression {	if(strcmp($1,$3)!=0&&!(strcmp($1,"float32")==0&&strcmp($3,"int32")==0)&&yylineno!=err){HAS_ERROR=1;printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n",yylineno,$1,$3);}
	if(nn==1){printf("error:%d: cannot assign to %s\n",yylineno,$1);HAS_ERROR=1;}
	printf("QUO_ASSIGN\n");
	if(strcmp(tp2,"int32")==0){fprintf(fp,"iload %d\nswap\nidiv\nistore %d\n",left,left);}
	else if(strcmp(tp2,"float32")==0){fprintf(fp,"fload %d\nswap\nfdiv\nfstore %d\n",left,left);}
	//else if(strcmp(tp2,"bool")==0){fprintf(fp,"istore %d\n",where);}
	//else if(strcmp(tp2,"string")==0){fprintf(fp,"astore %d\n",where);}
	//else if(strcmp(tp2,"array")==0){if(strcmp(tp,"int32")==0){fprintf(fp,"iastore\n");}else if(strcmp(tp,"float32")==0){fprintf(fp,"fastore\n");}}
	}
	|leftExpression REM_ASSIGN Expression {	if(strcmp($1,$3)!=0&&!(strcmp($1,"float32")==0&&strcmp($3,"int32")==0)&&yylineno!=err){HAS_ERROR=1;printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n",yylineno,$1,$3);}
	if(nn==1){printf("error:%d: cannot assign to %s\n",yylineno,$1);HAS_ERROR=1;}
	printf("REM_ASSIGN\n");
	if(strcmp(tp2,"int32")==0){fprintf(fp,"iload %d\nswap\nirem\nistore %d\n",left,left);}
	}

;

ExpressionStmt :
	Expression
;

leftExpression:
	Literal {nn=1;}
	|ID {where = lookup_symbol($1); 
	if(where>=0){printf("IDENT (name=%s, address=%d)\n",$1,where);$$=tp;}
	else{printf("error:%d: undefined: %s\n",yylineno+1,$1);err=yylineno+1;HAS_ERROR=1;}
	left=where;
	}
	|PrimaryExpr '[' Expression ']' 
	|ConversionExpr
	|'+' leftExpression {$$=$2;printf("POS\n");}
	| '-' leftExpression {$$=$2;printf("NEG\n");}
	| '!' leftExpression {$$=$2;printf("NOT\n");}
;


Expression:
	UnaryExpr {$$=$1;}
	| Expression LOR Expression {
	if(strcmp($1,"bool")!=0&&pt==0){printf("error:%d: invalid operation: (operator LOR not defined on %s)\n",yylineno,$1);HAS_ERROR=1;}
	if(strcmp($3,"bool")!=0&&pt==0){printf("error:%d: invalid operation: (operator LOR not defined on %s)\n",yylineno,$3);HAS_ERROR=1;} 
	printf("LOR\n");$$="bool";fprintf(fp,"ior\n");}
	| Expression LAND Expression {
	if(strcmp($1,"bool")!=0&&pt==0){printf("error:%d: invalid operation: (operator LAND not defined on %s)\n",yylineno,$1);HAS_ERROR=1;}
	if(strcmp($3,"bool")!=0&&pt==0){printf("error:%d: invalid operation: (operator LAND not defined on %s)\n",yylineno,$3);HAS_ERROR=1;}
	printf("LAND\n");$$="bool";fprintf(fp,"iand\n");}
	| Expression EQL Expression {printf("EQL\n");$$="bool";
	if(strcmp($3,"int32")==0){fprintf(fp,"isub\nifeq L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);}
	else if(strcmp($3,"float32")==0){fprintf(fp,"fcmpl\nifeq L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);}cmpnum+=2;
	}
	| Expression NEQ Expression {printf("NEQ\n");$$="bool";
	if(strcmp($3,"int32")==0){fprintf(fp,"isub\nifne L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);}
	else if(strcmp($3,"float32")==0){fprintf(fp,"fcmpl\nifne L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);}cmpnum+=2;
	}
	| Expression '<' Expression {printf("LSS\n");$$="bool";
	if(strcmp($3,"int32")==0){fprintf(fp,"isub\niflt L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);}
	else if(strcmp($3,"float32")==0){fprintf(fp,"fcmpl\niflt L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);}cmpnum+=2;
	}
	| Expression LEQ Expression {printf("LEQ\n");$$="bool";
	if(strcmp($3,"int32")==0){fprintf(fp,"isub\nifle L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);}
	else if(strcmp($3,"float32")==0){fprintf(fp,"fcmpl\nifle L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);}cmpnum+=2;
	}
	| Expression '>' Expression {printf("GTR\n");$$="bool";
	if(strcmp($3,"int32")==0){fprintf(fp,"isub\nifgt L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);}
	else if(strcmp($3,"float32")==0){fprintf(fp,"fcmpl\nifgt L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);}cmpnum+=2;
	}
	| Expression GEQ Expression {printf("GEQ\n");$$="bool";
	if(strcmp($3,"int32")==0){fprintf(fp,"isub\nifge L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);}
	else if(strcmp($3,"float32")==0){fprintf(fp,"fcmpl\nifge L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",cmpnum,cmpnum+1,cmpnum,cmpnum+1);}cmpnum+=2;
	}
	| Expression '+' Expression {if(strcmp($1,$3)!=0){HAS_ERROR=1;printf("error:%d: invalid operation: ADD (mismatched types %s and %s)\n",yylineno,$1,$3);}printf("ADD\n");$$=$1;
	if(strcmp($3,"int32")==0){fprintf(fp,"iadd\n");}
	else if(strcmp($3,"float32")==0){fprintf(fp,"fadd\n");}
	//else if(strcmp($3,"bool")==0){fprintf(fp,"iload %d\n",where);}
	//else if(strcmp($3,"string")==0){fprintf(fp,"iload %d\n",where);}
	//else if(strcmp($3,"array")==0){fprintf(fp,"iload %d\n",where);}
	}
	| Expression '-' Expression {if(strcmp($1,$3)!=0){HAS_ERROR=1;printf("error:%d: invalid operation: SUB (mismatched types %s and %s)\n",yylineno,$1,$3);}printf("SUB\n");$$=$1;
	if(strcmp($3,"int32")==0){fprintf(fp,"isub\n");}
	else if(strcmp($3,"float32")==0){fprintf(fp,"fsub\n");}
	//else if(strcmp($3,"bool")==0){fprintf(fp,"iload %d\n",where);}
	//else if(strcmp($3,"string")==0){fprintf(fp,"iload %d\n",where);}
	//else if(strcmp($3,"array")==0){fprintf(fp,"iload %d\n",where);}
	}
	| Expression '*' Expression {if(strcmp($1,$3)!=0){HAS_ERROR=1;printf("error:%d: invalid operation: MUL (mismatched types %s and %s)\n",yylineno,$1,$3);}printf("MUL\n");$$=$1;
	if(strcmp($3,"int32")==0){fprintf(fp,"imul\n");}
	else if(strcmp($3,"float32")==0){fprintf(fp,"fmul\n");}
	//else if(strcmp($3,"bool")==0){fprintf(fp,"iload %d\n",where);}
	//else if(strcmp($3,"string")==0){fprintf(fp,"iload %d\n",where);}
	//else if(strcmp($3,"array")==0){fprintf(fp,"iload %d\n",where);}
	}
	| Expression '/' Expression {if(strcmp($1,$3)!=0){HAS_ERROR=1;printf("error:%d: invalid operation: QUO (mismatched types %s and %s)\n",yylineno,$1,$3);}printf("QUO\n");$$=$1;
	if(strcmp($3,"int32")==0){fprintf(fp,"idiv\n");}
	else if(strcmp($3,"float32")==0){fprintf(fp,"fdiv\n");}
	//else if(strcmp($3,"bool")==0){fprintf(fp,"iload %d\n",where);}
	//else if(strcmp($3,"string")==0){fprintf(fp,"iload %d\n",where);}
	//else if(strcmp($3,"array")==0){fprintf(fp,"iload %d\n",where);}
	}
	| Expression '%' Expression {if(strcmp($1,"int32")!=0){HAS_ERROR=1;printf("error:%d: invalid operation: (operator REM not defined on %s)\n",yylineno,$1);fprintf(fp,"irem\n");}
	if(strcmp($3,"int32")!=0){HAS_ERROR=1;printf("error:%d: invalid operation: (operator REM not defined on %s)\n",yylineno,$3);}printf("REM\n");$$=$1;
	if(strcmp($3,"int32")==0){fprintf(fp,"irem\n");}
	
	}
	| '(' Expression ')' {$$=$2;}

;

UnaryExpr :
	PrimaryExpr {$$=$1;}
	| '+' UnaryExpr {$$=$2;printf("POS\n");}
	| '-' UnaryExpr {$$=$2;printf("NEG\n");if(strcmp($2,"int32")==0){fprintf(fp,"ineg\n");}else if(strcmp($2,"float32")==0){fprintf(fp,"fneg\n");}}
	| '!' UnaryExpr {$$=$2;printf("NOT\n");fprintf(fp,"iconst_1\nixor\n");}
;


PrimaryExpr:
	Literal {$$=$1;}
	|ID {where = lookup_symbol($1); 
	if(where>=0){printf("IDENT (name=%s, address=%d)\n",$1,where);$$=tp;
	if(strcmp(tp2,"int32")==0){fprintf(fp,"iload %d\n",where);}
	else if(strcmp(tp2,"float32")==0){fprintf(fp,"fload %d\n",where);}
	else if(strcmp(tp2,"bool")==0){fprintf(fp,"iload %d\n",where);}
	else if(strcmp(tp2,"string")==0){fprintf(fp,"aload %d\n",where);}
	else if(strcmp(tp2,"array")==0){fprintf(fp,"aload %d\n",where);}
	}
	else{HAS_ERROR=1;printf("error:%d: undefined: %s\n",yylineno+1,$1);err=yylineno+1;}
	}
	|PrimaryExpr '[' Expression ']' {if(strcmp($1,"int32")==0){fprintf(fp,"iaload\n");}else if(strcmp($1,"float32")==0){fprintf(fp,"faload\n");}}
	|ConversionExpr
;
ConversionExpr:
	Type '(' Expression ')' 
	{

	if(strcmp($3,$1)!=0)
	{
		if(strcmp($3,"int32")==0){printf("I to ");fprintf(fp,"i2");}
		else if (strcmp($3,"float32")==0){printf("F to ");fprintf(fp,"f2");}
		if(strcmp($1,"int32")==0){printf("I\n");fprintf(fp,"i\n");tp2="int32";}
		else if (strcmp($1,"float32")==0){printf("F\n");fprintf(fp,"f\n");tp2="float32";}
	}
	
	}
;

Literal:
	INT_LIT {printf("INT_LIT %d\n",$1);$$="int32";fprintf(fp,"ldc %d\n",$1);}
	| FLOAT_LIT {printf("FLOAT_LIT %f\n",$1);$$="float32";fprintf(fp,"ldc %f\n",$1);}
	|'"' STRING_LIT '"' {printf("STRING_LIT %s\n",$2);$$="string";fprintf(fp,"ldc \"%s\"\n",$2);}
	|TRUE {printf("TRUE\n");$$="bool";fprintf(fp,"iconst_1\n");}
	|FALSE {printf("FALSE\n");$$="bool";fprintf(fp,"iconst_0\n");}
;
IncDecStmt:
	Expression INC {printf("INC\n");if(strcmp(tp2,"int32")==0){fprintf(fp,"ldc 1\niadd\nistore %d\n",where);}else if(strcmp(tp2,"float32")==0){fprintf(fp,"ldc 1.000000\nfadd\nfstore %d\n",where);}}
	|Expression DEC {printf("DEC\n");if(strcmp(tp2,"int32")==0){fprintf(fp,"ldc 1\nisub\nistore %d\n",where);}else if(strcmp(tp2,"float32")==0){fprintf(fp,"ldc 1.000000\nfsub\nfstore %d\n",where);}}
;


%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }
	fp = fopen("hw3.j","w");
	fprintf(fp,".source hw3.j\n.class public Main\n.super java/lang/Object\n.method public static main([Ljava/lang/String;)V\n.limit stack 1000\n.limit locals 1000\n");

    yylineno = 0;
	scope=0;
	lineno=1;
	for(int i=0;i<20;++i){st[i].num=0;cmprem[i]=0;forrem[i]=0;for(int j=0;j<100;++j){st[i].element_type[j]="-";}}
    yyparse();
	dump_symbol();
	printf("Total lines: %d\n", yylineno);

    fclose(yyin);
	fprintf(fp,"return\n.end method");
	
    if (HAS_ERROR) {
        remove("hw3.j");
    }

    return 0;
}



static void insert_symbol(char* temp1,char* temp2) {
    printf("> Insert {%s} into symbol table (scope level: %d)\n", temp1, scope);
	st[scope].index[st[scope].num]=st[scope].num;
	st[scope].name[st[scope].num]=temp1;
	st[scope].type[st[scope].num]=temp2;
	st[scope].address[st[scope].num]=Address;
	st[scope].lineno[st[scope].num]=yylineno;
	st[scope].num++;
	Address++;
}

static void insert_symbol_a(char* temp1,char* temp2,char* temp3) {
    printf("> Insert {%s} into symbol table (scope level: %d)\n", temp1, scope);
	st[scope].index[st[scope].num]=st[scope].num;
	st[scope].name[st[scope].num]=temp1;
	st[scope].type[st[scope].num]=temp2;
	st[scope].address[st[scope].num]=Address;
	st[scope].lineno[st[scope].num]=yylineno;
	st[scope].element_type[st[scope].num]=temp3;
	st[scope].num++;
	Address++;
}

static int lookup_symbol(char* temp1) {
for(int j=scope;j>=0;--j)
{
	for(int i=0;i<st[j].num;++i)
	{
		if(strcmp(st[j].name[i],temp1)==0){
		tp=st[j].type[i];
		tp2=st[j].type[i];
		if(strcmp(tp,"array")==0){tp=st[j].element_type[i];}
		indexx=i;
		deep=j;
		return(st[j].address[i]);
		}
	}
}
	return(-1);

}

static void dump_symbol() {
    printf("> Dump symbol table (scope level: %d)\n", scope);
	printf("%-10s%-10s%-10s%-10s%-10s%s\n","Index", "Name", "Type", "Address", "Lineno", "Element type");
	for(int i = 0 ; i < st[scope].num;++i)
	{
		printf("%-10d%-10s%-10s%-10d%-10d%s\n",st[scope].index[i], st[scope].name[i], st[scope].type[i],st[scope].address[i],st[scope].lineno[i], st[scope].element_type[i]);
	}
	int i = scope;
	st[i].num=0;for(int j=0;j<100;++j){st[i].element_type[j]="-";}
	scope--;
}