grammar MiniJava;

goal : mainClass (classDeclaration)* EOF ;

mainClass : 'class' IDENTIFIER '{' 'public' 'static' io='io'? 'void' 'main' '(' 'String' '[' ']' IDENTIFIER ')' '{' statement '}' '}' ;

classDeclaration : 'class' IDENTIFIER ('extends' IDENTIFIER)? '{' (varDeclaration)* (methodDeclaration*) '}' ;

varDeclaration : type IDENTIFIER ';' ;

methodDeclaration : 'public' io='io'? type IDENTIFIER '(' (type IDENTIFIER (',' type IDENTIFIER)*)? ')' '{' (varDeclaration)* (statement)* 'return' expression ';' '}' ;

type :
      'int' '[' ']' # IntArrayType
    | 'boolean' # BooleanType
    | 'int' # IntType
    | IDENTIFIER # IdentifierType
    ;

statement :
      '{' (statement)* '}' # StatementBlock
    | 'if' '(' expression ')' statement 'else' statement # IfStatement
    | 'while' '(' expression ')' statement # WhileStatement
    | 'System.out.println' '(' expression ')' ';' # PrintStatement
    | IDENTIFIER '=' expression ';' # AssignmentStatement
    | IDENTIFIER '[' expression ']' '=' expression ';' # ArrayAssignmentStatement
    ;

expression :
      expression '[' expression ']' # ArrayAccessExpression
    | expression '.' 'length' # ArrayLengthExpression
    | objectExpression=expression '.' IDENTIFIER'(' (expression ( ',' expression )*)? ')' # MethodCallExpression
    | expression binary_operator=('&&' | '<' | '+' | '-' | '*') expression # BinaryOperationExpression
    | '-' INTEGER_LITERAL # NegatedIntegerLiteral
    | INTEGER_LITERAL # IntegerLiteral
    | 'true' # True
    | 'false' # False
    | IDENTIFIER # IdentifierExpression
    | 'this' # This
    | 'new' 'int' '[' expression ']' # NewIntArrayExpression
    | 'new' IDENTIFIER '(' ')' # NewObjectExpression
    | '!' expression # NegationExpression
    | '(' expression ')' # ParenedExpression
    ;

IDENTIFIER : [a-zA-Z_]+[a-zA-Z_0-9]* ;
INTEGER_LITERAL : [0-9]+ ;

WS: [ \n\t\r]+ -> skip;
COMMENT : '//' .+? ('\n'|EOF) -> skip ;
