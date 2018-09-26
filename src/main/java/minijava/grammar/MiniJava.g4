grammar MiniJava;

goal : mainClass (classDeclaration)* EOF ;

mainClass : 'class' IDENTIFIER '{' 'public' 'static' ('io')? 'void' 'main' '(' 'String' '[' ']' IDENTIFIER ')' '{' statement '}' '}' ;

classDeclaration : 'class' IDENTIFIER ('extends' IDENTIFIER)? '{' (varDeclaration)* (methodDeclaration*) '}' ;

varDeclaration : type IDENTIFIER ';' ;

methodDeclaration : 'public' ('io')? type IDENTIFIER '(' (type IDENTIFIER (',' type IDENTIFIER)*)? ')' '{' (varDeclaration)* (statement)* 'return' expression ';' '}' ;

type :
      'int' '[' ']'
    | 'boolean'
    | 'int'
    | IDENTIFIER
    ;

statement :
      '{' (statement)* '}'
    | 'if' '(' expression ')' statement 'else' statement
    | 'while' '(' expression ')' statement
    | 'System.out.println' '(' expression ')' ';'
    | IDENTIFIER '=' expression ';'
    | IDENTIFIER '[' expression ']' '=' expression ';'
    ;

expression :
      expression (AND | LT | PLUS | MINUS | TIMES) expression
    | expression '[' expression ']'
    | expression '.' 'length'
    | expression '.' IDENTIFIER'(' (expression ( ',' expression )*)? ')'
    | INTEGER_LITERAL
    | 'true' 
    | 'false'
    | IDENTIFIER
    | 'this'
    | 'new' 'int' '[' expression ']'
    | 'new' IDENTIFIER '(' ')'
    | NOT expression
    | '(' expression ')'
    ;

IDENTIFIER : [a-zA-Z_]+ ;
INTEGER_LITERAL : [0-9]+ ;

AND : '&&' ;
LT : '<' ;
PLUS : '+' ;
MINUS : '-' ;
TIMES : '*' ;
NOT : '!';

WS: [ \n\t\r]+ -> skip;
