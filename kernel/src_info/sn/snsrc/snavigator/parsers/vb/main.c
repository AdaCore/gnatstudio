#include <stdio.h>

void yyparse();

int
main()
{
	yyparse();
	return 0;
}

int
yyerror(char * s)
{
	puts(s);
	return 0;
}
