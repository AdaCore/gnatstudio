#include <stdio.h>

int
emit_subprogram(char * name)
{
	printf("emit_subprogram: %s\n", name);
	return 0;
}

int
emit_function(char * name)
{
	printf("emit_function: %s\n", name);
	return 0;
}

int
emit_type(char * name)
{
	printf("emit_type: %s\n", name);
	return 0;
}

int
emit_variable(char * name, char * type)
{
	printf("emit_variable: %s (%s)\n", name, type);
	return 0;
}
