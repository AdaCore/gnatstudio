#ifndef EMIT_H
#define EMIT_H

int emit_subprogram(char * name);
int emit_function(char * name);
int emit_type(char * name);
int emit_variable(char * name, char * type);

#endif /* EMIT_H */
