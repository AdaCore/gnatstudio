%{
#include <string.h>
#include "parser.tab.h"
%}

digit   [0-9]
name	[a-z][a-z0-9_]*[\$%]?
	
%%

\'.*		{ /* Eat comments. */ }
rem[ \t]*.*	{ /* Likewise. */ }

\".*\"		{ return STRINGLITERAL; }
alias		{ return ALIAS; }
public		{ return PUBLIC; }
private		{ return PRIVATE; }
byref		{ return BYREF; }
byval		{ return BYVAL; }
case		{ return CASE; }
close		{ return CLOSE; }
const		{ return CONST; }
declare		{ return DECLARE; }
do		{ return DO; }
end		{ return END; }
error		{ return ERROR; }
exit		{ return EXIT; }
input		{ return INPUT; }
let		{ return LET; }
lib		{ return LIB; }
loop		{ return LOOP; }
on		{ return ON; }
function	{ return FUNCTION; }
global		{ return GLOBAL; }
as		{ return AS; }
dim		{ return DIM; }
if		{ return IF; }
then		{ return THEN; }
else		{ return ELSE; }
select		{ return SELECT; }
step		{ return STEP; }
string		{ return STRING; }
sub		{ return SUB; }
next		{ return NEXT; }
for		{ return FOR; }
to		{ return TO; }
type		{ return TYPE; }
until		{ return UNTIL; }
while		{ return WHILE; }

"<>"		{ return NOTEQ; }

\(		{ return '('; }
\)		{ return ')'; }
,		{ return ','; }
=		{ return '='; }
\*		{ return '*'; }

{name}		{	
			yylval.text = strdup(yytext);
			return NAME;
		}

{digit}+	{ return NUMBER; }

[ \t]+		{ /* Eat white space. */ }
\n		{ /* Eat newlines. */ }

%%
