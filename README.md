Scala Lambda
==========================

Lambda calculus implementation in scala.
## Overview
The	syntax	of	the	lambda	expressions	will use \ for	λ.	There will be no	spaces.	Variables	will	be	just	
a	single	letter.	Parenthesis	may	be	included	as	necessary.	E.g.	here	is	a	legal	input	\x.(xx)a	which	
would	evaluate	to	'aa'. The	main	program	consists	of	a	simple REPL	loop	that	reads	one	line,	parses	
it	as	a	lambda	expression,	then	prints	the	normal	form	of	that	expression.

## Parser
Parser is implemented using Scala parser combinator library.

## Evaluator
Evaluator follows	the	call-by-value	evaluation	rules	from	Pierce	chapter	5. Evaluator performs alpha-renaming followed by beta substitutions recursively to arrive at the normal form.

## Requirements
Scala, Maven, Java

## How to build the project
Use the following command:
_mvn clean install package_

## To run the project:
Use the following command:
_java -jar scalalambda.jar_