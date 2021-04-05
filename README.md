# smallScheme

dont use test.sh, it is just a reference for the final test setup

stack test should work now ish
run stack test --test-arguments "--no-create"
if there is only .rkt files in test folder as this will prevent golden tests from auto generating, since we have a canonical compiler (racket-lang)

canonical.sh should run racket on each rkt file in features and generate ref (erence) files. 
todo - 	integrate canonical generation in stack test framework. 
	get canonical to ask for folder so it can do both features and programs
