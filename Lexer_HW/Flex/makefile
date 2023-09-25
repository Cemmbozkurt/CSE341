build: gpp_lexer.out

gpp_lexer.out: gpp_lexer.c 
	gcc gpp_lexer.c -o gpp_lexer.out

gpp_lexer.c: gpp_lexer.l
	lex -o gpp_lexer.c gpp_lexer.l

clean: 
	rm gpp_lexer.c gpp_lexer.out