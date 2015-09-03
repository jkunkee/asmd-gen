
LEXER=lexer

test: lex
	./$(LEXER)

lex: lex.l
	flex lex.l
	g++ lex.yy.c -o $(LEXER) -lfl

clean:
	rm -f $(LEXER)
	rm -f lex.yy.c

