%{

#include <string>
//#include "node.h"
//#include "parser.hpp"
#define SAVE_TOKEN yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t) (yylval.token = t)
extern "C" int yywrap() { }

%}

%%

.                       printf("Unknown token!n"); yyterminate();

%%

main() {
    yylex();
}
