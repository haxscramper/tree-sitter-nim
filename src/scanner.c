#include <stdbool.h>
#include <stdio.h>
#include <tree_sitter/parser.h>

enum TokenType
{
    IND_GE,
    IND_EQ,
    DED,
    COMMENT,
    CHAR_LIT,
    STR_LIT,
    TRIPLESTR_LIT,
    RSTR_LIT,
    GENERALIZED_STR_LIT,
    GENERALIZED_TRIPLESTR_LIT
};

void* tree_sitter_nim_external_scanner_create() {
    return NULL;
}

void tree_sitter_nim_external_scanner_destroy(void* payload) {
}

unsigned tree_sitter_nim_external_scanner_serialize(
    void* payload,
    char* buffer) {
    return 0;
}

void tree_sitter_nim_external_scanner_deserialize(
    void*       payload,
    const char* buffer,
    unsigned    length) {
}

#define AT(charVal) (lexer->lookahead == (charVal))
#define SKIP() (lexer->advance(lexer, false);)
#define NEXT() (lexer->advance(lexer, true);)

bool tree_sitter_nim_external_scanner_scan(
    void*       payload,
    TSLexer*    lexer,
    const bool* valid_symbols) {


    /* printf("Requested scan at char '%c'\n", lexer->lookahead); */
    /* if (valid_symbols[STR_LIT] && AT('"')) { */
    /*     puts("Scanning over string"); */
    /* } */

    return false;
}
