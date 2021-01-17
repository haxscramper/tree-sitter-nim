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

void* tree_sitter_nim_external_scanner_create() {}

void tree_sitter_nim_external_scanner_destroy(void* payload) {}

unsigned tree_sitter_nim_external_scanner_serialize(
    void* payload,
    char* buffer) {}

void tree_sitter_nim_external_scanner_deserialize(
    void*       payload,
    const char* buffer,
    unsigned    length) {}

bool tree_sitter_nim_external_scanner_scan(
    void*       payload,
    TSLexer*    lexer,
    const bool* valid_symbols) {

    printf("Requested scan at char %c\n", lexer->lookahead);
    for (int i = 0; i < GENERALIZED_TRIPLESTR_LIT; ++i) {
        printf(
            "Expecting [%d]: [%s]\n",
            i,
            valid_symbols[i] ? "true" : "false");
    }
}
