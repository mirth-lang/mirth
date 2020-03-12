#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

// Command-line info
struct command_t {
    const char* path;
} command = {0};

// String table
enum builtin_t {
    BUILTIN_END = 0,
    BUILTIN_ID,
    BUILTIN_DUP,
    BUILTIN_DROP,
    BUILTIN_SWAP,
};
#define NUM_BUILTINS (BUILTIN_SWAP+1)

#define SYMBOLS_SIZE 0xC010
#define NAME_SIZE 0x10

struct symbols_t {
    size_t length;
    struct __attribute__((packed)) name_t {
        char data [NAME_SIZE];
    } name [SYMBOLS_SIZE];
} symbols = {
    .length = NUM_BUILTINS,
    .name = {
        [BUILTIN_END] = { .data = "end" },
        [BUILTIN_ID] = { .data = "id" },
        [BUILTIN_DUP] = { .data = "dup" },
        [BUILTIN_DROP] = { .data = "drop" },
        [BUILTIN_SWAP] = { .data = "swap" },
    }
};

// Tokens, Syntax Tree (it's the same structure)
#define TOKENS_SIZE 0x8100
struct tokens_t {
    size_t length;
    uint16_t row [TOKENS_SIZE];
    uint8_t col [TOKENS_SIZE];
    uint8_t depth [TOKENS_SIZE];
    enum __attribute__((packed)) token_kind_t {
        TOKEN_NONE = 0,
        TOKEN_NEWLINE,
        TOKEN_LPAREN,
        TOKEN_RPAREN,
        TOKEN_COMMA,
        TOKEN_COLON,
        TOKEN_WORD,
    } kind [TOKENS_SIZE];
    uint16_t value [TOKENS_SIZE];
} tokens = {0};

// Lexer state
#define LEXER_LINE_MAX 0x200
#define LEXER_STACK_SIZE 0x80
struct lexer_t {
    FILE* file;
    uint16_t row;
    uint8_t col;
    uint8_t depth;
    uint32_t stack [LEXER_STACK_SIZE];
    char line [LEXER_LINE_MAX];
} lexer = {0};

int main (int argc, const char** argv)
{
    { // Parse command-line arguments.
        for (int i = 1; i < argc; i++) {
            if (command.path == NULL) {
                command.path = argv[i];
            } else {
                fprintf(stderr, "mirth: Bad command.\n");
                return 1;
            }
        }
        if (command.path == NULL) {
            fprintf(stderr, "mirth: Expected source file.\n");
            return 1;
        }
    }

    { // Read file and tokenize it.
        lexer.file = fopen(command.path, "r");
        lexer.row = 0;
        lexer.col = 0;
        lexer.depth = 0;
        if (lexer.file == NULL) {
            fprintf(stderr, "mirth: Failed to open file %s\n", command.path);
            return 1;
        }
        while (1) {
            char* line = fgets(lexer.line, LEXER_LINE_MAX, lexer.file);
            if (ferror(lexer.file)) {
                fprintf(stderr, "mirth: Error while reading file %s\n", command.path);
                return 1;
            }
            if (feof(lexer.file)) break;
            if (line == NULL)
                exit(100); // can't happen
            lexer.row ++;
            lexer.col = 1;
            //fprintf(stderr, "%s:%d:%d: info: %s", command.path, lexer.row, lexer.col, line);

            while (*line) {
                const char* token_start;
                uint32_t token_size;
                uint32_t token_row = lexer.row;
                uint8_t token_col = lexer.col;

                uint32_t t = tokens.length;
                uint32_t t0;
                if ((size_t)t >= TOKENS_SIZE-1) {
                    fprintf(stderr, "mirth: Ran out of tokens space. Increase TOKENS_SIZE.\n");
                    exit(102);
                }
                tokens.row[t] = token_row;
                tokens.col[t] = token_col;
                tokens.depth[t] = lexer.depth;
                tokens.kind[t] = TOKEN_NONE;
                tokens.value[t] = 0;

                switch (*line) {
                    case '\0':
                        exit(101); // can't happen

                    case ' ':
                    case '\t':
                        lexer.col++;
                    case '\v':
                    case '\r':
                        line++;
                        break; // ignore whitespace

                    case '\n':
                        fprintf(stderr, "%s:%d:%d: info: NEWLINE %d\n", command.path, token_row, token_col, t);
                        tokens.kind[tokens.length++] = TOKEN_NEWLINE;
                        line++;
                        break;

                    case '(':
                        fprintf(stderr, "%s:%d:%d: info: LPAREN %d\n", command.path, token_row, token_col, t);
                        if ((size_t)lexer.depth >= LEXER_STACK_SIZE-1) {
                            fprintf(stderr, "%s:%d:%d: error: Ran out of lexer stack. Increase LEXER_STACK_SIZE in compiler.\n", command.path, token_row, token_col);
                            exit(103);
                        }
                        lexer.stack[lexer.depth++] = t;
                        tokens.kind[tokens.length++] = TOKEN_LPAREN;
                        lexer.col++;
                        line++;
                        break;

                    case ')':
                        fprintf(stderr, "%s:%d:%d: info: RPAREN %d\n", command.path, token_row, token_col, t);
                        if (lexer.depth == 0) {
                            fprintf(stderr, "%s:%d:%d: error: Right paren without matching left paren.\n", command.path, token_row, token_col);
                            exit(2);
                        }
                        t0 = lexer.stack[--lexer.depth];
                        if (tokens.kind[t0] != TOKEN_LPAREN) {
                            fprintf(stderr, "%s:%d:%d: error: Left token without matching right token.\n", command.path, tokens.row[t0], tokens.col[t0]);
                            fprintf(stderr, "%s:%d:%d: error: Right paren without matching left paren.\n", command.path, token_row, token_col);
                            exit(3);
                        }
                        tokens.value[t0] = t;
                        tokens.value[t] = t0;
                        fprintf(stderr, "%s:%d:%d: info: ^ LPAREN %d\n", command.path, token_row, token_col, t0);
                        tokens.kind[tokens.length++] = TOKEN_RPAREN;
                        lexer.col++;
                        line++;
                        break;

                    case ',':
                        fprintf(stderr, "%s:%d:%d: info: COMMA %d\n", command.path, token_row, token_col, t);
                        tokens.kind[tokens.length++] = TOKEN_COMMA;
                        lexer.col++;
                        line++;
                        break;

                    case ':':
                        fprintf(stderr, "%s:%d:%d: info: COLON %d\n", command.path, token_row, token_col, t);
                        tokens.kind[tokens.length++] = TOKEN_COLON;
                        lexer.col++;
                        line++;
                        break;

                    case '\"':
                        exit(200); // not yet implemented

                    default:
                        token_start = line;
                        while (*line) {
                            switch (*line) {
                                case '\0':
                                    exit(102); // can't happen;

                                case ' ':
                                case '\t':
                                case '\v':
                                case '\r':
                                case '\n':
                                case '(':
                                case ')':
                                case ',':
                                case ':':
                                case '"':
                                    goto end_of_token;

                                default:
                                    line++;
                                    lexer.col += (lexer.col >= 1);
                                    break;
                            }
                        }
                    end_of_token:
                        token_size = line - token_start;
                        if (token_size >= NAME_SIZE) {
                            token_size = NAME_SIZE-1;
                            fprintf(stderr, "%s:%d:%d: warning: Token too large. Cutting to %d bytes.\n", command.path, token_row, token_col, token_size);
                        }
                        bool found_match = false;
                        char buf[NAME_SIZE];
                        memcpy(buf, token_start, token_size);
                        memset(buf+token_size, 0, NAME_SIZE-token_size);
                        for (int j = 0; j < symbols.length; j++) {
                            // TODO: experiment with changing where you start searching based on some simple rudimentary hash.
                            if (memcmp(buf, symbols.name[j].data, NAME_SIZE) == 0) {
                                found_match = true;
                                tokens.value[t] = j;
                                break;
                            }
                        }
                        if (!found_match) {
                            if ((size_t)symbols.length >= SYMBOLS_SIZE-1) {
                                fprintf(stderr, "%s:%d:%d: error: Ran out of symbols. Increase SYMBOLS_SIZE in the compiler.\n", command.path, token_row, token_col);
                            }
                            memcpy(symbols.name[symbols.length].data, buf, NAME_SIZE);
                            tokens.value[t] = symbols.length++;
                        }
                        fprintf(stderr, "%s:%d:%d: info: WORD %d \"%s\" %d\n", command.path, token_row, token_col, tokens.value[t], symbols.name[tokens.value[t]].data, t);
                        tokens.kind[tokens.length++] = TOKEN_WORD;
                        break;
                }
            }
        }
        if ((tokens.length == 0) || (tokens.kind[tokens.length - 1] != TOKEN_NEWLINE)) {
            fprintf(stderr, "%s:%d:%d: info: Added missing newline at end of file.", command.path, lexer.row, lexer.col);
            size_t t = tokens.length;
            tokens.row[t] = lexer.row;
            tokens.col[t] = lexer.col+1;
            tokens.depth[t] = lexer.depth;
            tokens.kind[tokens.length++] = TOKEN_NEWLINE;
        }
        fclose(lexer.file);
    }

    return 0;
}
