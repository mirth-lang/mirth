#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

enum error_t {
    ERROR_NONE = 0,
    ERROR_BAD_COMMAND = 1,
    ERROR_FILE_IO = 2,
    ERROR_SYNTAX = 3,
    ERROR_ARITY = 50,
    ERROR_UNDERFLOW = 51,
    ERROR_OVERFLOW = 52,
    ERROR_UNDEFINED = 53,
    ERROR_NOT_IMPLEMENTED = 200,
    ERROR_IMPOSSIBLE = 201,
};

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
    BUILTIN_DIP,
    BUILTIN_DEF,
};
#define NUM_BUILTINS (BUILTIN_DEF+1)

#define SYMBOLS_SIZE 0xC010
#define NAME_SIZE 0x10
#define ASSERT(b) do { if (!(b)) { fprintf(stderr, "%s:%d:1: error: assertion failed: %s", __FILE__, __LINE__, #b); return ERROR_IMPOSSIBLE; } } while(0)

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
        [BUILTIN_DIP] = { .data = "dip" },
        [BUILTIN_DROP] = { .data = "drop" },
        [BUILTIN_SWAP] = { .data = "swap" },
        [BUILTIN_DEF] = { .data = "def" },
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
        TOKEN_INT,
        TOKEN_STR,
    } kind [TOKENS_SIZE];
    uint16_t value [TOKENS_SIZE];
} tokens = {0};

// String table
#define STRINGS_SIZE 0x4000
struct strings_t {
    uint16_t length;
    char data [STRINGS_SIZE];
} strings = {0};

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

enum __attribute__((packed)) type_t {
    TYPE_NIL,

    // actual value types
    TYPE_INT,
    TYPE_STR,

    // return stack types
    TYPE_FRAME,
    TYPE_JUMP
};

struct __attribute__((packed)) value_t {
    enum type_t type;
    int16_t data;
};

struct __attribute__((packed)) fvalue_t {
    int16_t pc;
    int16_t fi;
};

// intepreter state
#define STACK_SIZE 0x400
#define RSTACK_SIZE 0x100
#define FSTACK_SIZE 0x400
struct __attribute__((packed)) state_t {
    uint16_t pc; // program counter
    uint16_t sc; // stack counter
    uint16_t rc; // return stack counter
    uint16_t fc; // frame stack counter
    uint16_t fi; // frame pointer
    struct value_t stack [STACK_SIZE];
    struct value_t rstack [RSTACK_SIZE];
    struct fvalue_t fstack [FSTACK_SIZE];
} state = {
    .pc = 0,
    .sc = STACK_SIZE,
    .rc = RSTACK_SIZE,
    .fc = FSTACK_SIZE,
};

int main (int argc, const char** argv)
{
    { // Parse command-line arguments.
        for (int i = 1; i < argc; i++) {
            if (command.path == NULL) {
                command.path = argv[i];
            } else {
                fprintf(stderr, "mirth: Bad command.\n");
                return ERROR_BAD_COMMAND;
            }
        }
        if (command.path == NULL) {
            fprintf(stderr, "mirth: Expected source file.\n");
            return ERROR_BAD_COMMAND;
        }
    }

    { // Read file and tokenize it.
        lexer.file = fopen(command.path, "r");
        lexer.row = 0;
        lexer.col = 0;
        lexer.depth = 0;
        if (lexer.file == NULL) {
            fprintf(stderr, "mirth: Failed to open file %s\n", command.path);
            return ERROR_FILE_IO;
        }
        while (1) {
            char* line = fgets(lexer.line, LEXER_LINE_MAX, lexer.file);
            if (ferror(lexer.file)) {
                fprintf(stderr, "%s:%d:%d: Error while reading file %s\n",
                    command.path, lexer.row, lexer.col, command.path);
                return ERROR_FILE_IO;
            }
            if (feof(lexer.file)) break;
            ASSERT (line != NULL);
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
                    return ERROR_OVERFLOW;
                }
                tokens.row[t] = token_row;
                tokens.col[t] = token_col;
                tokens.depth[t] = lexer.depth;
                tokens.kind[t] = TOKEN_NONE;
                tokens.value[t] = 0;

                switch (*line) {
                    case '\0':
                        return ERROR_IMPOSSIBLE; // can't happen

                    case ' ':
                    case '\t':
                        lexer.col++;
                    case '\v':
                    case '\r':
                        line++;
                        break; // ignore whitespace

                    case '\n':
                        // fprintf(stderr, "%s:%d:%d: info: NEWLINE %d\n", command.path, token_row, token_col, t);
                        tokens.kind[tokens.length++] = TOKEN_NEWLINE;
                        line++;
                        break;

                    case '(':
                        // fprintf(stderr, "%s:%d:%d: info: LPAREN %d\n", command.path, token_row, token_col, t);
                        if ((size_t)lexer.depth >= LEXER_STACK_SIZE-1) {
                            fprintf(stderr, "%s:%d:%d: error: Ran out of lexer stack. Increase LEXER_STACK_SIZE in compiler.\n", command.path, token_row, token_col);
                            return ERROR_OVERFLOW;
                        }
                        lexer.stack[lexer.depth++] = t;
                        tokens.kind[tokens.length++] = TOKEN_LPAREN;
                        lexer.col++;
                        line++;
                        break;

                    case ')':
                        // fprintf(stderr, "%s:%d:%d: info: RPAREN %d\n", command.path, token_row, token_col, t);
                        if (lexer.depth == 0) {
                            fprintf(stderr, "%s:%d:%d: error: Right paren without matching left paren.\n", command.path, token_row, token_col);
                            return ERROR_SYNTAX;
                        }
                        t0 = lexer.stack[--lexer.depth];
                        if (tokens.kind[t0] != TOKEN_LPAREN) {
                            fprintf(stderr, "%s:%d:%d: error: Left token without matching right token.\n", command.path, tokens.row[t0], tokens.col[t0]);
                            fprintf(stderr, "%s:%d:%d: error: Right paren without matching left paren.\n", command.path, token_row, token_col);
                            return ERROR_SYNTAX;
                        }
                        tokens.value[t0] = t;
                        tokens.value[t] = t0;
                        // fprintf(stderr, "%s:%d:%d: info: ^ LPAREN %d\n", command.path, token_row, token_col, t0);
                        tokens.kind[tokens.length++] = TOKEN_RPAREN;
                        lexer.col++;
                        line++;
                        break;

                    case ',':
                        // fprintf(stderr, "%s:%d:%d: info: COMMA %d\n", command.path, token_row, token_col, t);
                        tokens.kind[tokens.length++] = TOKEN_COMMA;
                        lexer.col++;
                        line++;
                        break;

                    case ':':
                        // fprintf(stderr, "%s:%d:%d: info: COLON %d\n", command.path, token_row, token_col, t);
                        tokens.kind[tokens.length++] = TOKEN_COLON;
                        lexer.col++;
                        line++;
                        break;

                    case '\"':
                        { // lex a string
                            size_t n = strings.length;
                            if (((n+7) & 0x3F) < (n & 0x3F)) {
                                // n is nearly at cache line break, so add some padding
                                n += 0x40 - (n & 0x3F);
                            }
                            size_t i = n;
                            line++;
                            lexer.col++;
                            while (*line) {
                                switch (*line) {
                                    case 0:
                                        return ERROR_IMPOSSIBLE;
                                    case '\"':
                                    case '\n':
                                        goto string_over;
                                    case '\\':
                                        line++;
                                        lexer.col++;
                                        switch (*line) {
                                            default:
                                                return ERROR_NOT_IMPLEMENTED;
                                        }
                                    default:
                                        strings.data[i++] = *line;
                                        line++;
                                        lexer.col++;
                                }
                            }
                            string_over:
                            if (*line == '\"') {
                                line++;
                                lexer.col++;
                                strings.data[i++] = 0;
                                tokens.value[t] = n;
                                tokens.kind[tokens.length++] = TOKEN_STR;
                                strings.length = i;
                            } else {
                                fprintf(stderr, "%s:%d:%d: error: Unexpected newline in string literal.\n",
                                    command.path, lexer.row, lexer.col);
                                return ERROR_SYNTAX;
                            }
                        }
                        break;

                    default:
                        token_start = line;
                        bool hexadecimal = false;
                        bool positive = true;
                        bool only_digits = true;
                        bool only_hexdigits = true;
                        uint64_t decimal_value = 0;
                        uint64_t hexadecimal_value = 0;

                        switch (*line) {
                            case '+': line++; lexer.col++; break;
                            case '-': positive = false; line++; lexer.col++; break;
                        }

                        if (*line == '+' || *line == '-') {
                            line++;
                            lexer.col++;
                        }
                        if (line[0] == '0' && (line[1] == 'x' || line[1] == 'X')) {
                            line+=2;
                            lexer.col+=2;
                            hexadecimal = true;
                        }

                        while (*line) {
                            switch (*line) {
                                case '\0':
                                    return ERROR_IMPOSSIBLE;

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

                                case '0': case '1':
                                case '2': case '3':
                                case '4': case '5':
                                case '6': case '7':
                                case '8': case '9':
                                    decimal_value *= 10;
                                    decimal_value += *line - '0';
                                    hexadecimal_value *= 16;
                                    hexadecimal_value += *line - '0';
                                    line++;
                                    lexer.col++;
                                    break;

                                case 'a': case 'b':
                                case 'c': case 'd':
                                case 'e': case 'f':
                                    hexadecimal_value *= 16;
                                    hexadecimal_value += *line - 'a' + 10;
                                    line++;
                                    lexer.col++;
                                    only_digits = false;
                                    break;

                                case 'A': case 'B':
                                case 'C': case 'D':
                                case 'E': case 'F':
                                    hexadecimal_value *= 16;
                                    hexadecimal_value += *line - 'A' + 10;
                                    line++;
                                    lexer.col++;
                                    only_digits = false;
                                    break;

                                default:
                                    line++;
                                    lexer.col++;
                                    only_digits = false;
                                    only_hexdigits = false;
                                    break;
                            }
                        }
                    end_of_token:
                        if (hexadecimal && only_hexdigits) {
                            if (positive) {
                                if (hexadecimal_value >= 0x8000) {
                                    fprintf(stderr, "%s:%d:%d: error: Positive integer literal too large.\n", command.path, token_row, token_col);
                                }
                                tokens.value[t] = hexadecimal_value;
                            } else {
                                if (hexadecimal_value > 0x8000) {
                                    fprintf(stderr, "%s:%d:%d: error: Negative integer literal too large.\n", command.path, token_row, token_col);
                                }
                                tokens.value[t] = -(int16_t)hexadecimal_value;
                            }
                            // fprintf(stderr, "%s:%d:%d: info: INT 0x%X %d\n", command.path, token_row, token_col, tokens.value[t], t);
                            tokens.kind[tokens.length++] = TOKEN_INT;

                        } else if (only_digits) {
                            if (positive) {
                                if (decimal_value >= 0x8000) {
                                    fprintf(stderr, "%s:%d:%d: error: Positive integer literal too large.\n", command.path, token_row, token_col);
                                }
                                tokens.value[t] = decimal_value;
                            } else {
                                if (decimal_value > 0x8000) {
                                    fprintf(stderr, "%s:%d:%d: error: Negative integer literal too large.\n", command.path, token_row, token_col);
                                }
                                tokens.value[t] = -(int16_t)decimal_value;
                            }
                            // fprintf(stderr, "%s:%d:%d: info: INT %d %d\n", command.path, token_row, token_col, tokens.value[t], t);
                            tokens.kind[tokens.length++] = TOKEN_INT;
                        } else {
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
                            // fprintf(stderr, "%s:%d:%d: info: WORD %d \"%s\" %d\n", command.path, token_row, token_col, tokens.value[t], symbols.name[tokens.value[t]].data, t);
                            tokens.kind[tokens.length++] = TOKEN_WORD;
                        }
                        break;
                }
            }
        }
        if ((tokens.length == 0) || (tokens.kind[tokens.length - 1] != TOKEN_NEWLINE)) {
            fprintf(stderr, "%s:%d:%d: warning: Missing newline at end of file.", command.path, lexer.row, lexer.col);
            size_t t = tokens.length;
            tokens.row[t] = lexer.row;
            tokens.col[t] = lexer.col+1;
            tokens.depth[t] = lexer.depth;
            tokens.kind[tokens.length++] = TOKEN_NEWLINE;
        }
        fclose(lexer.file);
    }

    { // Run file.
        uint16_t pc = 0;
        uint16_t sc = STACK_SIZE;
        #define arity_check(word,num_params,num_in,num_out) \
            if (num_args != num_params) { \
                fprintf(stderr, "%s:%d:%d: error: %s expects %d arguments, got %d\n", \
                    command.path, tokens.row[pc], tokens.col[pc], \
                    word, num_params, num_args); \
                return ERROR_ARITY; \
            } \
            if (sc+num_in > STACK_SIZE) { \
                fprintf(stderr, "%s:%d:%d: error: stack underflow in %s\n", \
                    command.path, tokens.row[pc], tokens.col[pc], word); \
                return ERROR_UNDERFLOW; \
            } \
            if (sc+num_in < num_out) { \
                fprintf(stderr, "%s:%d:%d: error: stack overflow in %s\n", \
                    command.path, tokens.row[pc], tokens.col[pc], word); \
                return ERROR_OVERFLOW; \
            }
        uint16_t rc = RSTACK_SIZE;
        uint16_t fc = FSTACK_SIZE;
        int16_t a;
        int16_t stack [STACK_SIZE];
        int16_t rstack [RSTACK_SIZE];
        int16_t next_pc, num_args;
        int16_t fstack [FSTACK_SIZE];
        for (; pc < tokens.length; pc++) {
        resume_loop:
            switch (tokens.kind[pc]) {
                case TOKEN_NONE:
                case TOKEN_RPAREN:
                    goto pop_rstack;
                case TOKEN_NEWLINE:
                case TOKEN_COMMA:
                case TOKEN_COLON:
                    break;
                case TOKEN_LPAREN:
                    pc = tokens.value[pc]+1;
                    break;
                case TOKEN_INT:
                    stack[--sc] = tokens.value[pc];
                    break;
                case TOKEN_STR:
                    stack[--sc] = tokens.value[pc];
                    break;
                case TOKEN_WORD:
                    next_pc = pc+1;
                    num_args = 0;
                    if (tokens.kind[pc+1] == TOKEN_LPAREN) { // determine arguments to word
                        bool is_empty_arg = true;
                        next_pc = tokens.value[pc+1]+1;
                        if (fc < 1) {
                            fprintf(stderr, "%s:%d:%d: error: fstack ran out, increase FSTACK_SIZE\n",
                                command.path, tokens.row[pc], tokens.col[pc]);
                            return ERROR_OVERFLOW;
                        }
                        num_args += 1;
                        fstack[--fc] = pc+1;
                        for (int i = pc+2; i < next_pc; i++) {
                            switch (tokens.kind[i]) {
                                case TOKEN_COMMA:
                                    if (fc < 1) {
                                        fprintf(stderr, "%s:%d:%d: error: fstack ran out, increase FSTACK_SIZE\n",
                                            command.path, tokens.row[pc], tokens.col[pc]);
                                        return ERROR_OVERFLOW;
                                    }
                                    num_args += 1;
                                    fstack[--fc] = pc+1;
                                    is_empty_arg = true;
                                    break;

                                case TOKEN_NEWLINE:
                                case TOKEN_RPAREN:
                                    break;

                                case TOKEN_LPAREN:
                                    i = tokens.value[i];
                                    is_empty_arg = false;
                                    break;

                                default:
                                    is_empty_arg = false;
                                    break;
                            }
                        }

                        if (is_empty_arg) {
                            num_args--;
                            fc++;
                        }
                    }

                    switch (tokens.value[pc]) {
                        case BUILTIN_END:
                            fprintf(stderr, "%s:%d:%d: error: unexpected \"end\"\n",
                                command.path, tokens.row[pc], tokens.col[pc]);
                            return ERROR_SYNTAX;
                        case BUILTIN_ID:
                            arity_check("id", 0,0,0);
                            break;
                        case BUILTIN_DIP:
                            arity_check("dip", 1, 1, 1);
                            if (rc < 3) {
                                fprintf(stderr, "%s:%d:%d: error: rstack ran out, increase RSTACK_SIZE\n",
                                    command.path, tokens.row[pc], tokens.col[pc]);
                                return ERROR_OVERFLOW;
                            }
                            pc = fstack[fc++];
                            rstack[--rc] = next_pc;
                            rstack[--rc] = stack[sc++];
                            rstack[--rc] = -1;
                            break;
                        case BUILTIN_DUP:
                            arity_check("dup", 0, 1, 2);
                            a = stack[sc];
                            stack[--sc] = a;
                            pc = next_pc - 1;
                            break;
                        case BUILTIN_DROP:
                            arity_check("drop", 0, 1, 0);
                            sc++;
                            break;
                        case BUILTIN_SWAP:
                            arity_check("swap", 0, 2, 2);
                            a = stack[sc];
                            stack[sc] = stack[sc+1];
                            stack[sc+1] = a;
                            break;
                        case BUILTIN_DEF:
                            return ERROR_NOT_IMPLEMENTED;
                        default:
                            fprintf(stderr, "%s:%d:%d: error: undefined word \"%s\"\n",
                                command.path, tokens.row[pc], tokens.col[pc],
                                symbols.name[tokens.value[pc]].data);
                            return ERROR_UNDEFINED;
                    }
                    break;

            }
        }

        pop_rstack:
        if (rc < RSTACK_SIZE) {
            if (rstack[rc] == -1) {
                if (sc < 1) {
                    fprintf(stderr, "%s:%d:%d: error: stack overflow when returning from dip",
                        command.path, tokens.row[pc], tokens.col[pc]);
                    return 56;
                }
                rc++;
                stack[--sc] = rstack[rc++];
                goto pop_rstack;
            } else {
                pc = rstack[rc++];
                goto resume_loop;
            }
        }

        for (; sc < STACK_SIZE; sc++) {
            fprintf(stderr, "%d ", stack[sc]);
        }
        fprintf(stderr, "\n");
    }

    return 0;
}
