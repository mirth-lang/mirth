#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define WITH_PREFETCHING

enum error_t {
    ERROR_NONE = 0,
    ERROR_BAD_COMMAND = 1,
    ERROR_FILE_IO = 2,
    ERROR_SYNTAX = 3,
    ERROR_PANIC = 4,
    ERROR_ARITY = 50,
    ERROR_UNDERFLOW = 51,
    ERROR_OVERFLOW = 52,
    ERROR_UNDEFINED = 53,
    ERROR_TYPE = 54,
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
    BUILTIN_IF,
    BUILTIN_PANIC,
    BUILTIN_DEF,
    BUILTIN_OUTPUT_ASM,
    NUM_BUILTINS
};

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
        [BUILTIN_IF] = { .data = "if" },
        [BUILTIN_PANIC] = { .data = "panic" },
        [BUILTIN_OUTPUT_ASM] = { .data = "output-asm" },
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

// Definitions
struct defs_t {
    int16_t pc[SYMBOLS_SIZE];
} defs = { {0} };

enum __attribute__((packed)) type_t {
    TYPE_NIL,

    // actual value types
    TYPE_INT,
    TYPE_STR,

    // return stack types
    TYPE_FI,
    TYPE_FC,
    TYPE_PC
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

static void fprint_value (FILE* fp, struct value_t value) {
    switch (value.type) {
        case TYPE_INT:
            fprintf(fp, "%d", value.data);
            break;

        case TYPE_STR:
            fprintf(fp, "\"%s\"", &strings.data[value.data]);
            break;

        case TYPE_NIL:
            fprintf(fp, "<nil>");
            break;

        case TYPE_FI:
            fprintf(fp, "<fi>");
            break;

        case TYPE_FC:
            fprintf(fp, "<fc>");
            break;

        case TYPE_PC:
            fprintf(fp, "<pc>");
            break;
    }
}

static void fprint_stack (FILE* fp) {
    for (int i = STACK_SIZE-1; i >= state.sc; i--) {
        fprint_value(fp, state.stack[i]);
        fprintf(fp, " ");
    }
    fprintf(fp, "\n");
}

static void mangle (char* dst, const char* src) {
    for (int j = 0; j < NAME_SIZE && *src; j++, src++) {
        char c = *src;
        if (isalpha(c) || isdigit(c)) {
            *dst++ = *src;
        } else {
            *dst++ = '_';
            *dst++ = 'a' + (c >> 4);
            *dst++ = 'a' + (c & 0xF);
            *dst++ = '_';
        }
    }
    *dst++ = 0;
}

struct output_t {
    FILE* file;
    int fresh;
} output = {0};

static void output_asm_block (size_t t) {
    char mangled_name[NAME_SIZE*4+1];
    for (; t < tokens.length; t++) {
        switch(tokens.kind[t]) {
            case TOKEN_NONE:
            case TOKEN_RPAREN:
            case TOKEN_COMMA:
            case TOKEN_COLON:
                return;

            case TOKEN_NEWLINE:
                break;

            case TOKEN_LPAREN:
                t = tokens.value[t];
                break;

            case TOKEN_INT:
                fprintf(output.file, "    lea rbx, [rbx-8]\n");
                fprintf(output.file, "    mov [rbx], rax\n");
                fprintf(output.file, "    mov rax, %d\n", tokens.value[t]);
                break;

            case TOKEN_STR:
                fprintf(output.file, "    lea rbx, [rbx-8]\n");
                fprintf(output.file, "    mov [rbx], rax\n");
                fprintf(output.file, "    lea rax, [rel strings+%d]\n", tokens.value[t]);
                break;

            case TOKEN_WORD:
                {
                    size_t num_args = 0;
                    size_t args[16];
                    size_t next = t+1;
                    if (tokens.kind[t+1] == TOKEN_LPAREN) {
                        next = tokens.value[t+1]+1;
                        args[num_args++] = t+2;
                        for (size_t i = t+2; i+1 < next && num_args < 16; i++) {
                            switch (tokens.kind[i]) {
                                case TOKEN_LPAREN:
                                    i = tokens.value[i];
                                    break;
                                case TOKEN_COMMA:
                                    args[num_args++] = i+1;
                                    break;
                                default:
                                    break;
                            }
                        }
                    }
                    switch (tokens.value[t]) {
                        case BUILTIN_ID:
                            break;
                        case BUILTIN_DROP:
                            fprintf(output.file, "    mov rax, [rbx]\n");
                            fprintf(output.file, "    lea rbx, [rbx+8]\n");
                            break;
                        case BUILTIN_DUP:
                            fprintf(output.file, "    lea rbx, [rbx-8]\n");
                            fprintf(output.file, "    mov [rbx], rax\n");
                            break;
                        case BUILTIN_SWAP:
                            fprintf(output.file, "    mov rcx, [rbx]\n");
                            fprintf(output.file, "    mov [rbx], rax\n");
                            fprintf(output.file, "    mov rax, rcx\n");
                            break;
                        case BUILTIN_DIP:
                            fprintf(output.file, "    push rax\n");
                            fprintf(output.file, "    mov rax, [rbx]\n");
                            fprintf(output.file, "    lea rbx, [rbx+8]\n");
                            output_asm_block(args[0]);
                            fprintf(output.file, "    lea rbx, [rbx-8]\n");
                            fprintf(output.file, "    mov [rbx], rax\n");
                            fprintf(output.file, "    pop rax\n");
                            break;
                        case BUILTIN_IF:
                            {
                                int l1 = output.fresh++;
                                int l2 = output.fresh++;
                                fprintf(output.file, "    test rax, rax\n");
                                fprintf(output.file, "    jz .L%d\n", l1);
                                fprintf(output.file, "    mov rax, [rbx]\n");
                                fprintf(output.file, "    lea rbx, [rbx+8]\n");
                                output_asm_block(args[0]);
                                fprintf(output.file, "    jmp .L%d\n", l2);
                                fprintf(output.file, ".L%d:\n", l1);
                                fprintf(output.file, "    mov rax, [rbx]\n");
                                fprintf(output.file, "    lea rbx, [rbx+8]\n");
                                    // read manual to figure out if can do
                                    // these moves before jumping
                                output_asm_block(args[1]);
                                fprintf(output.file, ".L%d:\n", l2);
                            }
                            break;
                        case BUILTIN_PANIC:
                            fprintf(output.file, "    mov rax, 0x2000001\n"); // exit syscall
                            fprintf(output.file, "    mov rdi, 1\n");
                            fprintf(output.file, "    syscall\n");
                            break;

                        default:
                            if (defs.pc[tokens.value[t]]) {
                                const char* unmangled_name = symbols.name[tokens.value[t]].data;
                                mangle(mangled_name, unmangled_name);
                                fprintf(output.file, "    call w_%s\n", mangled_name);
                            } else {
                                fprintf(stderr, "%s:%d:%d: warning: asm not implemented\n",
                                    command.path, tokens.row[t], tokens.col[t]);
                            }
                            break;
                    }
                    break;
                }

            default:
                break;
        }
    }
}

static void output_asm (struct value_t path_value) {
    if (path_value.type != TYPE_STR) {
        fprintf(stderr, "%s:%d:%d: error: output-asm expects a string\n",
            command.path, tokens.row[state.pc], tokens.col[state.pc]);
        exit(ERROR_TYPE);
    }
    const char* path = &strings.data[path_value.data];
    output.file = fopen(path, "w");
    output.fresh = 0;
    fprintf(output.file, "bits 64\n");
    fprintf(output.file, "section .text\n");
    fprintf(output.file, "global start\n");
    fprintf(output.file, "start:\n");
    fprintf(output.file, "    lea rbx, [rel vs+0x10000]\n");
    fprintf(output.file, "    ; add start code here\n");
    fprintf(output.file, "    mov rax, 0x2000001\n");
    fprintf(output.file, "    mov rdi, 0\n");
    fprintf(output.file, "    syscall\n");

    char mangled_name[NAME_SIZE*4+1];
    for (int sym = 0; sym < symbols.length; sym++) {
        if (defs.pc[sym]) {
            const char* unmangled_name = symbols.name[sym].data;
            mangle(mangled_name, unmangled_name);
            fprintf(output.file, "w_%s:\n", mangled_name);
            output_asm_block(defs.pc[sym]);
            fprintf(output.file, "    ret\n");
        }
    }
    fprintf(output.file, "section .data\n");
    fprintf(output.file, "    vc: dq 0x10000\n");
    fprintf(output.file, "    strings: db ");
    for (int i = 0; i < strings.length; i++) {
        fprintf(output.file, "0%.2Xh, ", strings.data[i]);
    }
    fprintf(output.file, "0\n");
    fprintf(output.file, "section .bss\n");
    fprintf(output.file, "    vs: resq 0x10020\n");
    fclose(output.file);
}

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
                            uint32_t escape_char;
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
                                            case '\\':
                                                strings.data[i++] = '\\';
                                                break;

                                            case '\"':
                                                strings.data[i++] = '\"';
                                                break;

                                            case '\'':
                                                strings.data[i++] = '\'';
                                                break;

                                            case 'n': // newline
                                                strings.data[i++] = '\n';
                                                break;

                                            case 't': // horizontal tab
                                                strings.data[i++] = '\t';
                                                break;

                                            case 'r': // carriage return
                                                strings.data[i++] = '\r';
                                                break;

                                            case 'v': // vertical tab
                                                strings.data[i++] = '\v';
                                                break;

                                            case 'e': // escape
                                                strings.data[i++] = 0x1B;
                                                break;

                                            case 'a': // alert
                                                strings.data[i++] = '\a';
                                                break;

                                            case 'b': // backspace
                                                strings.data[i++] = '\b';
                                                break;

                                            case 'x': // raw byte
                                                escape_char = 0;
                                                for (int j = 0; j < 2; j++) {
                                                    line++;
                                                    lexer.col++;
                                                    switch (*line) {
                                                        case '0': case '1': case '2':
                                                        case '3': case '4': case '5':
                                                        case '6': case '7': case '8':
                                                        case '9':
                                                            escape_char *= 16;
                                                            escape_char += *line - '0';
                                                            break;

                                                        case 'a': case 'b': case 'c':
                                                        case 'd': case 'e': case 'f':
                                                            escape_char *= 16;
                                                            escape_char = *line + 10 - 'a';
                                                            break;

                                                        case 'A': case 'B': case 'C':
                                                        case 'D': case 'E': case 'F':
                                                            escape_char *= 16;
                                                            escape_char = *line + 10 - 'a';
                                                            break;

                                                        default:
                                                            fprintf(stderr, "%s:%d:%d: error: unrecognized character escape sequence", command.path, lexer.row, lexer.col);
                                                            return ERROR_SYNTAX;
                                                    }
                                                }
                                                strings.data[i++] = escape_char;
                                                break;

                                            default:
                                                fprintf(stderr, "%s:%d:%d: error: unrecognized character escape sequence \"\\%c\"\n", command.path, lexer.row, lexer.col, *line);
                                                return ERROR_SYNTAX;
                                        }
                                        line++;
                                        lexer.col++;
                                        break;
                                    default:
                                        strings.data[i++] = *line;
                                        line++;
                                        lexer.col++;
                                        break;
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
                        bool has_digits = false;
                        bool only_digits = true;
                        bool has_hexdigits = false;
                        bool only_hexdigits = true;
                        uint64_t decimal_value = 0;
                        uint64_t hexadecimal_value = 0;

                        switch (*line) {
                            case '+': line++; lexer.col++; break;
                            case '-': positive = false; line++; lexer.col++; break;
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
                                    has_digits = true;
                                    has_hexdigits = true;
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
                                    has_hexdigits = true;
                                    break;

                                case 'A': case 'B':
                                case 'C': case 'D':
                                case 'E': case 'F':
                                    hexadecimal_value *= 16;
                                    hexadecimal_value += *line - 'A' + 10;
                                    line++;
                                    lexer.col++;
                                    only_digits = false;
                                    has_hexdigits = true;
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
                        if (hexadecimal && only_hexdigits && has_hexdigits) {
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

                        } else if (only_digits && has_digits) {
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
        state.pc = 0;
        state.sc = STACK_SIZE;
        state.rc = RSTACK_SIZE;
        state.fc = FSTACK_SIZE;
        state.fi = FSTACK_SIZE;

        #define arity_check(word,num_params,num_in,num_out) \
            if (num_args != num_params) { \
                fprintf(stderr, "%s:%d:%d: error: %s expects %d arguments, got %d\n", \
                    command.path, tokens.row[state.pc], tokens.col[state.pc], \
                    word, num_params, num_args); \
                return ERROR_ARITY; \
            } \
            if (state.sc+num_in > STACK_SIZE) { \
                fprintf(stderr, "%s:%d:%d: error: stack underflow in %s\n", \
                    command.path, tokens.row[state.pc], tokens.col[state.pc], word); \
                return ERROR_UNDERFLOW; \
            } \
            if (state.sc+num_in < num_out) { \
                fprintf(stderr, "%s:%d:%d: error: stack overflow in %s\n", \
                    command.path, tokens.row[state.pc], tokens.col[state.pc], word); \
                return ERROR_OVERFLOW; \
            }
        struct value_t a;
        struct fvalue_t f;
        int16_t next_pc, num_args, saved_fc;
        for (; state.pc < tokens.length; state.pc++) {
        resume_loop:
            switch (tokens.kind[state.pc]) {
                case TOKEN_NONE:
                case TOKEN_RPAREN:
                case TOKEN_COMMA:
                case TOKEN_COLON:
                    goto pop_rstack;
                case TOKEN_NEWLINE:
                    break;
                case TOKEN_LPAREN:
                    state.pc = tokens.value[state.pc];
                    break;
                case TOKEN_INT:
                    a.type = TYPE_INT;
                    a.data = tokens.value[state.pc];
                    state.stack[--state.sc] = a;
                    break;
                case TOKEN_STR:
                    a.type = TYPE_STR;
                    a.data = tokens.value[state.pc];
                    state.stack[--state.sc] = a;
                    #ifdef WITH_PREFETCHING
                        __builtin_prefetch(&strings.data[tokens.value[state.pc]]);
                    #endif
                    break;
                case TOKEN_WORD:
                    next_pc = state.pc+1;
                    saved_fc = state.fc;
                    num_args = 0;
                    if (tokens.kind[state.pc+1] == TOKEN_LPAREN) { // determine arguments to word
                        bool is_empty_arg = true;
                        next_pc = tokens.value[state.pc+1]+1;
                        if (state.fc < 1) {
                            fprintf(stderr, "%s:%d:%d: error: fstack ran out, increase FSTACK_SIZE\n",
                                command.path, tokens.row[state.pc], tokens.col[state.pc]);
                            return ERROR_OVERFLOW;
                        }
                        num_args += 1;
                        f.fi = state.fi;
                        f.pc = state.pc+2;
                        state.fstack[--state.fc] = f;
                        for (int i = state.pc+2; i < next_pc; i++) {
                            switch (tokens.kind[i]) {
                                case TOKEN_COMMA:
                                    if (state.fc < 1) {
                                        fprintf(stderr, "%s:%d:%d: error: fstack ran out, increase FSTACK_SIZE\n",
                                            command.path, tokens.row[i], tokens.col[i]);
                                        return ERROR_OVERFLOW;
                                    }
                                    num_args += 1;
                                    f.pc = i+1;
                                    state.fstack[--state.fc] = f;
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
                            state.fc++;
                        }
                    }

                    switch (tokens.value[state.pc]) {
                        case BUILTIN_END:
                            fprintf(stderr, "%s:%d:%d: error: unexpected \"end\"\n",
                                command.path, tokens.row[state.pc], tokens.col[state.pc]);
                            return ERROR_SYNTAX;
                        case BUILTIN_ID:
                            arity_check("id", 0,0,0);
                            break;
                        case BUILTIN_DIP:
                            arity_check("dip", 1, 1, 1);
                            if (state.rc < 3) {
                                fprintf(stderr, "%s:%d:%d: error: rstack ran out, increase RSTACK_SIZE\n",
                                    command.path, tokens.row[state.pc], tokens.col[state.pc]);
                                return ERROR_OVERFLOW;
                            }
                            state.fi = state.fstack[state.fc].fi;
                            state.pc = state.fstack[state.fc++].pc;
                            a.type = TYPE_PC;
                            a.data = next_pc;
                            state.rstack[--state.rc] = a;
                            a.type = TYPE_FC;
                            a.data = saved_fc;
                            state.rstack[--state.rc] = a;
                            state.rstack[--state.rc] = state.stack[state.sc++];
                            goto resume_loop;
                        case BUILTIN_IF:
                            arity_check("if", 2, 1, 0);
                            if (state.rc < 2) {
                                fprintf(stderr, "%s:%d:%d: error: rstack ran out, increase RSTACK_SIZE\n",
                                    command.path, tokens.row[state.pc], tokens.col[state.pc]);
                                return ERROR_OVERFLOW;
                            }
                            a.type = TYPE_PC;
                            a.data = next_pc;
                            state.rstack[--state.rc] = a;
                            a = state.stack[state.sc++];
                            state.pc = a.data ? state.fstack[state.fc+1].pc : state.fstack[state.fc].pc;
                            state.fc += 2;
                            goto resume_loop;
                        case BUILTIN_PANIC:
                            if (state.sc < STACK_SIZE && state.stack[state.sc].type == TYPE_STR) {
                                fprintf(stderr, "%s:%d:%d: error: panic: %s\n",
                                    command.path, tokens.row[state.pc], tokens.col[state.pc],
                                    &strings.data[state.stack[state.sc++].data]);
                                fprintf(stderr, "stack: ");
                                fprint_stack(stderr);
                                return ERROR_PANIC;
                            } else {
                                fprintf(stderr, "%s:%d:%d: error: panic\n",
                                    command.path, tokens.row[state.pc], tokens.col[state.pc]);
                                fprintf(stderr, "stack: ");
                                fprint_stack(stderr);
                                return ERROR_PANIC;
                            }
                        case BUILTIN_DUP:
                            arity_check("dup", 0, 1, 2);
                            a = state.stack[state.sc];
                            state.stack[--state.sc] = a;
                            break;
                        case BUILTIN_DROP:
                            arity_check("drop", 0, 1, 0);
                            state.sc++;
                            break;
                        case BUILTIN_SWAP:
                            arity_check("swap", 0, 2, 2);
                            a = state.stack[state.sc];
                            state.stack[state.sc] = state.stack[state.sc+1];
                            state.stack[state.sc+1] = a;
                            break;
                        case BUILTIN_DEF:
                            arity_check("def",2+(num_args>2),0,0);
                            {
                                uint16_t name = state.fstack[state.fc+1+(num_args>2)].pc;
                                // uint16_t type = saved_fc[1].pc;
                                uint16_t body = state.fstack[state.fc+0].pc;
                                if (tokens.kind[name] == TOKEN_WORD) {
                                    uint16_t w = tokens.value[name];
                                    defs.pc[w] = body;
                                } else {
                                    fprintf(stderr, "%s:%d:%d: error: expected word\n",
                                        command.path, tokens.row[name], tokens.col[name]);
                                }
                                state.fc = saved_fc;
                                state.pc = next_pc;
                                goto resume_loop;
                            }
                        case BUILTIN_OUTPUT_ASM:
                            arity_check("output-asm",0,1,0);
                            a = state.stack[state.sc++];
                            output_asm(a);
                            break;
                        default:
                            if (defs.pc[tokens.value[state.pc]]) {
                                if (state.rc < 3) {
                                    fprintf(stderr, "%s:%d:%d: error: rstack ran out, increase RSTACK_SIZE\n",
                                        command.path, tokens.row[state.pc], tokens.col[state.pc]);
                                    return ERROR_OVERFLOW;
                                }
                                int jumpto = defs.pc[tokens.value[state.pc]];
                                a.type = TYPE_PC;
                                a.data = next_pc;
                                state.rstack[--state.rc] = a;
                                a.type = TYPE_FC;
                                a.data = saved_fc;
                                state.rstack[--state.rc] = a;
                                a.type = TYPE_FI;
                                a.data = state.fi;
                                state.rstack[--state.rc] = a;
                                state.fi = state.fc;
                                state.pc = jumpto;
                                goto resume_loop;
                            } else {
                                fprintf(stderr, "%s:%d:%d: error: undefined word \"%s\"\n",
                                    command.path, tokens.row[state.pc], tokens.col[state.pc],
                                    symbols.name[tokens.value[state.pc]].data);
                                return ERROR_UNDEFINED;
                            }
                    }
                    break;

            }
        }

        pop_rstack:
        if (state.rc < RSTACK_SIZE) {
            switch (state.rstack[state.rc].type) {
                case TYPE_NIL:
                case TYPE_INT:
                case TYPE_STR:
                    if (state.sc < 1) {
                        fprintf(stderr, "%s:%d:%d: error: stack overflow when pushing from return stack",
                            command.path, tokens.row[state.pc], tokens.col[state.pc]);
                        return ERROR_OVERFLOW;
                    }
                    state.stack[--state.sc] = state.rstack[state.rc++];
                    goto pop_rstack;

                case TYPE_FI:
                    state.fi = state.rstack[state.rc++].data;
                    goto pop_rstack;

                case TYPE_FC:
                    state.fc = state.rstack[state.rc++].data;
                    goto pop_rstack;

                case TYPE_PC:
                    state.pc = state.rstack[state.rc++].data;
                    goto resume_loop;
            }
        }

        // display stack
        if (state.sc < STACK_SIZE) {
            fprint_stack(stderr);
        }
    }

    return 0;
}
