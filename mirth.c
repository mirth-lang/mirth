#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define ASSERT(b, error_code, file, line, col, msg) \
    do { \
        if (!(b)) { \
            fprintf(stderr, "%s:%d:%d: %s: %s\n", file, line, col, (error_code ? "error" : "warning"), msg); \
            if (error_code) exit(error_code); \
        } \
    } while(0)

#define ASSERT_LEXER(b, error, msg) \
    ASSERT(b, error, command.path, lexer.row, lexer.col, msg)
#define ASSERT_TOKEN(b, error, t, msg) \
    ASSERT(b, error, command.path, tokens.row[t], tokens.col[t], msg)
#define ASSERT_BASIC(b) \
    ASSERT(b, ERROR_IMPOSSIBLE, __FILE__, __LINE__, 1, "assertion failed")

enum error_t {
    WARNING = 0,
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
#define NAME_SIZE 0x20

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
    int row [TOKENS_SIZE];
    int col [TOKENS_SIZE];
    int depth [TOKENS_SIZE];
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
    uint64_t value [TOKENS_SIZE];
} tokens = {0};

// String table
#define STRINGS_SIZE 0x4000
struct strings_t {
    uint64_t length;
    char data [STRINGS_SIZE];
} strings = {0};

// Lexer state
#define LEXER_LINE_MAX 0x200
#define LEXER_STACK_SIZE 0x80
struct lexer_t {
    FILE* file;
    int row;
    int col;
    int depth;
    uint64_t stack [LEXER_STACK_SIZE];
    char line [LEXER_LINE_MAX];
} lexer = {0};

// Definitions
struct defs_t {
    int64_t pc[SYMBOLS_SIZE];
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
    int64_t data;
};

struct __attribute__((packed)) fvalue_t {
    int64_t pc;
    int64_t fi;
};

// intepreter state
#define STACK_SIZE 0x400
#define RSTACK_SIZE 0x100
#define FSTACK_SIZE 0x400
struct __attribute__((packed)) state_t {
    uint64_t pc; // program counter
    uint64_t sc; // stack counter
    uint64_t rc; // return stack counter
    uint64_t fc; // frame stack counter
    uint64_t fi; // frame pointer
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
            fprintf(fp, "%lld", value.data);
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
                fprintf(output.file, "    mov rax, %lld\n", tokens.value[t]);
                break;

            case TOKEN_STR:
                fprintf(output.file, "    lea rbx, [rbx-8]\n");
                fprintf(output.file, "    mov [rbx], rax\n");
                fprintf(output.file, "    lea rax, [rel strings+%lld]\n", tokens.value[t]);
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
                            ASSERT_TOKEN(defs.pc[tokens.value[t]] != 0, ERROR_NOT_IMPLEMENTED, t,
                                "not implemented in output-asm");
                            const char* unmangled_name = symbols.name[tokens.value[t]].data;
                            mangle(mangled_name, unmangled_name);
                            fprintf(output.file, "    call w_%s\n", mangled_name);
                            break;
                    }
                    break;
                }

            default:
                break;
        }
    }
}

static void output_asm (struct value_t path_value, size_t pc) {
    ASSERT_TOKEN(path_value.type == TYPE_STR, ERROR_TYPE, state.pc,
        "output-asm expects a string.");
    const char* path = &strings.data[path_value.data];
    output.file = fopen(path, "w");
    output.fresh = 0;
    fprintf(output.file, "bits 64\n");
    fprintf(output.file, "section .text\n");
    fprintf(output.file, "global start\n");
    fprintf(output.file, "start:\n");
    fprintf(output.file, "    lea rbx, [rel vs+0x10000]\n");
    output_asm_block(pc);
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
            ASSERT_LEXER(!ferror(lexer.file), ERROR_FILE_IO, "Error while reading file.\n");
            if (feof(lexer.file)) break;
            ASSERT_BASIC (line != NULL);
            lexer.row ++;
            lexer.col = 1;

            while (*line) {
                const char* token_start;
                uint64_t token_size;
                int token_row = lexer.row;
                int token_col = lexer.col;

                uint64_t t = tokens.length;
                uint64_t t0;
                ASSERT_LEXER(t+1 < TOKENS_SIZE, ERROR_OVERFLOW,
                    "Ran out of token buffer. Increase TOKENS_SIZE.");

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
                        tokens.kind[tokens.length++] = TOKEN_NEWLINE;
                        line++;
                        break;

                    case '#': // just ignore comments.
                        while (*line != 0 && *line != '\n')
                            line++;
                        break;

                    case '(':
                        ASSERT_LEXER(lexer.depth+1 < LEXER_STACK_SIZE, ERROR_OVERFLOW,
                            "Ran out of lexer stack. Increase LEXER_STACK_SIZE.");
                        lexer.stack[lexer.depth++] = t;
                        tokens.kind[tokens.length++] = TOKEN_LPAREN;
                        lexer.col++;
                        line++;
                        break;

                    case ')':
                        ASSERT_LEXER(lexer.depth > 0, ERROR_SYNTAX,
                            "Right paren without matching left paren.");
                        t0 = lexer.stack[--lexer.depth];
                        ASSERT_LEXER(tokens.kind[t0] == TOKEN_LPAREN, ERROR_SYNTAX,
                            "Right paren without matching left paren.");
                        tokens.value[t0] = t;
                        tokens.value[t] = t0;
                        tokens.kind[tokens.length++] = TOKEN_RPAREN;
                        lexer.col++;
                        line++;
                        break;

                    case ',':
                        tokens.kind[tokens.length++] = TOKEN_COMMA;
                        lexer.col++;
                        line++;
                        break;

                    case ':':
                        tokens.kind[tokens.length++] = TOKEN_COLON;
                        lexer.col++;
                        line++;
                        break;

                    case '\"':
                        { // lex a string
                            size_t n = strings.length;
                            uint64_t escape_char;
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
                                                            ASSERT_LEXER(false, ERROR_SYNTAX,
                                                                "Unrecognized character escape sequence.");
                                                    }
                                                }
                                                strings.data[i++] = escape_char;
                                                break;

                                            default:
                                                ASSERT_LEXER(false, ERROR_SYNTAX,
                                                    "Unrecognized character escape sequence.");
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
                            ASSERT_LEXER(*line == '\"', ERROR_SYNTAX,
                                "Unrecognized newline in string literal.");
                            line++;
                            lexer.col++;
                            strings.data[i++] = 0;
                            tokens.value[t] = n;
                            tokens.kind[tokens.length++] = TOKEN_STR;
                            strings.length = i;
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
                                tokens.value[t] = hexadecimal_value;
                            } else {
                                tokens.value[t] = -(int64_t)hexadecimal_value;
                            }
                            tokens.kind[tokens.length++] = TOKEN_INT;

                        } else if (only_digits && has_digits) {
                            if (positive) {
                                tokens.value[t] = decimal_value;
                            } else {
                                tokens.value[t] = -(int64_t)decimal_value;
                            }
                            tokens.kind[tokens.length++] = TOKEN_INT;
                        } else {
                            token_size = line - token_start;
                            ASSERT_LEXER(token_size < NAME_SIZE, ERROR_OVERFLOW,
                                "Name too large. Increase NAME_SIZE or shorten name.");
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
                                ASSERT_LEXER(symbols.length < SYMBOLS_SIZE-1, ERROR_OVERFLOW,
                                    "Ran out of symbol buffer. Increase SYMBOLS_SIZE.");
                                memcpy(symbols.name[symbols.length].data, buf, NAME_SIZE);
                                tokens.value[t] = symbols.length++;
                            }
                            tokens.kind[tokens.length++] = TOKEN_WORD;
                        }
                        break;
                }
            }
        }
        if ((tokens.length == 0) || (tokens.kind[tokens.length - 1] != TOKEN_NEWLINE)) {
            ASSERT_LEXER(false, WARNING, "Missing newline at end of file.");
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
            do { \
                ASSERT_TOKEN(num_args == num_params, ERROR_ARITY, state.pc, "Wrong number of arguments."); \
                ASSERT_TOKEN(state.sc <= STACK_SIZE - num_in, ERROR_UNDERFLOW, state.pc, "Stack underflow."); \
                ASSERT_TOKEN(state.sc >= num_out, ERROR_OVERFLOW, state.pc, "Stack overflow."); \
            } while(0)
        struct value_t a;
        struct fvalue_t f;
        int64_t next_pc, num_args, saved_fc;
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
                    break;
                case TOKEN_WORD:
                    next_pc = state.pc+1;
                    saved_fc = state.fc;
                    num_args = 0;
                    if (tokens.kind[state.pc+1] == TOKEN_LPAREN) { // determine arguments to word
                        next_pc = tokens.value[state.pc+1]+1;
                        ASSERT_TOKEN(state.fc >= 1, ERROR_OVERFLOW, state.pc,
                            "fstack ran out, increase FSTACK_SIZE.");
                        num_args += 1;
                        f.fi = state.fi;
                        f.pc = state.pc+2;
                        state.fstack[--state.fc] = f;
                        for (int i = state.pc+2; i < next_pc; i++) {
                            switch (tokens.kind[i]) {
                                case TOKEN_COMMA:
                                    ASSERT_TOKEN(state.fc >= 1, ERROR_OVERFLOW, state.pc,
                                        "fstack ran out, increase FSTACK_SIZE.");
                                    num_args += 1;
                                    f.pc = i+1;
                                    state.fstack[--state.fc] = f;
                                    break;

                                case TOKEN_LPAREN:
                                    i = tokens.value[i];
                                    break;

                                default:
                                    break;
                            }
                        }
                    }

                    switch (tokens.value[state.pc]) {
                        case BUILTIN_END:
                            ASSERT_TOKEN(false, ERROR_SYNTAX, state.pc,
                                "Unexpected \"end\".");
                        case BUILTIN_ID:
                            arity_check("id", 0,0,0);
                            break;
                        case BUILTIN_DIP:
                            arity_check("dip", 1, 1, 1);
                            ASSERT_TOKEN(state.rc >= 3, ERROR_OVERFLOW, state.pc,
                                "rstack ran out, increase RSTACK_SIZE.");
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
                            ASSERT_TOKEN(state.rc >= 2, ERROR_OVERFLOW, state.rc,
                                "rstack ran out, increase RSTACK_SIZE.");
                            a.type = TYPE_PC;
                            a.data = next_pc;
                            state.rstack[--state.rc] = a;
                            a = state.stack[state.sc++];
                            state.pc = a.data ? state.fstack[state.fc+1].pc : state.fstack[state.fc].pc;
                            state.fc += 2;
                            goto resume_loop;
                        case BUILTIN_PANIC:
                            arity_check("panic", 0, 1, 0);
                            if (state.stack[state.sc].type == TYPE_STR) {
                                fprintf(stderr, "%s:%d:%d: error: panic: %s\n",
                                    command.path, tokens.row[state.pc], tokens.col[state.pc],
                                    &strings.data[state.stack[state.sc++].data]);
                                fprintf(stderr, "stack: ");
                                fprint_stack(stderr);
                                return ERROR_PANIC;
                            } else {
                                fprintf(stderr, "%s:%d:%d: error: panic expected message\n",
                                    command.path, tokens.row[state.pc], tokens.col[state.pc]);
                                fprintf(stderr, "stack: ");
                                fprint_stack(stderr);
                                return ERROR_TYPE;
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
                                uint64_t name = state.fstack[state.fc+1+(num_args>2)].pc;
                                // uint64_t type = saved_fc[1].pc;
                                uint64_t body = state.fstack[state.fc+0].pc;
                                ASSERT_TOKEN(tokens.kind[name] == TOKEN_WORD, ERROR_SYNTAX, name,
                                    "Expected word.");
                                uint64_t w = tokens.value[name];
                                defs.pc[w] = body;
                                state.fc = saved_fc;
                                state.pc = next_pc;
                                goto resume_loop;
                            }
                        case BUILTIN_OUTPUT_ASM:
                            arity_check("output-asm",1,1,0);
                            a = state.stack[state.sc++];
                            output_asm(a, state.fstack[state.fc].pc);
                            state.fc = saved_fc;
                            state.pc = next_pc;
                            break;
                        default:
                            if (defs.pc[tokens.value[state.pc]]) {
                                ASSERT_TOKEN(state.rc >= 3, ERROR_OVERFLOW, state.pc,
                                    "rstack ran out, increase RSTACK_SIZE.");
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
                                exit(ERROR_UNDEFINED);
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
                    ASSERT_TOKEN(state.sc >= 1, ERROR_OVERFLOW, state.pc,
                        "Stack overflow when pushing from return stack.");
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
