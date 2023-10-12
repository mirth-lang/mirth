/* MIRTH HEADER */

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__)
#define MIRTH_WINDOWS 1
#elif defined(__linux__)
#define MIRTH_LINUX 1
#elif defined(__APPLE__)
#define MIRTH_MACOS 1
#else
#error "Platform not supported."
#endif

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

extern void* malloc(size_t);
extern void* calloc(size_t, size_t);
extern void* realloc(void*, size_t);
extern void* memset(void*, int, size_t);
extern void* memcpy(void*, const void*, size_t);
extern int memcmp(const void*, const void*, size_t);
extern int strcmp(const char*, const char*);
extern size_t strlen(const char*);
extern void free(void*);
extern int read(int, void*, size_t);
extern int write(int, const char*, size_t);
extern int close(int);
extern int open(const char*, int, int);
extern void exit(int);

#define HAS_REFS_FLAG 0x8000
typedef enum TAG {
    // TODO: TAG_NIL
    // TODO: TAG_PTR
    TAG_INT  = 1,
    TAG_CONS = 2 | HAS_REFS_FLAG,
    TAG_STR  = 3 | HAS_REFS_FLAG,
} TAG;

typedef void (*fnptr)(void);

typedef uint32_t REFS;
typedef uint64_t USIZE;

typedef union DATA {
    USIZE usize;
    uint64_t u64;
    uint32_t u32;
    uint16_t u16;
    uint8_t u8;
    int64_t i64;
    int32_t i32;
    int16_t i16;
    int8_t i8;
    void* ptr;
    void (*fnptr)(void);
    struct VAL* valptr;
    char* charptr;
    REFS* refs;
    struct CONS* cons;
    struct STR* str;
} DATA;

typedef struct VAL {
    DATA data;
    TAG tag;
} VAL;

#define VREFS(v)  (*(v).data.refs)
#define VINT(v)   ((v).data.i64)
#define VI64(v)   ((v).data.i64)
#define VU64(v)   ((v).data.u64)
#define VPTR(v)   ((v).data.ptr)
#define VFNPTR(v) ((v).data.fnptr)
#define VSTR(v)   ((v).data.str)
#define VCONS(v)  ((v).data.cons)

#define HAS_REFS(v) ((v).tag & HAS_REFS_FLAG)
#define IS_INT(v)   ((v).tag == TAG_INT)
#define IS_U64(v)   ((v).tag == TAG_INT)
#define IS_I64(v)   ((v).tag == TAG_INT)
#define IS_PTR(v)   ((v).tag == TAG_INT)
#define IS_FNPTR(v) ((v).tag == TAG_INT)
#define IS_STR(v)   ((v).tag == TAG_STR)
#define IS_CONS(v)  ((v).tag == TAG_CONS)
#define IS_NIL(v)   (((v).tag == TAG_INT) && ((v).data.i64 == 0))

#define MKINT(x)   ((VAL){.tag=TAG_INT,  .data={.i64=(x)}})
#define MKI64(x)   ((VAL){.tag=TAG_INT,  .data={.i64=(x)}})
#define MKU64(x)   ((VAL){.tag=TAG_INT,  .data={.u64=(x)}})
#define MKFNPTR(x) ((VAL){.tag=TAG_INT,  .data={.fnptr=(x)}})
#define MKPTR(x)   ((VAL){.tag=TAG_INT,  .data={.ptr=(x)}})
#define MKSTR(x)   ((VAL){.tag=TAG_STR,  .data={.str=(x)}})
#define MKCONS(x)  ((VAL){.tag=TAG_CONS, .data={.cons=(x)}})
#define MKNIL()    ((VAL){.tag=TAG_INT,  .data={.i64=0}})

typedef struct CONS {
    REFS refs;
    VAL car;
    VAL cdr;
} CONS;

typedef struct STR {
    REFS refs;
    USIZE cap;
    USIZE size;
    char data[];
} STR;

#define STACK_MAX 0x8000
static USIZE stack_counter = STACK_MAX;
static VAL stack [STACK_MAX] = {0};
static int global_argc;
static char** global_argv;

static void push_value(VAL v);
static void mw_prim_debug(void);

#define EXPECT(test,msg) \
    do { \
        if (!(test)) { \
            write(2, msg "\n", strlen(msg "\n")); \
            mw_prim_debug(); \
            exit(1); \
        } \
    } while(0)

#define EXPECT1(test,msg,v1) \
    do { \
        if (!(test)) { \
            write(2, msg "\n", strlen(msg "\n")); \
            push_value(v1); \
            mw_prim_debug(); \
            exit(1); \
        } \
    } while(0)

#define EXPECT2(test,msg,v1,v2) \
    do { \
        if (!(test)) { \
            write(2, msg "\n", strlen(msg "\n")); \
            push_value(v1); \
            push_value(v2); \
            mw_prim_debug(); \
            exit(1); \
        } \
    } while(0)

#define ASSERT(test) \
    EXPECT(test, "assertion failed (" #test ")")
#define ASSERT1(test,v) \
    EXPECT1(test,  "assertion failed (" #test ")", v)
#define ASSERT2(test,v1,v2) \
    EXPECT2(test,  "assertion failed (" #test ")", v1, v2)

static void free_value(VAL v);

static void incref(VAL v) {
    if (HAS_REFS(v)) {
        VREFS(v)++;
    }
}

static void decref(VAL v) {
    if (HAS_REFS(v)) {
        if(--VREFS(v) == 0) {
            free_value(v);
        }
    }
}

static void free_value(VAL v) {
    ASSERT(HAS_REFS(v));
    ASSERT(VREFS(v) == 0);
    ASSERT1(IS_CONS(v)||IS_STR(v), v);
    if (IS_CONS(v)) {
        CONS* cons = VCONS(v);
        ASSERT(cons);
        decref(cons->car);
        decref(cons->cdr);
        free(cons);
    } else if (IS_STR(v)) {
        STR* str = VSTR(v);
        ASSERT(str);
        free(str);
    }
}

static void value_uncons(VAL val, VAL* car, VAL* cdr) {
    if (IS_CONS(val)) {
        CONS* cons = VCONS(val);
        *car = cons->car;
        *cdr = cons->cdr;
    } else {
        *car = MKNIL();
        *cdr = val;
    }
}

static void* value_ptr (VAL v) {
    ASSERT(IS_PTR(v));
    return VPTR(v);
}

#define pop_fnptr() VFNPTR(pop_value())
#define pop_u8() ((uint8_t)VU64(pop_value()))
#define pop_u16() ((uint16_t)VU64(pop_value()))
#define pop_u32() ((uint32_t)VU64(pop_value()))
#define pop_u64() (VU64(pop_value()))
#define pop_i8() ((int8_t)VI64(pop_value()))
#define pop_i16() ((int16_t)VI64(pop_value()))
#define pop_i32() ((int32_t)VI64(pop_value()))
#define pop_i64() (VI64(pop_value()))
#define pop_usize() (VU64(pop_value()))
#define pop_bool() ((bool)VU64(pop_value()))
#define pop_ptr() (VPTR(pop_value()))

#define push_u64(v) push_value(MKU64(v))
#define push_i64(v) push_value(MKI64(v))
#define push_usize(v) push_u64((uint64_t)(v))
#define push_fnptr(v) push_u64((uint64_t)(v))
#define push_bool(b) push_u64((uint64_t)((bool)(b)))
#define push_u8(b) push_u64((uint64_t)(b))
#define push_u16(b) push_u64((uint64_t)(b))
#define push_u32(b) push_u64((uint64_t)(b))
#define push_i8(b) push_i64((int64_t)(b))
#define push_i16(b) push_i64((int64_t)(b))
#define push_i32(b) push_i64((int64_t)(b))
#define push_ptr(v) push_u64((uint64_t)(void*)(v))

static void push_value(VAL x) {
    ASSERT(stack_counter > 0);
    stack[--stack_counter] = x;
}

static VAL top_value(void) {
    ASSERT(stack_counter < STACK_MAX);
    return stack[stack_counter];
}

static VAL pop_value(void) {
    ASSERT(stack_counter < STACK_MAX);
    return stack[stack_counter++];
}

static VAL mkcons (VAL car, VAL cdr) {
    if (IS_NIL(car) && !IS_CONS(cdr))
        return cdr;
    CONS *cons = calloc(1, sizeof(CONS));
    EXPECT(cons, "failed to allocate a cons cell");
    cons->refs = 1;
    cons->car = car;
    cons->cdr = cdr;
    return MKCONS(cons);
}

static STR* str_alloc (USIZE cap) {
    ASSERT(cap <= SIZE_MAX - sizeof(STR) - 4);
    STR* str = calloc(1, (size_t)(cap + sizeof(STR) + 4));
    EXPECT(str, "failed to allocate string");
    str->refs = 1;
    str->cap = cap;
    return str;
}

static VAL mkstr (const char* data, USIZE size) {
    ASSERT(data);
    ASSERT(size <= SIZE_MAX - sizeof(STR) - 4);
    STR* str = str_alloc(size);
    str->size = size;
    memcpy(str->data, data, (size_t)size);
    return MKSTR(str);
}

static void do_uncons(void) {
    VAL val, car, cdr;
    val = pop_value();
    value_uncons(val, &car, &cdr);
    push_value(car);
    push_value(cdr);
    incref(car);
    incref(cdr);
    decref(val);
}

static USIZE get_data_tag(VAL v) {
    VAL car, cdr;
    value_uncons(v, &car, &cdr);
    return VU64(cdr);
}

static USIZE get_top_data_tag(void) {
    return get_data_tag(top_value());
}

static int value_cmp_(VAL v1, VAL v2) {
    while (IS_CONS(v1) || IS_CONS(v2)) {
        VAL v1car, v1cdr; value_uncons(v1, &v1car, &v1cdr);
        VAL v2car, v2cdr; value_uncons(v2, &v2car, &v2cdr);
        int r = value_cmp_(v1cdr, v2cdr);
        if (r) return r;
        v1 = v1car;
        v2 = v2car;
    }
    if (IS_INT(v1) && IS_INT(v2)) {
        if (VINT(v1) < VINT(v2)) return -1;
        if (VINT(v1) > VINT(v2)) return 1;
        return 0;
    } else if (IS_STR(v1) && IS_STR(v2)) {
        ASSERT2(VSTR(v1) && VSTR(v2), v1, v2);
        USIZE n1 = VSTR(v1)->size;
        USIZE n2 = VSTR(v2)->size;
        USIZE n = (n1 < n2 ? n1 : n2);
        ASSERT(n < SIZE_MAX);
        int r = memcmp(VSTR(v1)->data, VSTR(v2)->data, (size_t)n);
        if (r) return r;
        if (n1 < n2) return -1;
        if (n1 > n2) return 1;
        return 0;
    }
    ASSERT2(0, v1, v2);
    return 0;
}

static void run_value(VAL v) {
    // TODO Make a closure tag or something.
    // As it is, this feels kinda wrong.
    VAL car, cdr;
    value_uncons(v, &car, &cdr);
    push_value(car);
    incref(car);
    decref(v);
    ASSERT(IS_FNPTR(cdr) && VFNPTR(cdr));
    VFNPTR(cdr)();
}

static void mw_prim_id (void) {}
static void mw_prim_dup (void) {
    VAL v = top_value();
    push_value(v);
    incref(v);
}
static void mw_prim_drop (void) {
    VAL v = pop_value();
    decref(v);
}

static void mw_prim_swap (void) {
    VAL a = pop_value();
    VAL b = pop_value();
    push_value(a);
    push_value(b);
}

static void mw_prim_dip (void) {
    VAL f = pop_value();
    VAL x = pop_value();
    run_value(f);
    push_value(x);
}

static void mw_prim_if (void) {
    VAL else_branch = pop_value();
    VAL then_branch = pop_value();
    bool b = pop_bool();
    if (b) {
        decref(else_branch);
        run_value(then_branch);
    } else {
        decref(then_branch);
        run_value(else_branch);
    }
}

static void mw_prim_while (void) {
    VAL body = pop_value();
    VAL cond = pop_value();
    while(1) {
        incref(cond); run_value(cond);
        bool b = pop_bool();
        if (!b) break;
        incref(body); run_value(body);
    }
    decref(cond);
    decref(body);
}

static void mw_prim_int_add (void) {
    // TODO promote to bigint on overflow.
    int64_t b = pop_i64();
    int64_t a = pop_i64();
    if (b >= 0) {
        EXPECT(a <= INT64_MAX - b, "integer overflow during addition (too positive)");
    } else {
        EXPECT(a >= INT64_MIN - b, "integer overflow during addition (too negative)");
    }
    push_i64(a + b);
}
static void mw_prim_int_sub (void) {
    // TODO promote to bigint on overflow
    int64_t b = pop_i64();
    int64_t a = pop_i64();
    if (b >= 0) {
        EXPECT(a >= INT64_MIN + b, "integer overflow during subtraction (too negative)");
    } else {
        EXPECT(a <= INT64_MAX + b, "integer overflow during subtraction (too positive)");
    }
    push_i64(a - b);
}
static void mw_prim_int_mul (void) {
    // TODO promote to bigint on overflow
    int64_t b = pop_i64();
    int64_t a = pop_i64();
    // overflow checks for multiplication
    push_i64(a * b);
}
static void mw_prim_int_div (void) {
    // TODO promote to bigint on overflow
    int64_t b = pop_i64();
    int64_t a = pop_i64();
    EXPECT(b != 0, "divide by zero");
    EXPECT(!((b == -1) && (a == INT64_MIN)), "overflow during division");
    int64_t r = a % b;
    int64_t q = a / b;
    if (((a < 0) ^ (b < 0)) && r) q--;
    push_i64(q);
}
static void mw_prim_int_mod (void) {
    int64_t b = pop_i64();
    int64_t a = pop_i64();
    EXPECT(b != 0, "divide by zero");
    if (b == -1) { push_i64(0); return; }
    int64_t r = a % b;
    int64_t q = a / b;
    if (((a < 0) ^ (b < 0)) && r) r += b;
    push_i64(r);
}

static void mw_prim_int_and (void) {
    uint64_t b = pop_u64();
    uint64_t a = pop_u64();
    push_u64(a & b);
}
static void mw_prim_int_or (void) {
    uint64_t b = pop_u64();
    uint64_t a = pop_u64();
    push_u64(a | b);
}
static void mw_prim_int_xor (void) {
    uint64_t b = pop_u64();
    uint64_t a = pop_u64();
    push_u64(a ^ b);
}
static void mw_prim_int_shl (void) {
    uint64_t b = pop_u64();
    uint64_t a = pop_u64();
    push_u64((b >= 64) ? 0 : (a << b));
}
static void mw_prim_int_shr (void) {
    uint64_t b = pop_u64();
    uint64_t a = pop_u64();
    push_u64((b >= 64) ? 0 : (a >> b));
}

static void mw_prim_bool_true (void) {
    push_bool(true);
}

static void mw_prim_bool_false (void) {
    push_bool(false);
}

static void mw_prim_bool_and (void) {
    uint64_t b = pop_bool();
    uint64_t a = pop_bool();
    push_bool(a && b);
}

static void mw_prim_bool_or (void) {
    uint64_t b = pop_bool();
    uint64_t a = pop_bool();
    push_bool(a || b);
}

static void mw_prim_value_eq (void) {
    VAL b = pop_value();
    VAL a = pop_value();
    int cmp = value_cmp_(a,b);
    push_bool(cmp == 0);
    decref(a); decref(b);
}
static void mw_prim_value_lt (void) {
    VAL b = pop_value();
    VAL a = pop_value();
    int cmp = value_cmp_(a,b);
    push_bool(cmp < 0);
    decref(a); decref(b);
}
static void mw_prim_value_le (void) {
    VAL b = pop_value();
    VAL a = pop_value();
    int cmp = value_cmp_(a,b);
    push_bool(cmp <= 0);
    decref(a); decref(b);
}

static void mw_prim_sys_argc (void) {
    push_i64(global_argc);
}
static void mw_prim_sys_argv (void) {
    push_ptr(global_argv);
}

static void mw_prim_posix_write (void) {
    USIZE n = pop_usize();
    VAL vp = pop_value();
    void* p = value_ptr(vp);
    int fd = (int)pop_i64();
    ASSERT(n <= SIZE_MAX);
    push_i64((int64_t)write(fd, p, (size_t)n));
    decref(vp);
}
static void mw_prim_posix_read (void) {
    USIZE n = pop_usize();
    VAL vp = pop_value();
    void* p = value_ptr(vp);
    int fd = (int)pop_i64();
    ASSERT(n <= SIZE_MAX);
    push_i64((int64_t)read(fd, p, (size_t)n));
    decref(vp);
}
static void mw_prim_posix_open (void) {
    int m = (int)pop_i64();
    int f = (int)pop_i64();
    VAL vp = pop_value();
    void* path = value_ptr(vp);
    push_i64((int64_t)open(path,f,m));
    decref(vp);
}
static void mw_prim_posix_close (void) {
    int fd = (int)pop_i64();
    push_i64((int64_t)close(fd));
}
static void mw_prim_posix_exit (void) {
    int x = (int)pop_i64();
    exit(x);
}

void int_trace_(int64_t y, int fd) {
    char c[32] = {0};
    char* p = c+30;
    size_t n = 0;
    uint64_t x;
    if (y < 0) {
        if (y == INT64_MIN) {
            x = 1+(uint64_t)INT64_MAX;
        } else {
            x = (uint64_t)-y;
        }
    } else {
        x = (uint64_t)y;
    }
    do {
        *--p = '0' + (x % 10);
        x /= 10;
        n++;
    } while (x);
    if (y < 0) {
        *--p = '-';
        n++;
    }
    write(fd, p, n);
}

void str_trace_(STR* str, int fd) {
    ASSERT(str->size <= SIZE_MAX);
    write(fd, "\"", 1);
    write(fd, str->data, (size_t)str->size); // TODO handle escapes
    write(fd, "\"", 1);
}

void value_trace_(VAL val, int fd) {
    if (IS_INT(val)) {
        int_trace_(VINT(val), fd);
    } else if (IS_STR(val)) {
        str_trace_(VSTR(val), fd);
    } else if (IS_CONS(val)) {
        VAL car, cdr;
        value_uncons(val, &car, &cdr);
        write(fd, "[ ", 2);
        value_trace_(car, fd);
        write(fd, " ", 1);
        value_trace_(cdr, fd);
        write(fd, " ]", 2);
    } else {
        const char* msg = "value cannot be traced";
        write(2, msg, strlen(msg));
        exit(1);
    }
}

static void mw_prim_debug (void) {
    write(2, "??", 2);
    for (long i = STACK_MAX-1; i >= (long)stack_counter; i--) {
        write(2, " ", 1);
        value_trace_(stack[i], 2);
    }
    write(2, "\n", 1);
}

static void mw_prim_value_get (void) {
    VAL vp = pop_value();
    VAL *p = value_ptr(vp);
    push_value(*p);
    incref(*p);
    decref(vp);
}

static void mw_prim_int_get (void) {
    VAL vp = pop_value();
    int64_t *p = value_ptr(vp);
    push_i64(*p);
    decref(vp);
}

static void mw_prim_ptr_get (void) {
    VAL vp = pop_value();
    void **p = value_ptr(vp);
    push_ptr(*p);
    decref(vp);
}

static void mw_prim_u8_get (void) {
    VAL vp = pop_value();
    uint8_t *p = value_ptr(vp);
    push_u8(*p);
    decref(vp);
}

static void mw_prim_u16_get (void) {
    VAL vp = pop_value();
    uint16_t *p = value_ptr(vp);
    push_u16(*p);
    decref(vp);
}

static void mw_prim_u32_get (void) {
    VAL vp = pop_value();
    uint32_t *p = value_ptr(vp);
    push_u32(*p);
    decref(vp);
}

static void mw_prim_u64_get (void) {
    VAL vp = pop_value();
    uint64_t *p = value_ptr(vp);
    push_u64(*p);
    decref(vp);
}

static void mw_prim_i8_get (void) {
    VAL vp = pop_value();
    int8_t *p = value_ptr(vp);
    push_i8(*p);
    decref(vp);
}

static void mw_prim_i16_get (void) {
    VAL vp = pop_value();
    int16_t *p = value_ptr(vp);
    push_i16(*p);
    decref(vp);
}

static void mw_prim_i32_get (void) {
    VAL vp = pop_value();
    int32_t *p = value_ptr(vp);
    push_i32(*p);
    decref(vp);
}

static void mw_prim_i64_get (void) {
    VAL vp = pop_value();
    int64_t *p = value_ptr(vp);
    push_i64(*p);
    decref(vp);
}

static void mw_prim_value_set (void) {
    VAL vp = pop_value();
    VAL *p = value_ptr(vp);
    decref(*p);
    *p = pop_value();
    decref(vp);
}

static void mw_prim_int_set (void) {
    VAL vp = pop_value();
    int64_t *p = value_ptr(vp);
    *p = pop_i64();
    decref(vp);
}

static void mw_prim_ptr_set (void) {
    VAL vp = pop_value();
    void **p = value_ptr(vp);
    *p = pop_ptr();
    decref(vp);
}

static void mw_prim_u8_set (void) {
    VAL vp = pop_value();
    uint8_t *p = value_ptr(vp);
    *p = pop_u8();
    decref(vp);
}

static void mw_prim_u16_set (void) {
    VAL vp = pop_value();
    uint16_t *p = value_ptr(vp);
    *p = pop_u16();
    decref(vp);
}

static void mw_prim_u32_set (void) {
    VAL vp = pop_value();
    uint32_t *p = value_ptr(vp);
    *p = pop_u32();
    decref(vp);
}

static void mw_prim_u64_set (void) {
    VAL vp = pop_value();
    uint64_t *p = value_ptr(vp);
    *p = pop_u64();
    decref(vp);
}

static void mw_prim_i8_set (void) {
    VAL vp = pop_value();
    int8_t *p = value_ptr(vp);
    *p = pop_i8();
    decref(vp);
}

static void mw_prim_i16_set (void) {
    VAL vp = pop_value();
    int16_t *p = value_ptr(vp);
    *p = pop_i16();
    decref(vp);
}

static void mw_prim_i32_set (void) {
    VAL vp = pop_value();
    int32_t *p = value_ptr(vp);
    *p = pop_i32();
    decref(vp);
}

static void mw_prim_i64_set (void) {
    VAL vp = pop_value();
    int64_t *p = value_ptr(vp);
    *p = pop_i64();
    decref(vp);
}


#if defined(MIRTH_WINDOWS)
#define mw_prim_sys_os() push_u64(1)
#elif defined(MIRTH_LINUX)
#define mw_prim_sys_os() push_u64(2)
#elif defined(MIRTH_MACOS)
#define mw_prim_sys_os() push_u64(3)
#else
#define mw_prim_sys_os() push_u64(0)
#endif

static void mw_prim_unsafe_cast (void) { }

static void mw_prim_run (void) {
    VAL f = pop_value();
    run_value(f);
}

static void mw_prim_ptr_nil (void) {
    push_ptr((void*)0);
}
static void mw_prim_ptr_add (void) {
    VAL vptr = pop_value();
    USIZE n = pop_usize();
    ASSERT(IS_PTR(vptr) && VPTR(vptr));
    char* ptr = (char*)VPTR(vptr);
    push_ptr(ptr + n);
}
#define mw_prim_ptr_size() push_u64((uint64_t)sizeof(void*))
static void mw_prim_ptr_alloc (void) {
    ASSERT(0);
}
static void mw_prim_ptr_realloc (void) {
    ASSERT(0);
}

static void mw_prim_ptr_copy (void) {
    VAL vdst = pop_value();
    int64_t ilen = pop_i64();
    VAL vsrc = pop_value();
    ASSERT2(IS_PTR(vsrc) && IS_PTR(vdst), vsrc, vdst);
    void* src = value_ptr(vsrc);
    void* dst = value_ptr(vdst);
    if (src && dst && (ilen > 0)) {
        ASSERT((USIZE)ilen <= SIZE_MAX);
        memcpy(dst, src, (size_t)ilen);
    }
}

static void mw_prim_ptr_fill (void) {
    VAL vdst = pop_value();
    ASSERT1(IS_PTR(vdst), vdst);
    int64_t ilen = pop_i64();
    int64_t val = pop_i64();
    void* dst = value_ptr(vdst);
    if (dst && (ilen > 0)) {
        ASSERT((USIZE)ilen <= SIZE_MAX);
        memset(dst, (int)val, (size_t)ilen);
    }
}

static void mw_prim_ptr_raw (void) {
    VAL vptr = top_value();
    ASSERT(IS_PTR(vptr));
    push_value(vptr);
}

static void mw_prim_str_eq (void) {
    VAL vptr1 = pop_value();
    VAL vptr2 = pop_value();
    ASSERT2(IS_STR(vptr1) && IS_STR(vptr2), vptr1, vptr2);
    STR* str1 = VSTR(vptr1);
    STR* str2 = VSTR(vptr2);
    ASSERT(str1->size <= SIZE_MAX);
    push_bool((str1->size == str2->size) &&
        (memcmp(str1->data, str2->data, (size_t)str1->size) == 0));
    decref(vptr1);
    decref(vptr2);
}

static void mw_prim_str_alloc (void) {
    USIZE size = pop_usize();
    ASSERT(size <= SIZE_MAX-sizeof(STR)-4);
    STR* str = str_alloc(size);
    str->size = size;
    push_value(MKSTR(str));
}

static void mw_prim_str_copy (void) {
    USIZE size = pop_usize();
    char* ptr = (char*)pop_ptr();
    ASSERT(size <= SIZE_MAX-sizeof(STR)-4);
    ASSERT(ptr);
    push_value(mkstr(ptr, size));
}

static void mw_prim_str_cat (void) {
    VAL v2 = pop_value();
    VAL v1 = pop_value();
    ASSERT2(IS_STR(v1) && IS_STR(v2), v1, v2);
    STR* s1 = VSTR(v1);
    STR* s2 = VSTR(v2);
    USIZE m = s1->cap;
    USIZE n1 = s1->size;
    USIZE n2 = s2->size;
    if ((s1->refs == 1) && (n1 + n2 + 4 <= m)) {
        ASSERT(n2 <= SIZE_MAX);
        memcpy(s1->data + n1, s2->data, (size_t)n2);
        s1->size += n2;
        ASSERT(s1->size + 4 <= s1->cap);
        push_value(v1);
        decref(v2);
    } else {
        USIZE m2 = n1 + n2 + 4;
        if (m2 < m*2) m2 = m*2;
        STR* str = str_alloc(m2);
        str->size = n1+n2;
        ASSERT(n1 <= SIZE_MAX);
        ASSERT(n2 <= SIZE_MAX);
        memcpy(str->data, s1->data, (size_t)n1);
        memcpy(str->data+n1, s2->data, (size_t)n2);
        push_value(MKSTR(str));
        decref(v1);
        decref(v2);
    }
}

static void mw_prim_str_base (void) {
    VAL vstr = pop_value();
    ASSERT1(IS_STR(vstr) && VSTR(vstr), vstr);
    push_ptr(VSTR(vstr)->data);
    decref(vstr);
}

static void mw_prim_str_size (void) {
    VAL v = top_value();
    ASSERT(IS_STR(v) && VSTR(v));
    push_usize(VSTR(v)->size);
}

static void mw_prim_pack_nil (void) {
    push_u64(0);
}

static void mw_prim_pack_cons (void) {
    VAL cdr = pop_value();
    VAL car = pop_value();
    push_value(mkcons(car,cdr));
}

static void mw_prim_pack_uncons (void) {
    VAL v = pop_value();
    VAL car,cdr;
    value_uncons(v, &car, &cdr);
    push_value(car);
    push_value(cdr);
    incref(car); incref(cdr); decref(v);
}

static void mw_prim_mut_get (void) {
    VAL mut = pop_value();
    ASSERT1(IS_PTR(mut) && VPTR(mut), mut);
    VAL v = *(VAL*)VPTR(mut);
    if (!v.tag) v = MKNIL();
    // EXPECT(v.tag, "read uninitialized value");
    push_value(v);
    incref(v);
}
static void mw_prim_mut_set (void) {
    VAL mut = pop_value();
    VAL newval = pop_value();
    push_value(mut);
    ASSERT1(IS_PTR(mut) && VPTR(mut), mut);
    VAL oldval = *(VAL*)VPTR(mut);
    *(VAL*)VPTR(mut) = newval;
    if (oldval.tag) {
        decref(oldval);
    }
}

/* GENERATED C99 */
