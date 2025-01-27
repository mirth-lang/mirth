/* MIRTH HEADER */
// #line 3 "src/mirth.h"

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__)
#define MIRTH_WINDOWS 1
#elif defined(__linux__)
#define MIRTH_LINUX 1
#elif defined(__APPLE__)
#define MIRTH_MACOS 1
#else
#error "Platform not supported."
#endif

#if defined(__x86_64__) || defined(_M_X64)
#define MIRTH_AMD64
#elif defined(i386) || defined(__i386__) || defined(__i386) || defined(_M_IX86)
#define MIRTH_I386
#elif defined(__aarch64__) || defined(_M_ARM64)
#define MIRTH_ARM64
#else
#error "Architecture not supported."
#endif

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <float.h>

extern void* malloc(size_t);
extern void* calloc(size_t, size_t);
extern void* realloc(void*, size_t);
extern void* memset(void*, int, size_t);
extern void* memcpy(void*, const void*, size_t);
extern void* memmove(void*, const void*, size_t);
extern int memcmp(const void*, const void*, size_t);
extern int strcmp(const char*, const char*);
extern size_t strlen(const char*);
extern void free(void*);
extern int read(int, void*, size_t);
extern int write(int, const char*, size_t);
extern int close(int);
extern int open(const char*, int, ...);
extern void exit(int);
extern int sprintf (char * s, const char * format, ...);
extern float strtof(const char* str, char** endptr);
extern double strtod(const char* str, char** endptr);

typedef struct INT { int64_t val; } INT;
typedef uint64_t TAG;

typedef uint32_t REFS;
typedef uint64_t USIZE;
typedef void (*FNPTR)(void);

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
    float f32;
    double f64;
    void* ptr;
    FNPTR fnptr;
    REFS* refs;
    INT iint;
    struct STR* str;
    struct TUP* tup;
} DATA;

typedef struct VAL {
    DATA data;
    TAG tag;
} VAL;

typedef struct TYPE {
    const char* name;
    uint64_t flags;
    void (*free)(VAL v);
    void (*trace_)(VAL v, int fd);
    void (*run)(VAL v);
} TYPE;


#define REFS_FLAG 0x0001
#define PTRMK(e,p,t) (((uint64_t)(e) << 48) | (uint64_t)(p) | (uint64_t)(t))
#define PTRPTR(p) ((void*)(uintptr_t)((uint64_t)(p) & 0xFFFFFFFFFFFC))
#define PTRTAG(p) ((uint64_t)(p) & 0x3)
#define PTREXTRA(p) ((uint64_t)(p) >> 48)

static void default_free   (VAL v);
static void default_trace_ (VAL v, int fd);
static void default_run    (VAL v);

static void bool_trace_ (VAL v, int fd);
static TYPE TYPE_BOOL = { .name = "Bool", .trace_ = bool_trace_ };

static void int_trace_ (VAL v, int fd);
static void int_free (VAL v);
static TYPE TYPE_INT = { .name = "Int", .trace_ = int_trace_, .free = int_free };

static void i64_trace_ (VAL v, int fd);
static TYPE TYPE_I64 = { .name = "I64", .trace_ = i64_trace_ };

static void i32_trace_ (VAL v, int fd);
static TYPE TYPE_I32 = { .name = "I32", .trace_ = i32_trace_ };

static void i16_trace_ (VAL v, int fd);
static TYPE TYPE_I16 = { .name = "I16", .trace_ = i16_trace_ };

static void i8_trace_ (VAL v, int fd);
static TYPE TYPE_I8 = { .name = "I8", .trace_ = i8_trace_ };

static void u64_trace_ (VAL v, int fd);
static TYPE TYPE_U64 = { .name = "U64", .trace_ = u64_trace_ };

static void u32_trace_ (VAL v, int fd);
static TYPE TYPE_U32 = { .name = "U32", .trace_ = u32_trace_ };

static void u16_trace_ (VAL v, int fd);
static TYPE TYPE_U16 = { .name = "U16", .trace_ = u16_trace_ };

static void u8_trace_ (VAL v, int fd);
static TYPE TYPE_U8 = { .name = "U8", .trace_ = u8_trace_ };

static void f64_trace_ (VAL v, int fd);
static TYPE TYPE_F64 = { .name = "F64", .trace_ = f64_trace_ };

static void f32_trace_ (VAL v, int fd);
static TYPE TYPE_F32 = { .name = "F32", .trace_ = f32_trace_ };

static void ptr_trace_ (VAL v, int fd);
static TYPE TYPE_PTR = { .name = "Ptr", .trace_ = ptr_trace_ };

static void fnptr_run (VAL v);
static TYPE TYPE_FNPTR = { .name = "FnPtr", .run = fnptr_run };

static void str_free (VAL v);
static void str_trace_ (VAL v, int fd);
static TYPE TYPE_STR = { .name = "Str", .flags=REFS_FLAG, .trace_ = str_trace_, .free=str_free };

static void tup_free (VAL v);
static void tup_trace_ (VAL v, int fd);
static void tup_run (VAL v);
static TYPE TYPE_TUP = { .name = "Tup", .flags = REFS_FLAG, .free = tup_free, .trace_ = tup_trace_, .run = tup_run };

#define TAG_BOOL  ((TAG)&TYPE_BOOL)
#define TAG_I64   ((TAG)&TYPE_I64)
#define TAG_I32   ((TAG)&TYPE_I32)
#define TAG_I16   ((TAG)&TYPE_I16)
#define TAG_I8    ((TAG)&TYPE_I8)
#define TAG_U64   ((TAG)&TYPE_U64)
#define TAG_U32   ((TAG)&TYPE_U32)
#define TAG_U16   ((TAG)&TYPE_U16)
#define TAG_U8    ((TAG)&TYPE_U8)
#define TAG_F64   ((TAG)&TYPE_F64)
#define TAG_F32   ((TAG)&TYPE_F32)
#define TAG_PTR   ((TAG)&TYPE_PTR)
#define TAG_FNPTR ((TAG)&TYPE_FNPTR)
#define TAG_INT   (REFS_FLAG | (TAG)&TYPE_INT)
#define TAG_STR   (REFS_FLAG | (TAG)&TYPE_STR)
#define TAG_TUP   (REFS_FLAG | (TAG)&TYPE_TUP)

#define VALEQ(v1,v2) (((v1).tag == (v2).tag) && ((v1).data.u64 == (v2).data.u64))

#define VTYPE(v) ((const TYPE*)(PTRPTR((v).tag)))
#define VREFS(v) (*(REFS*)PTRPTR((v).data.u64))
#define HAS_REFS(v) (((v).tag & REFS_FLAG) && PTRPTR((v).data.u64) && (PTRTAG((v).data.u64) == 0))

#define VINT(v)   ((v).data.iint)
#define VI64(v)   ((v).data.i64)
#define VI32(v)   ((v).data.i32)
#define VI16(v)   ((v).data.i16)
#define VI8(v)    ((v).data.i8)
#define VU64(v)   ((v).data.u64)
#define VU32(v)   ((v).data.u32)
#define VU16(v)   ((v).data.u16)
#define VU8(v)    ((v).data.u8)
#define VBOOL(v)  ((_Bool)((v).data.u64))
#define VF32(v)   ((v).data.f32)
#define VF64(v)   ((v).data.f64)
#define VPTR(v)   ((v).data.ptr)
#define VFNPTR(v) ((v).data.fnptr)

#define IS_VAL(v)   (1)
#define IS_INT(v)   ((v).tag == TAG_INT)
#define IS_I64(v)   ((v).tag == TAG_I64)
#define IS_I32(v)   ((v).tag == TAG_I32)
#define IS_I16(v)   ((v).tag == TAG_I16)
#define IS_I8(v)    ((v).tag == TAG_I8)
#define IS_U64(v)   ((v).tag == TAG_U64)
#define IS_U32(v)   ((v).tag == TAG_U32)
#define IS_U16(v)   ((v).tag == TAG_U16)
#define IS_U8(v)    ((v).tag == TAG_U8)
#define IS_BOOL(v)  ((v).tag == TAG_BOOL)
#define IS_F32(v)   ((v).tag == TAG_F32)
#define IS_F64(v)   ((v).tag == TAG_F64)
#define IS_PTR(v)   ((v).tag == TAG_PTR)
#define IS_FNPTR(v) ((v).tag == TAG_FNPTR)
#define IS_STR(v)   ((v).tag == TAG_STR)
#define IS_TUP(v)   ((v).tag == TAG_TUP)
#define IS_NIL(v)   (IS_TUP(v) && (VTUPLEN(v) == 0))

#define MKVAL(x)   (x)
#define MKBOOL(x)  ((VAL){.tag=TAG_BOOL, .data={.u64=(x)}})
#define MKI64(x)   ((VAL){.tag=TAG_I64, .data={.i64=(x)}})
#define MKI32(x)   ((VAL){.tag=TAG_I32, .data={.i64=(x)}})
#define MKI16(x)   ((VAL){.tag=TAG_I16, .data={.i64=(x)}})
#define MKI8(x)    ((VAL){.tag=TAG_I8, .data={.i64=(x)}})
#define MKU64(x)   ((VAL){.tag=TAG_U64, .data={.u64=(x)}})
#define MKU32(x)   ((VAL){.tag=TAG_U32, .data={.u64=(x)}})
#define MKU16(x)   ((VAL){.tag=TAG_U16, .data={.u64=(x)}})
#define MKU8(x)    ((VAL){.tag=TAG_U8, .data={.u64=(x)}})
#define MKF32(x)   ((VAL){.tag=TAG_F32, .data={.f32=(x)}})
#define MKF64(x)   ((VAL){.tag=TAG_F64, .data={.f64=(x)}})
#define MKPTR(x)   ((VAL){.tag=TAG_PTR, .data={.ptr=(x)}})
#define MKFNPTR(x) ((VAL){.tag=TAG_FNPTR, .data={.fnptr=(x)}})

#define VSTR(v)    (((v).data.str))
#define MKSTR(x)   ((VAL){.tag=TAG_STR, .data={.str=(x)}})

#define VTUP(v)    (((v).data.tup))
#define VTUPLEN(v) (tup_len_(VTUP(v)))
#define MKTUP(x,n) ((VAL){.tag=TAG_TUP, .data={.tup=(x)}})
#define MKNIL      ((VAL){.tag=TAG_TUP, .data={.tup=NULL}})

#define INT63_MIN (-0x4000000000000000LL)
#define INT63_MAX ( 0x3FFFFFFFFFFFFFFFLL)

#define IS_I63(x) ((x).val & 1)
#define WRAP_I63(x) ((INT){ .val = 1+2*(int64_t)(x) })
#define GET_I63(x) ((((x).val)-1)/2)

#define IS_BIG(x) (!IS_I63(x))
#define WRAP_BIG(x) ((INT){ .val = (intptr_t)(x) })
#define GET_BIG(x) ((struct BIG*)(intptr_t)((x).val))

#define MKINT(x) ((VAL){.tag=TAG_INT, .data={.iint=(x)}})

#define IS_INT_I63(x) (IS_INT(x) && IS_I63((x).data.iint))
#define MKINT_I63(x) (MKINT(WRAP_I63(x)))
#define VINT_I63(x) (GET_I63((x).data.iint))

#define IS_INT_BIG(x) (IS_INT(x) && IS_BIG((x).data.iint))
#define MKINT_BIG(x) (MKINT(WRAP_BIG(x)))
#define VINT_BIG(x) (GET_BIG((x).data.iint))

#define TUP_LEN_MAX 0x3FFF

#define STRLIT(v,x,n) \
    do { \
        static STR* mval = 0; \
        if (!mval) mval = str_make(x,n); \
        incref(MKSTR(mval)); \
        v = mval; \
    } while(0)

typedef uint16_t TUPLEN;
typedef struct TUP {
    REFS refs;
    TUPLEN cap;
    TUPLEN size;
    VAL cells[];
} TUP;

typedef struct STR {
    REFS refs;
    USIZE cap;
    USIZE size;
    char data[];
} STR;

#define ANY
#define BIG_S(z) { REFS refs; USIZE cap; USIZE size; uint32_t radix[z]; }
typedef struct BIG BIG_S(ANY) BIG;

#define STACK_MAX 0x80000
static USIZE stack_counter = STACK_MAX;
static VAL stack [STACK_MAX] = {0};
static USIZE rstack_counter = STACK_MAX;
static VAL rstack [STACK_MAX] = {0};

static int global_argc;
static char** global_argv;

static void push_value(VAL v);
static void trace_stack(void);
static void trace_rstack(void);

#if MIRTH_DEBUG
    typedef struct LOC {
        const char* word;
        const char* path;
        USIZE line, col;
        const char* atom;
    } LOC;
    static USIZE fstack_counter = 0;
    static LOC fstack [STACK_MAX] = {
        {
            .word="<word>",
            .path="<path>",
            .line=0, .col=0,
            .atom="<atom>"
        },
    };

    #define WORD_ENTER(_w,_p,_l,_c) \
        do { \
            fstack[fstack_counter].word = (_w); \
            fstack[fstack_counter].path = (_p); \
            fstack[fstack_counter].line = (_l); \
            fstack[fstack_counter].col = (_c); \
            fstack[fstack_counter].atom = ""; \
            fstack_counter++; \
        } while(0)

    #define WORD_ATOM(_l,_c,_n) \
        do { \
            if (fstack_counter > 0) { \
                fstack[fstack_counter-1].line = (_l); \
                fstack[fstack_counter-1].col = (_c); \
                fstack[fstack_counter-1].atom = (_n); \
            } \
        } while(0)

    #define WORD_EXIT \
        do { \
            if (fstack_counter == 0) { \
                TRACE("mismatched WORD_EXIT\n"); \
                exit(1); \
            } \
            fstack_counter--; \
        } while(0)
#endif

#define TRACE(x) write(2,x,strlen(x))
#define _STR(x) #x
#define STR(x) _STR(x)

#define EXPECT(test,msg) \
    do { \
        if (!(test)) { \
            TRACE(msg "\n"); \
            trace_stack(); \
            trace_rstack(); \
            exit(1); \
        } \
    } while(0)

#define EXPECT1(test,msg,v1) \
    do { \
        if (!(test)) { \
            TRACE(msg "\n"); \
            push_value(v1); \
            trace_stack(); \
            trace_rstack(); \
            exit(1); \
        } \
    } while(0)

#define EXPECT2(test,msg,v1,v2) \
    do { \
        if (!(test)) { \
            TRACE(msg "\n"); \
            push_value(v1); \
            push_value(v2); \
            trace_stack(); \
            trace_rstack(); \
            exit(1); \
        } \
    } while(0)

#define ASSERT(test) \
    EXPECT(test, __FILE__ ":" STR(__LINE__) ": error: assertion failed (" #test ")")
#define ASSERT1(test,v) \
    EXPECT1(test, __FILE__ ":" STR(__LINE__) ": error: assertion failed (" #test ")", v)
#define ASSERT2(test,v1,v2) \
    EXPECT2(test, __FILE__ ":" STR(__LINE__) ": error: assertion failed (" #test ")", v1, v2)

#define incref(v) do { if (HAS_REFS(v)) VREFS(v)++; } while(0)
#define decref(v) do { if (HAS_REFS(v)) if (!--VREFS(v)) free_value(v); } while(0)
static void free_value(VAL v) {
    ASSERT(VTYPE(v));
    ASSERT1(VTYPE(v)->free,v);
    ASSERT1(HAS_REFS(v),v);
    ASSERT1(VREFS(v) == 0,v);
    VTYPE(v)->free(v);
}

static void str_free (VAL v) {
    STR* str = VSTR(v);
    free(str);
}

static void tup_free (VAL v) {
    TUP* tup = VTUP(v);
    if (tup) {
        for (TUPLEN i = 0; i < tup->size; i++) {
            decref(tup->cells[i]);
        }
        free(tup);
    }
}

static void tup_decref_outer(TUP* tup, size_t n) {
    ASSERT(tup);
    ASSERT(tup->size == n);
    if (tup->refs == 1) {
        free(tup);
    } else {
        for (size_t i = 0; i < n; i++) {
            incref(tup->cells[i]);
        }
        if (!--tup->refs) tup_free(MKTUP(tup,n));
    }
}

static TUPLEN tup_len_ (TUP* tup) {
    if (tup) {
        return tup->size;
    } else {
        return 0;
    }
}

static uint64_t value_u64 (VAL v) { ASSERT1(IS_U64(v),v); return VU64(v); }
static uint32_t value_u32 (VAL v) { ASSERT1(IS_U32(v),v); return VU32(v); }
static uint16_t value_u16 (VAL v) { ASSERT1(IS_U16(v),v); return VU16(v); }
static uint8_t  value_u8  (VAL v) { ASSERT1(IS_U8(v) ,v); return VU8(v) ; }

static INT value_int (VAL v) { ASSERT1(IS_INT(v),v); return VINT(v); }
static int64_t value_i64 (VAL v) { ASSERT1(IS_I64(v),v); return VI64(v); }
static int32_t value_i32 (VAL v) { ASSERT1(IS_I32(v),v); return VI32(v); }
static int16_t value_i16 (VAL v) { ASSERT1(IS_I16(v),v); return VI16(v); }
static int8_t  value_i8  (VAL v) { ASSERT1(IS_I8(v) ,v); return VI8(v) ; }
static bool value_bool (VAL v) { ASSERT1(IS_BOOL(v),v); return VBOOL(v); }

static double value_f64 (VAL v) { ASSERT1(IS_F64(v), v); return VF64(v); }
static float value_f32 (VAL v) { ASSERT1(IS_F32(v), v); return VF32(v); }

static void* value_ptr (VAL v) { ASSERT1(IS_PTR(v),v); return VPTR(v); }
static FNPTR value_fnptr (VAL v) { ASSERT1(IS_FNPTR(v),v); return VFNPTR(v); }
static STR* value_str (VAL v) { ASSERT1(IS_STR(v),v); return VSTR(v); }
static TUP* value_tup (VAL v, TUPLEN n) { ASSERT1(IS_TUP(v) && (VTUPLEN(v) == n), v); return VTUP(v); }

static void push_value (VAL x) { ASSERT(stack_counter > 0); stack[--stack_counter] = x; }
static void push_resource (VAL x) { ASSERT(rstack_counter > 0); rstack[--rstack_counter] = x; }
static VAL pop_value (void) { ASSERT(stack_counter < STACK_MAX); return stack[stack_counter++]; }
static VAL pop_resource (void) { ASSERT(rstack_counter < STACK_MAX); return rstack[rstack_counter++]; }

// Create a TUP with at least min(cap_hint, TUP_LEN_MAX) capacity.
static TUP* tup_new (TUPLEN cap_hint) {
    if (cap_hint < 3) cap_hint = 3;
    if (cap_hint > TUP_LEN_MAX) cap_hint = TUP_LEN_MAX;
    TUP *new_tup = calloc(1, sizeof(TUP) + sizeof(VAL)*(USIZE)cap_hint);
    ASSERT(new_tup);
    new_tup->refs = 1;
    new_tup->cap = cap_hint;
    return new_tup;
}

static VAL tup_replace (VAL tup, TUPLEN i, VAL v) {
    ASSERT(IS_TUP(tup));
    TUPLEN n = VTUPLEN(tup);
    ASSERT(i < n);
    if (VTUP(tup)->refs > 1) {
        TUP* newtup = tup_new(n);
        newtup->size = n;
        memcpy(newtup->cells, VTUP(tup)->cells, n*sizeof(VAL));
        for (TUPLEN j=0; j<n; j++) incref(newtup->cells[j]);
        decref(tup);
        tup = MKTUP(newtup, n);
    }
    VAL u = VTUP(tup)->cells[i];
    VTUP(tup)->cells[i] = v;
    decref(u);
    return tup;
}

typedef struct STACK {
    size_t cap;
    size_t len;
    VAL* val;
} STACK;

static VAL lpop(STACK* stk) {
    ASSERT(stk->len > 0);
    return stk->val[--stk->len];
}
static void lpush(STACK* stk, VAL val) {
    if (stk->len >= stk->cap) {
        stk->cap = stk->cap * 2 + 4;
        VAL* newval = realloc(stk->val, stk->cap * sizeof(VAL));
        ASSERT(newval);
        stk->val = newval;
    }
    stk->val[stk->len++] = val;
}

static STR* str_alloc (USIZE cap) {
    ASSERT(cap <= SIZE_MAX - sizeof(STR) - 4);
    STR* str = calloc(1, (size_t)(cap + sizeof(STR) + 4));
    EXPECT(str, "failed to allocate string");
    str->refs = 1;
    str->cap = cap;
    return str;
}

static STR* str_make (const char* data, USIZE size) {
    ASSERT(data);
    ASSERT(size <= SIZE_MAX - sizeof(STR) - 4);
    STR* str = str_alloc(size);
    str->size = size;
    memcpy(str->data, data, (size_t)size);
    return str;
}
#define mkstr(x,n) MKSTR(str_make((x), (n)))

static STR* str_pushn (STR* s, const char* p, USIZE n2) {
    if (n2 == 0) return s;
    ASSERT(s && p);
    ASSERT(n2 <= SIZE_MAX);
    USIZE m = s->cap;
    USIZE n1 = s->size;
    ASSERT(n1 <= SIZE_MAX-n2);
    ASSERT(4 <= SIZE_MAX-n1-n2);
    if ((s->refs == 1) && (n1 + n2 + 4 <= m)) {
        memcpy(s->data + n1, p, (size_t)n2);
        s->size += n2;
        ASSERT(s->size + 4 <= s->cap);
        return s;
    } else {
        USIZE m2 = n1 + n2 + 4;
        if ((s->refs == 1) && (m <= SIZE_MAX-m) && (m2 < m*2)) m2 = m*2;
        STR* str = str_alloc(m2);
        str->size = n1+n2;
        memcpy(str->data, s->data, (size_t)n1);
        memcpy(str->data+n1, p, (size_t)n2);
        decref(MKSTR(s));
        return str;
    }
}

static STR* str_cat (STR* s1, STR* s2) {
    EXPECT(s1 && s2, "invalid strings in prim-str-cat");
    STR* out = str_pushn(s1, s2->data, s2->size);
    decref(MKSTR(s2));
    return out;
}

static USIZE get_data_tag(VAL v) {
    if (IS_TUP(v)) {
        ASSERT(VTUPLEN(v) > 0);
        return VU64(VTUP(v)->cells[0]);
    } else {
        return VU64(v);
    }
}

static int str_cmp(STR* s1, STR* s2) {
    ASSERT(s1 && s2);
    USIZE n1 = s1->size;
    USIZE n2 = s2->size;
    USIZE n = (n1 < n2 ? n1 : n2);
    ASSERT(n < SIZE_MAX);
    int r = memcmp(s1->data, s2->data, (size_t)n);
    decref(MKSTR(s1));
    decref(MKSTR(s2));
    if (r) return r;
    if (n1 < n2) return -1;
    if (n1 > n2) return 1;
    return 0;
}

static STR* str_drop (STR* in, size_t num_bytes) {
    ASSERT(in);
    if (num_bytes > in->size) {
        num_bytes = in->size;
    }
    size_t remaining = in->size - num_bytes;
    char* slice = in->data + num_bytes;

    if (in->refs == 1) {
        if (remaining > 0) memmove(in->data, slice, remaining);
        memset(in->data + remaining, 0, in->cap - remaining);
        in->size = remaining;
        return in;
    } else {
        STR* out = str_make(slice, remaining);
        decref(MKSTR(in));
        return out;
    }
}

static float str_to_f32 (STR* in, STR** out) {
    ASSERT(in); ASSERT(out);
    ASSERT(in->data[in->size] == 0);
    char *endptr = NULL;
    float val = strtof(in->data, &endptr);
    ASSERT(endptr);
    ASSERT(endptr >= in->data);
    size_t consumed = endptr - in->data;
    *out = str_drop(in, consumed);
    return val;
}

static double str_to_f64 (STR* in, STR** out) {
    ASSERT(in); ASSERT(out);
    ASSERT(in->data[in->size] == 0);
    char *endptr = NULL;
    double val = strtod(in->data, &endptr);
    ASSERT(endptr);
    ASSERT(endptr >= in->data);
    size_t consumed = endptr - in->data;
    *out = str_drop(in, consumed);
    return val;
}

static void tup_run(VAL v) {
    ASSERT(VTUPLEN(v)>0);
    VAL h = VTUP(v)->cells[0];
    ASSERT(IS_FNPTR(h));
    push_value(v);
    VFNPTR(h)();
}

static void fnptr_run(VAL v) {
    VFNPTR(v)();
}

static void run_value(VAL v) {
    ASSERT(VTYPE(v));
    ASSERT1(VTYPE(v)->run, v);
    VTYPE(v)->run(v);
}

void int_free(VAL x) {
    if (IS_INT_BIG(x)) {
        free(VINT_BIG(x));
    }
}

BIG* big_alloc(size_t cap) {
    ASSERT(cap >= 2);
    BIG* out = calloc(1, sizeof(*out) + cap*sizeof(*out->radix) + 4);
    EXPECT(out, "Ran out of memory.");
    out->refs = 1;
    out->cap = cap;
    out->size = 0;
    return out;
}

#define SIGN_BIT(x) ((x) & 0x80000000)
#define NEXT_RADIX(x) (SIGN_BIT(x) ? (uint32_t)0xFFFFFFFF : (uint32_t)0)
#define SIGN_RADIX(a) (NEXT_RADIX((a)->radix[(a)->size-1]))

#define LO_RADIX(x) ((uint32_t)(x))
#define HI_RADIX(x) ((uint32_t)(((uint64_t)(x)) >> 32))

static void big_incref (BIG* a) { a->refs++; }
static void big_decref (BIG* a) { if (!--a->refs) free(a); }

// expand (or clone) to make room for n radixes, returns unique a
static BIG* big_reserve (BIG* a, size_t n) {
    ASSERT(a);
    if ((a->refs == 1) && (a->cap >= n))
        return a;
    if (n < a->cap*2) n = a->cap*2;
    if (a->refs == 1) {
        BIG* b = realloc(a, sizeof(*b) + n*sizeof(*b->radix) + 4);
        EXPECT(b, "Ran out of memory.");
        b->cap = n;
        return b;
    } else {
        BIG* b = big_alloc(n);
        b->size = a->size;
        memcpy(b->radix, a->radix, a->size*sizeof(a->radix[0]));
        big_decref(a);
        return b;
    }
}

#define FITS_I63(x) ((INT63_MIN <= (int64_t)(x)) && ((int64_t)(x) <= INT63_MAX))

static INT i64_to_int(int64_t x) {
    if (FITS_I63(x)) {
        return WRAP_I63(x);
    } else {
        BIG* big = big_alloc(3);
        big->size = 2;
        uint64_t y = (uint64_t)x;
        big->radix[0] = LO_RADIX(y);
        big->radix[1] = HI_RADIX(y);
        return WRAP_BIG(big);
    }
}

static INT u64_to_int(uint64_t x) {
    if (x <= (uint64_t)INT64_MAX) {
        return i64_to_int((int64_t)x);
    } else {
        BIG* big = big_alloc(4);
        big->size = 3;
        big->radix[0] = LO_RADIX(x);
        big->radix[1] = HI_RADIX(x);
        big->radix[2] = 0;
        return WRAP_BIG(big);
    }
}

static INT big_normalize (BIG* a) {
    ASSERT(a); ASSERT(a->refs == 1); ASSERT(a->size >= 2);
    while ((a->size > 2) && (NEXT_RADIX(a->radix[a->size-2]) == a->radix[a->size-1]))
        a->size--;
    if (a->size == 2) {
        uint64_t u = (uint64_t)(a->radix[0]) | ((uint64_t)(a->radix[1]) << 32);
        int64_t i = (int64_t)u;
        if (FITS_I63(i)) {
            free(a);
            return WRAP_I63(i);
        }
    }
    return WRAP_BIG(a);
}

static INT big_i63_add (BIG* a, int64_t b) {
    ASSERT(a); ASSERT(a->size >= 2);
    if (b == 0) return WRAP_BIG(a);
    a = big_reserve(a, a->size+1);

    uint32_t an = NEXT_RADIX(a->radix[a->size-1]);

    uint32_t b0 = LO_RADIX(b);
    uint32_t b1 = HI_RADIX(b);
    uint32_t bn = NEXT_RADIX(b1);

    uint64_t r0 = (uint64_t)(a->radix[0]) + (uint64_t)(b0);
    uint64_t r1 = (uint64_t)(a->radix[1]) + (uint64_t)(b1) + (uint64_t)HI_RADIX(r0);

    a->radix[0] = LO_RADIX(r0);
    a->radix[1] = LO_RADIX(r1);
    uint32_t c = HI_RADIX(r1);
    for (size_t i = 2; i < a->size; i++) {
        uint64_t r = (uint64_t)(a->radix[i]) + (uint64_t)bn + (uint64_t)c;
        a->radix[i] = LO_RADIX(r);
        c = HI_RADIX(r);
    }
    a->radix[a->size++] = an + bn + c;
    return big_normalize(a);
}

static INT big_big_add (BIG* a, BIG* b) {
    ASSERT(a); ASSERT(b);
    if (b->size > a->size) { BIG*t=a; a=b; b=t; }
    a = big_reserve(a, a->size+1);
    uint32_t an = NEXT_RADIX(a->radix[a->size-1]);
    uint32_t bn = NEXT_RADIX(b->radix[b->size-1]);
    uint32_t c = 0;
    size_t i=0;
    for (; i < b->size; i++) {
        uint64_t r = (uint64_t)a->radix[i] + (uint64_t)b->radix[i] + (uint64_t)c;
        a->radix[i] = LO_RADIX(r);
        c = HI_RADIX(r);
    }
    for (; i < a->size; i++) {
        uint64_t r = (uint64_t)a->radix[i] + (uint64_t)bn + (uint64_t)c;
        a->radix[i] = LO_RADIX(r);
        c = HI_RADIX(r);
    }
    a->radix[a->size++] = an + bn + c;
    big_decref(b);
    return big_normalize(a);
}

static INT int_add(INT a, INT b) {
    if (IS_I63(a)) {
        if (IS_I63(b)) {
            return i64_to_int(GET_I63(a) + GET_I63(b));
        } else {
            return big_i63_add(GET_BIG(b), GET_I63(a));
        }
    } else {
        if (IS_I63(b)) {
            return big_i63_add(GET_BIG(a), GET_I63(b));
        } else {
            return big_big_add(GET_BIG(a), GET_BIG(b));
        }
    }
}

static INT big_negate(BIG* a) {
    ASSERT(a);
    a = big_reserve(a, a->size+1);
    uint32_t an = NEXT_RADIX(a->radix[a->size-1]);
    uint32_t c = 1;
    for (size_t i=0; i < a->size; i++) {
        uint64_t r = (uint64_t)(~(uint32_t)(a->radix[i])) + (uint64_t)c;
        a->radix[i] = LO_RADIX(r);
        c = HI_RADIX(r);
    }
    a->radix[a->size++] = ~an + c;
    return big_normalize(a);
}

static INT int_negate(INT a) {
    if (IS_I63(a)) {
        return i64_to_int(-GET_I63(a));
    } else {
        return big_negate(GET_BIG(a));
    }
}

static INT int_sub(INT a, INT b) {
    if (IS_I63(a) && IS_I63(b)) {
        return i64_to_int(GET_I63(a) - GET_I63(b));
    } else {
        return int_add(a, int_negate(b));
    }
}

static uint64_t int_to_u64(INT a) {
    if (IS_I63(a)) {
        return (uint64_t)GET_I63(a);
    } else {
        BIG* b = GET_BIG(a);
        ASSERT(b && b->size >= 2);
        uint64_t r = (uint64_t)(b->radix[0]) | ((uint64_t)(b->radix[1]) << 32);
        big_decref(b);
        return r;
    }
}
static int64_t int_to_i64(INT a) { return (int64_t)int_to_u64(a); }

static INT i63_i63_mul (int64_t a, int64_t b) {
    if ((b == 0) ||
        ((b > 0) && (INT63_MIN/b <= a) && (a <= INT63_MAX/b)) ||
        ((b < 0) && (INT63_MAX/b <= a) && (a <= INT63_MIN/b)))
    {
        return WRAP_I63(a * b);
    }

    bool negate = (a < 0) ^ (b < 0);
    if (a < 0) a = -a;
    if (b < 0) b = -b;

    uint64_t a0 = LO_RADIX(a);
    uint64_t a1 = HI_RADIX(a);
    uint64_t b0 = LO_RADIX(b);
    uint64_t b1 = HI_RADIX(b);

    uint64_t r0 = a0 * b0;
    uint64_t r1 = a1 * b0 + a0 * b1 + HI_RADIX(r0);
    uint64_t r2 = a1 * b1 + HI_RADIX(r1);
    uint64_t r3 = HI_RADIX(r2);

    BIG* c = big_alloc(5);
    c->size = 4;
    c->radix[0] = LO_RADIX(r0);
    c->radix[1] = LO_RADIX(r1);
    c->radix[2] = LO_RADIX(r2);
    c->radix[3] = LO_RADIX(r3);
    INT x = big_normalize(c);
    return negate ? int_negate(x) : x;
}

static void big_u32_mul_shift_accum_(BIG* accum, BIG* a, uint32_t x, size_t shift) {
    if (x == 0) return;
    uint32_t mul_carry = 0;
    uint32_t add_carry = 0;
    uint32_t an = SIGN_RADIX(a);
    for (size_t i=0; i + shift < accum->size; i++) {
        uint32_t ar = (i < a->size) ? a->radix[i] : an;
        uint64_t mul_result = (uint64_t)ar * (uint64_t)x + (uint64_t)mul_carry;
        uint64_t add_result = (uint64_t)accum->radix[i+shift] + (uint64_t)LO_RADIX(mul_result) + (uint64_t)add_carry;
        accum->radix[i+shift] = LO_RADIX(add_result);
        mul_carry = HI_RADIX(mul_result);
        add_carry = HI_RADIX(add_result);
    }
}

static void big_negate_shift_accum_(BIG* accum, BIG* a, size_t shift) {
    uint32_t carry = 1;
    uint32_t an = SIGN_RADIX(a);
    for (size_t i = 0; i+shift < accum->size; i++) {
        uint32_t ar = (i < a->size) ? a->radix[i] : an;
        uint64_t result = (uint64_t)accum->radix[i+shift] + (uint64_t)(~ar) + (uint64_t)carry;
        accum->radix[i+shift] = LO_RADIX(result);
        carry = HI_RADIX(result);
    }
}

static INT big_i63_mul (BIG* a, int64_t b) {
    if (b == 0) { big_decref(a); return WRAP_I63(0); }
    if (b == 1) { return WRAP_BIG(a); }
    if (b == -1) { return big_negate(a); }
    BIG* accum = big_alloc(a->size + 3);
    accum->size = a->size + 2;
    big_u32_mul_shift_accum_(accum, a, LO_RADIX(b), 0);
    big_u32_mul_shift_accum_(accum, a, HI_RADIX(b), 1);
    uint32_t bn = NEXT_RADIX(HI_RADIX(b));
    if (b < 0) { big_negate_shift_accum_(accum, a, 2); }
    big_decref(a);
    return big_normalize(accum);
}

static INT big_big_mul (BIG* a, BIG* b) {

    if (a->size > b->size) { BIG*t=a; a=b; b=t; }
    BIG* accum = big_alloc(a->size + b->size + 1);
    accum->size = a->size + b->size;
    for (size_t i = 0; i < b->size; i++) {
        big_u32_mul_shift_accum_(accum, a, b->radix[i], i);
    }
    if (SIGN_RADIX(b)) {
        big_negate_shift_accum_(accum, a, b->size);
    }
    big_decref(a);
    big_decref(b);
    return big_normalize(accum);
}

static INT int_mul(INT a, INT b) {
    if (IS_I63(a)) {
        if (IS_I63(b)) {
            return i63_i63_mul(GET_I63(a),GET_I63(b));
        } else {
            return big_i63_mul(GET_BIG(b),GET_I63(a));
        }
    } else {
        if (IS_I63(b)) {
            return big_i63_mul(GET_BIG(a),GET_I63(b));
        } else {
            return big_big_mul(GET_BIG(a),GET_BIG(b));
        }
    }
}

static int64_t i64_div(int64_t, int64_t);
static int64_t i64_mod(int64_t, int64_t);
static INT int_divmod(INT a, INT b, INT *r) {
    // TODO
    int64_t ia = int_to_i64(a);
    int64_t ib = int_to_i64(b);
    int64_t iq = i64_div(ia,ib);
    int64_t ir = i64_mod(ia,ib);
    *r = i64_to_int(ir);
    return i64_to_int(iq);
}

static double int_to_f64(INT a) {
    if (IS_I63(a)) {
        return (double)GET_I63(a);
    } else {
        BIG* b = GET_BIG(a);
        ASSERT(b && b->size);
        bool negative = SIGN_BIT(b->radix[b->size-1]);
        double r = 0.0;
        for (size_t i = b->size; i --> 0;) {
            uint32_t br = b->radix[i];
            if (negative) br = ~br;
            r *= (double)0x100000000;
            r += (double)br;
        }
        if (negative) r = -(r + 1.0);
        big_decref(b);
        return r;
    }
}

static float int_to_f32(INT a) { return (float)int_to_f64(a); }

static bool big_negative_(BIG* a) {
    ASSERT(a && (a->size > 0));
    return SIGN_BIT(a->radix[a->size-1]);
}

static int big_cmp_(BIG* a, BIG* b) {
    ASSERT(a); ASSERT(b);
    bool an = big_negative_(a);
    bool bn = big_negative_(b);
    if (an && !bn) return -1;
    if (!an && bn) return  1;
    if (a->size != b->size) {
        int i = (a->size < b->size) ? -1 : +1;
        return an ? -i : i;
    }
    for (size_t i = a->size; i --> 0;) {
        int64_t ar = a->radix[i];
        int64_t br = b->radix[i];
        if (ar != br) {
            return (ar < br) ? -1 : +1;
            // negative doesn't matter here, since they are both either negative or positive.
            // think of comparing these as if adding a large constant to make them both non negative.
        }
    }
    return 0;
}

static int int_cmp(INT a, INT b) {
    if (IS_I63(a)) {
        if (IS_I63(b)) {
            int64_t ia = GET_I63(a);
            int64_t ib = GET_I63(b);
            if (ia < ib) return -1;
            if (ia > ib) return +1;
            return 0;
        } else {
            bool n = big_negative_(GET_BIG(b));
            big_decref(GET_BIG(b));
            return n ? 1 : -1;
        }
    } else {
        if (IS_I63(b)) {
            bool n = big_negative_(GET_BIG(a));
            big_decref(GET_BIG(a));
            return n ? -1 : 1;
        } else {
            int i = big_cmp_(GET_BIG(a), GET_BIG(b));
            big_decref(GET_BIG(a));
            big_decref(GET_BIG(b));
            return i;
        }
    }
}

static bool int_eq(INT a, INT b) { return int_cmp(a,b) == 0; }
static bool int_lt(INT a, INT b) { return int_cmp(a,b) <  0; }
static bool int_le(INT a, INT b) { return int_cmp(a,b) <= 0; }
static bool int_gt(INT a, INT b) { return int_cmp(a,b) >  0; }
static bool int_ge(INT a, INT b) { return int_cmp(a,b) >= 0; }
static bool int_ne(INT a, INT b) { return int_cmp(a,b) != 0; }


static STR* i64_to_str(int64_t);
static STR* int_to_str(INT a) {
    if (IS_I63(a) || (GET_BIG(a)->size == 2)) {
        return i64_to_str(int_to_i64(a));
    } else {
        STR *s = str_alloc(GET_BIG(a)->size * 10);
        if (SIGN_RADIX(GET_BIG(a))) {
            a = int_negate(a);
            ASSERT(IS_BIG(a));
            s = str_pushn(s, "-", 1);
        }
        s = str_pushn(s,"0x",2);
        BIG* b = GET_BIG(a);
        size_t i = b->radix[b->size-1] ? b->size : b->size-1;
        while (i --> 0) {
            uint32_t r = b->radix[i];
            char c[10] = "00000000_";
            int j = 8;
            while (j --> 0) {
                uint32_t h = r & 0xF;
                c[j] = (h > 9) ? h + 'A' - 10 : h + '0';
                r = r >> 4;
            }
            s = str_pushn(s,c,8+(i>0));
        }
        big_decref(b);
        return s;
    }
}

static int64_t i64_add (int64_t a, int64_t b) {
    EXPECT(((b >= 0) && (a <= INT64_MAX - b))
        || ((b <  0) && (a >= INT64_MIN - b)),
        "overflow during integer addition");
    return a + b;
}

static int64_t i64_sub (int64_t a, int64_t b) {
    EXPECT(((b >= 0) && (a >= INT64_MIN + b))
        || ((b <  0) && (a <= INT64_MAX + b)),
        "overflow during integer subtraction");
    return a - b;
}

static int64_t i64_mul (int64_t a, int64_t b) {
    EXPECT((a == 0) || (b == 0) ||
        ((a > 0) && (b > 0) && (a <= INT64_MAX/b)) ||
        ((a > 0) && (b < 0) && (b >= INT64_MIN/a)) ||
        ((a < 0) && (b > 0) && (a >= INT64_MIN/b)) ||
        ((a < 0) && (b < 0) && (a >= INT64_MAX/b)),
        "overflow during integer multiplication"
    );
    return a * b;
}

static int64_t i64_div (int64_t a, int64_t b) {
    EXPECT(b != 0, "divide by zero");
    EXPECT(!((b == -1) && (a == INT64_MIN)), "overflow during integer division");
    int64_t r = a % b;
    int64_t q = a / b;
    if (((a < 0) ^ (b < 0)) && r) q--;
    return q;
}

static int64_t i64_mod (int64_t a, int64_t b) {
    EXPECT(b != 0, "divide by zero");
    EXPECT(!((b == -1) && (a == INT64_MIN)), "overflow during integer division");
    int64_t r = a % b;
    int64_t q = a / b;
    if (((a < 0) ^ (b < 0)) && r) r += b;
    return r;
}

static uint64_t u64_div (uint64_t a, uint64_t b) {
    EXPECT(b != 0, "divide by zero");
    return a / b;
}

static uint64_t u64_mod (uint64_t a, uint64_t b) {
    EXPECT(b != 0, "divide by zero");
    return a % b;
}

static uint64_t u64_shl (uint64_t a, uint64_t b) {
    if (b >= 64) return 0;
    return (a << b);
}

static uint64_t u64_shr (uint64_t a, uint64_t b) {
    if (b >= 64) return 0;
    return (a >> b);
}

static STR* f32_to_str (float d) {
    char result[DBL_DIG+32] = {0};
    int len = sprintf(result, "%.*g", DBL_DIG, d);
    return str_make(result, len);
}

static STR* f64_to_str (double d) {
    char result[DBL_DIG+32] = {0};
    int len = sprintf(result, "%.*g", DBL_DIG, d);
    return str_make(result, len);
}

void u64_repr(uint64_t x, char* c, size_t m, char** out_ptr, size_t *out_size) {
    ASSERT(m >= 4);
    memset(c, 0, m);
    char* p = c+(m-1);
    size_t n = 0;
    do {
        *--p = '0' + (x % 10);
        x /= 10;
        n++;
    } while (x && (p > c));
    *out_ptr = p;
    *out_size = n;
}

void i64_repr(int64_t y, char* c, size_t m, char** out_ptr, size_t *out_size) {
    ASSERT(m >= 5);
    c[0] = 0;
    char* p;
    size_t n;
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
    u64_repr(x,c+1,m-1,&p,&n);
    if (y<0) { *--p = '-'; n++; }
    *out_ptr = p;
    *out_size = n;
}

void i64_trace_(VAL v, int fd) { char c[32], *p; size_t n; i64_repr(VI64(v), c, sizeof(c), &p, &n); write(fd, p, n); }
void i32_trace_(VAL v, int fd) { char c[32], *p; size_t n; i64_repr(VI32(v), c, sizeof(c), &p, &n); write(fd, p, n); }
void i16_trace_(VAL v, int fd) { char c[32], *p; size_t n; i64_repr(VI16(v), c, sizeof(c), &p, &n); write(fd, p, n); }
void  i8_trace_(VAL v, int fd) { char c[32], *p; size_t n; i64_repr(VI8(v),  c, sizeof(c), &p, &n); write(fd, p, n); }
void u64_trace_(VAL v, int fd) { char c[32], *p; size_t n; u64_repr(VU64(v), c, sizeof(c), &p, &n); write(fd, p, n); }
void u32_trace_(VAL v, int fd) { char c[32], *p; size_t n; u64_repr(VU32(v), c, sizeof(c), &p, &n); write(fd, p, n); }
void u16_trace_(VAL v, int fd) { char c[32], *p; size_t n; u64_repr(VU16(v), c, sizeof(c), &p, &n); write(fd, p, n); }
void  u8_trace_(VAL v, int fd) { char c[32], *p; size_t n; u64_repr(VU8(v),  c, sizeof(c), &p, &n); write(fd, p, n); }

void bool_trace_(VAL v, int fd) { if (VBOOL(v)) { write(fd, "True", 4); } else { write(fd, "False", 5); } }

void int_trace_(VAL v, int fd) {
    ASSERT(IS_INT(v));
    INT x = VINT(v);
    incref(v);
    STR* s = int_to_str(x);
    write(fd, s->data, s->size);
    decref(MKSTR(s));
}

void f32_trace_(VAL v, int fd) { (void)v; write(fd, "<F32>", 5); }
void f64_trace_(VAL v, int fd) { (void)v; write(fd, "<F64>", 5); }
void ptr_trace_(VAL v, int fd) { (void)v; write(fd, "<Ptr>", 5); }

static STR* u64_to_str (uint64_t x) {
    bool cache = (x <= 255);
    static STR* scache[256] = {0};
    if (cache && scache[x]) {
        STR* s = scache[x];
        incref(MKSTR(s));
        return s;
    } else {
        char c[32];
        char* p; size_t n;
        u64_repr(x,c,sizeof(c),&p,&n);
        STR* s = str_make(p,n);
        if (cache) {
            scache[x] = s;
            incref(MKSTR(s));
        }
        return s;
    }
}

static STR* i64_to_str (int64_t x) {
    bool cache = (-128 <= x) && (x < 128);
    static STR* scache[256] = {0};
    if (cache && scache[x+128]) {
        STR* s = scache[x+128];
        incref(MKSTR(s));
        return s;
    } else {
        char c[32];
        char* p; size_t n;
        i64_repr(x,c,sizeof(c),&p,&n);
        STR* s = str_make(p,n);
        if (cache) {
            scache[x+128] = s;
            incref(MKSTR(s));
        }
        return s;
    }
}

void str_trace_(VAL v, int fd) {
    STR* str = VSTR(v);
    ASSERT(str->size <= SIZE_MAX);
    write(fd, "\"", 1);
    USIZE i0 = 0;
    char xb[4]={'\\','x'};
    USIZE i;
    for (i = 0; i < str->size; i++) {
        const char* c = NULL; size_t n=0;
        uint8_t v=str->data[i];
        switch(v) {
            case '\n': c="\\n"; n=2; break;
            case '\r': c="\\r"; n=2; break;
            case '\t': c="\\t"; n=2; break;
            case '\\': c="\\\\"; n=2; break;
            case '\"': c="\\\""; n=2; break;
            default:
                if (!((' ' <= v) && (v < 0x7F))) {
                    xb[2] = '0' + (v&15) + ('A'-'9'-1)*((v&15) > 9);
                    xb[3] = '0' + (v/16) + ('A'-'9'-1)*((v/16) > 9);
                    c=xb; n=4;
                }
        }
        if ((n > 0) && (i0 < i)) {
            write(fd, str->data+i0, (size_t)(i-i0));
            i0=i+1;
        }
        write(fd, c, n);
    }
    if (i0 < i) write(fd, str->data+i0, (size_t)(i-i0));
    write(fd, "\"", 1);
}

static void value_trace_(VAL v, int fd) {
    if (VTYPE(v) && VTYPE(v)->trace_) {
        VTYPE(v)->trace_(v,fd);
    } else if (VTYPE(v)) {
        const char* name = VTYPE(v)->name;
        write(fd, "<", 1);
        write(fd, name, strlen(name));
        write(fd, ">", 1);
    } else {
        write(fd, "<NULL>", 6);
    }
}

static void tup_trace_(VAL v, int fd) {
    TUPLEN len = VTUPLEN(v);
    TUP* tup = VTUP(v);
    if (len == 0) {
        write(fd, "[]", 2);
    } else {
        write(fd, "[ ", 2);
        for(TUPLEN i = 0; i < len; i++) {
            if (i > 0) write(fd, " ", 1);
            value_trace_(tup->cells[i], fd);
        }
        write(fd, " ]", 2);
    }
}

static void trace_stack (void) {
    TRACE("??");
    for (long i = STACK_MAX-1; i >= (long)stack_counter; i--) {
        TRACE(" ");
        value_trace_(stack[i], 2);
    }
    TRACE("\n");
}

static void trace_rstack (void) {
    #if MIRTH_DEBUG
        TRACE("call stack:\n");
        for (USIZE i = fstack_counter; i --> 1;) {
            TRACE("    ");
            if (fstack[i-1].atom && *fstack[i-1].atom && strcmp(fstack[i-1].atom, fstack[i].word)) {
                TRACE(fstack[i-1].atom);
                TRACE(" -> ");
            }
            TRACE(fstack[i].word);
            TRACE(" at ");
            TRACE(fstack[i-1].path);
            TRACE(":");
            i64_trace_(MKI64(fstack[i-1].line), 2);
            TRACE(":");
            i64_trace_(MKI64(fstack[i-1].col), 2);
            TRACE("\n");
        }
    #endif
}

static void do_panic(STR* m) {
    if (m) {
        size_t n = (m->size < SIZE_MAX) ? (size_t)(m->size) : SIZE_MAX;
        write(2, m->data, n);
        TRACE("\n");
    } else {
        TRACE("panic!\n");
    }
    trace_stack();
    trace_rstack();
    exit(1);
}

#if defined(MIRTH_WINDOWS)
#define RUNNING_OS 1
#elif defined(MIRTH_LINUX)
#define RUNNING_OS 2
#elif defined(MIRTH_MACOS)
#define RUNNING_OS 3
#else
#define RUNNING_OS 0
#endif

#if defined(MIRTH_I386)
#define RUNNING_ARCH 1
#elif defined(MIRTH_AMD64)
#define RUNNING_ARCH 2
#elif defined(MIRTH_ARM64)
#define RUNNING_ARCH 3
#else
#define RUNNING_ARCH 0
#endif

static void* ptr_alloc (uint64_t n) {
    EXPECT((n > 0) && ((uint64_t)n <= SIZE_MAX), "invalid size in prim-ptr-alloc");
    void* p = malloc((size_t)n);
    EXPECT(p, "failed to allocate in prim-ptr-alloc");
    return p;
}

static void* ptr_realloc (void* p, uint64_t n) {
    EXPECT((n > 0) && ((uint64_t)n <= SIZE_MAX), "invalid size in prim-ptr-realloc");
    void* p2 = realloc(p, (size_t)n);
    EXPECT(p2, "failed to reallocate in prim-ptr-realloc");
    return p2;
}

static void ptr_copy (void* src, uint64_t len, void* dst) {
    if (len > 0) {
        EXPECT(len <= SIZE_MAX, "invalid size in prim-ptr-copy");
        EXPECT(src && dst, "invalid pointer in prim-ptr-copy");
        memcpy(dst, src, (size_t)len);
    }
}

static void ptr_fill (uint8_t val, uint64_t len, void* dst) {
    if (len > 0) {
        EXPECT(len <= SIZE_MAX, "invalid size in prim-ptr-fill");
        EXPECT(dst, "invalid pointer in prim-ptr-fill");
        memset(dst, (int)val, (size_t)len);
    }
}

static void* str_base (STR* s) {
    EXPECT(s && (s->refs > 1), "invalid string for prim-str-base");
    s->refs--;
    return s->data;
}

static uint64_t str_size (STR* s) {
    EXPECT(s, "invalid string for prim-str-size");
    uint64_t n = s->size;
    decref(MKSTR(s));
    return n;
}

static VAL mut_get (void* mut) {
    EXPECT(mut, "invalid pointer in prim-mut-get");
    VAL v = *(VAL*)mut;
    EXPECT(v.tag, "tried to read uninitialized value");
    incref(v);
    return v;
}

static void mut_set (VAL newval, void* mut) {
    EXPECT(mut, "invalid pointer in prim-mut-set");
    VAL oldval = *(VAL*)mut;
    *(VAL*)mut = newval;
    if (oldval.tag) {
        decref(oldval);
    }
}

static bool mut_is_set (void* mut) {
    EXPECT(mut, "invalid pointer in prim-mut-is-set");
    VAL val = *(VAL*)mut;
    return (val.tag != 0);
}

typedef struct FIELD { size_t num_blocks; VAL** blocks; } FIELD;

static VAL* field_mut(FIELD* field, uint64_t index) {
    ASSERT(field);
    size_t block_size = 1024;
    size_t block_i = index / block_size;
    size_t block_j = index % block_size;
    if (block_i >= field->num_blocks) {
        ASSERT(field->num_blocks <= SIZE_MAX - 4 - block_i);
        size_t new_num_blocks = field->num_blocks + block_i + 4;
        VAL** new_blocks = realloc(field->blocks, sizeof(VAL*) * new_num_blocks);
        ASSERT(new_blocks);
        memset(new_blocks + field->num_blocks, 0, sizeof(VAL*) * (new_num_blocks - field->num_blocks));
        field->blocks = new_blocks;
        field->num_blocks = new_num_blocks;
    }
    if(!field->blocks[block_i]) {
        field->blocks[block_i] = calloc(block_size, sizeof(VAL));
        ASSERT(field->blocks[block_i]);
    }
    return field->blocks[block_i] + block_j;
}

/* GENERATED C99 */
