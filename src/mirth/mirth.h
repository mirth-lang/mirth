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
extern void free(void*);
extern size_t strlen(const char*);
extern int read(int, void*, size_t);
extern int write(int, void*, size_t);
extern int close(int);
extern int open(void*, int, int);
extern int strcmp(const char*, const char*);
extern void exit(int);

#define EXPECT(test,msg) \
    do { \
        if (!(test)) { \
            write(2, msg "\n", strlen(msg "\n")); \
            exit(1); \
        } \
    } while(0)

#define ASSERT(test) \
    EXPECT(test, "assertion failed (" #test ")")

typedef enum TAG {
    TAG_INT = 0,
    TAG_CONS = 1,
} TAG;

typedef void (*fnptr)(void);

typedef union DATA {
    size_t usize;
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
} DATA;

typedef struct VAL {
    DATA data;
    TAG tag;
} VAL;

typedef uint32_t REFS;

typedef struct CONS {
    REFS refs;
    bool freecdr;
    VAL car;
    VAL cdr;
} CONS;

#define STACK_MAX 0x8000
static size_t stack_counter = STACK_MAX;
static VAL stack [STACK_MAX] = {0};

#define HEAP_SIZE 0x80000
#define HEAP_MASK 0x7FFFF
static size_t heap_next = 1;
static size_t heap_count = 0;
static CONS heap [HEAP_SIZE] = {0};

static int global_argc;
static char** global_argv;

static size_t get_cell_index(VAL v) {
    if (v.tag == TAG_CONS)
        return v.data.usize;
    else
        return 0;
}

static REFS* value_refs(VAL v) {
    if (v.tag == TAG_CONS)
        return &heap[v.data.usize].refs;
    else
        return 0;
}

static CONS* value_get_cons(VAL v) {
    if (v.tag == TAG_CONS)
        return heap + v.data.usize;
    else
        return 0;
}

static void free_value(VAL v);

static void incref(VAL v) {
    REFS *refs = value_refs(v);
    if (refs) *refs += 1;
}

static void decref(VAL v) {
    REFS *refs = value_refs(v);
    if (refs) {
        if (--*refs == 0) {
            free_value(v);
        }
    }
}

static void free_value(VAL v) {
    size_t i = get_cell_index(v);
    ASSERT(i);
    CONS cons = heap[i];
    ASSERT(cons.refs == 0);
    memset(heap+i, 0, sizeof(CONS));
    heap[i].cdr.data.usize = heap_next;
    heap_next = i;
    heap_count--;
    if (cons.freecdr) {
        free(cons.cdr.data.ptr);
    } else {
        decref(cons.cdr);
    }
    decref(cons.car);
}

static void value_uncons(VAL val, VAL* car, VAL* cdr) {
    CONS *cons = value_get_cons(val);
    if (cons) {
        *car = cons->car;
        *cdr = cons->cdr;
    } else {
        *car = (VAL){0};
        *cdr = val;
    }
}

static size_t value_ptr_size (VAL v) {
    VAL vcar, vcdr;
    value_uncons(v, &vcar, &vcdr);
    return vcar.data.usize;
}

static void* value_ptr (VAL v) {
    VAL vcar, vcdr;
    value_uncons(v, &vcar, &vcdr);
    return vcdr.data.ptr;
}

#define pop_fnptr() (pop_value().data.fnptr)
#define pop_u8() (pop_value().data.u8)
#define pop_u16() (pop_value().data.u16)
#define pop_u32() (pop_value().data.u32)
#define pop_u64() (pop_value().data.u64)
#define pop_i8() (pop_value().data.i8)
#define pop_i16() (pop_value().data.i16)
#define pop_i32() (pop_value().data.i32)
#define pop_i64() (pop_value().data.i64)
#define pop_usize() (pop_value().data.usize)
#define pop_bool() ((bool)pop_u64())
#define pop_ptr() (pop_value().data.ptr)

#define push_u64(v) push_value(mku64(v))
#define push_i64(v) push_value(mki64(v))
#define push_usize(v) push_value((uint64_t)(v))
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

static VAL mku64 (uint64_t x) {
    return (VAL){.tag=TAG_INT, .data={.u64=x}};
}

static VAL mki64 (int64_t x) {
    return (VAL){.tag=TAG_INT, .data={.i64=x}};
}

static VAL mkcell_raw (VAL car, VAL cdr, bool freecdr) {
    EXPECT(heap_count < HEAP_SIZE-1, "heap overflow");
    size_t i = heap_next;
    CONS *cell = heap+i;
    ASSERT(cell->refs == 0);
    heap_next = cell->cdr.data.usize ? cell->cdr.data.usize : i+1;
    heap_count++;
    cell->refs = 1;
    cell->freecdr = freecdr;
    cell->car = car;
    cell->cdr = cdr;
    return (VAL){ .tag=TAG_CONS, .data={.usize=i} };
}

static VAL mkcell(VAL car, VAL cdr) {
    if ((car.data.usize == 0) && (cdr.tag == TAG_INT))
        return cdr;
    return mkcell_raw(car, cdr, false);
}

static VAL mkptr_owned (void* ptr, size_t size) {
    VAL cdr = { .tag=TAG_INT, .data={.ptr=ptr} }; // TODO TAG_RAWPTR maybe?
    VAL car = { .tag=TAG_INT, .data={.usize=size} };
    return mkcell_raw(car, cdr, true);
}

static VAL mkptr_shared (void* ptr, size_t size) {
    VAL cdr = { .tag=TAG_INT, .data={.ptr=ptr} }; // TODO TAG_RAWPTR maybe?
    VAL car = { .tag=TAG_INT, .data={.usize=size} };
    return mkcell(car, cdr);
}

static bool val_has_freecdr (VAL v) {
    CONS* cons = value_get_cons(v);
    return cons && cons->freecdr;
}

static void do_uncons(void) {
    VAL val, car, cdr;
    val = pop_value();
    ASSERT(!val_has_freecdr(val));
    value_uncons(val, &car, &cdr);
    push_value(car); push_value(cdr);
    incref(car);
    incref(cdr);
    decref(val);
}

static size_t get_data_tag(VAL v) {
    VAL car, cdr;
    value_uncons(v, &car, &cdr);
    return cdr.data.usize;
}

static size_t get_top_data_tag(void) {
    return get_data_tag(top_value());
}

static int64_t int64_cmp(int64_t i1, int64_t i2) {
    if (i1 < i2) return -1;
    if (i1 > i2) return 1;
    return 0;
}

static int64_t value_cmp(VAL v1, VAL v2) {
    while (1) {
        // TODO fix string comparison, maybe with a TAG_STR
        if (v1.tag == v2.tag) {
            if (v1.data.u64 == v2.data.u64) return 0;
            if (v1.tag == TAG_INT) {
                if (v1.data.i64 < v2.data.i64) return -1;
                else return 1;
            }
        }
        VAL v1car, v1cdr, v2car, v2cdr;
        value_uncons(v1, &v1car, &v1cdr);
        value_uncons(v2, &v2car, &v2cdr);
        int64_t cdrcmp = value_cmp(v1cdr, v2cdr);
        if (cdrcmp) return cdrcmp;
        v1 = v1car; v2 = v2car;
    }
}

static bool value_eq(VAL v1, VAL v2) {
    return value_cmp(v1,v2) == 0;
}

static void run_value(VAL v) {
    VAL car, cdr;
    value_uncons(v, &car, &cdr);
    push_value(car);
    incref(car);
    decref(v);
    cdr.data.fnptr();
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
    int cmp = value_cmp(a,b);
    decref(a); decref(b);
    push_u64(cmp == 0);
}
static void mw_prim_value_lt (void) {
    VAL b = pop_value();
    VAL a = pop_value();
    int cmp = value_cmp(a,b);
    decref(a); decref(b);
    push_u64(cmp < 0);
}
static void mw_prim_value_le (void) {
    VAL b = pop_value();
    VAL a = pop_value();
    int cmp = value_cmp(a,b);
    decref(a); decref(b);
    push_u64(cmp <= 0);
}

static void mw_prim_sys_argc (void) {
    push_i64(global_argc);
}
static void mw_prim_sys_argv (void) {
    push_ptr(global_argv);
}

static void mw_prim_posix_write (void) {
    size_t n = pop_usize();
    VAL vp = pop_value();
    void* p = value_ptr(vp);
    int fd = (int)pop_i64();
    // TODO compare given size against pointer size
    push_i64((int64_t)write(fd, p, n));
    decref(vp);
}
static void mw_prim_posix_read (void) {
    size_t n = pop_usize();
    VAL vp = pop_value();
    void* p = value_ptr(vp);
    int fd = (int)pop_i64();
    // TODO compare given size against pointer size
    push_i64((int64_t)read(fd,p,n));
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

static void mw_prim_debug (void) {
    write(2, "??", 2);
    char c[32] = {0};
    char* cp;
    size_t n;
    int64_t x, y;
    for (long i = STACK_MAX-1; i >= (long)stack_counter; i--) {
        cp = c+30;
        x = stack[i].data.i64; // TODO look at tag, be better at this
        n = 1;
        y = x; if (x < 0) { x = -x; }
        do { *cp-- = '0' + (x % 10); x /= 10; n++; } while(x);
        if (y < 0) { *cp-- = '-'; n++; }
        *cp = ' ';
        write(2, cp, n);
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

static void mw_prim_ptr_add (void) {
    VAL vp = pop_value();
    uint64_t y = pop_u64();
    CONS* cons = value_get_cons(vp);
    if (cons) {
        ASSERT(!cons->freecdr || (cons->refs > 1));
        cons->refs--;
        void* ptr = (void*)(cons->cdr.data.charptr + y);
        size_t osize = cons->car.data.usize;
        size_t size = (osize > y ? osize - y : 0);
        push_value(mkptr_shared(ptr,size));
    } else {
        push_u64(y + vp.data.u64);
    }
}
#define mw_prim_ptr_size() push_u64((uint64_t)sizeof(void*))
static void mw_prim_ptr_alloc (void) {
    int64_t psize = pop_i64();
    if (psize > 0) {
        size_t size = (size_t)psize;
        void* ptr = calloc(1,size);
        VAL v = mkptr_owned(ptr, size);
        push_value(v);
    } else {
        push_u64(0);
    }
}

static void* alloc_but_copy (size_t dstn, void* src, size_t srcn) {
    void* dst = calloc(1,dstn);
    if (src && srcn) {
        size_t cpyn = (dstn > srcn) ? srcn : dstn;
        memcpy(dst, src, cpyn);
    }
    return dst;
}
static void mw_prim_ptr_realloc (void) {
    int64_t psize = pop_i64();
    VAL vptr = pop_value();
    if (psize <= 0) {
        decref(vptr);
        push_u64(0);
        return;
    } else {
        size_t new_size = (size_t)psize;
        void* old_ptr = value_ptr(vptr);
        size_t old_size = value_ptr_size(vptr);
        void* new_ptr = alloc_but_copy(new_size, old_ptr, old_size);
        push_value(mkptr_owned(new_ptr, new_size));
        decref(vptr);
    }
}

static void mw_prim_ptr_copy (void) {
    VAL vdst = pop_value();
    int64_t ilen = pop_i64();
    VAL vsrc = pop_value();
    void* src = value_ptr(vsrc);
    void* dst = value_ptr(vdst);
    if (src && dst && (ilen > 0)) {
        memcpy(dst, src, (size_t)ilen);
    }
    decref(vsrc);
    decref(vdst);
}

static void mw_prim_ptr_fill (void) {
    VAL vdst = pop_value();
    int64_t ilen = pop_i64();
    int64_t val = pop_i64();
    void* dst = value_ptr(vdst);
    if (dst && (ilen > 0)) {
        memset(dst, (int)val, (size_t)ilen);
    }
    decref(vdst);
}

static void mw_prim_ptr_raw (void) {
    VAL vptr = top_value();
    void *ptr = value_ptr(vptr);
    push_ptr(ptr);
}

static void mw_prim_str_eq (void) {
    VAL vptr1 = pop_value();
    VAL vptr2 = pop_value();
    const char* ptr1 = value_ptr(vptr1);
    const char* ptr2 = value_ptr(vptr2);
    bool result = (!ptr1 || !ptr2) ? (ptr1 == ptr2) : strcmp(ptr1,ptr2) == 0;
    push_bool(result);
    decref(vptr1); decref(vptr2);
}

static void mw_prim_str_alloc (void) {
    // strings have a 4 byte sentinel of zeros
    int64_t psize = pop_i64();
    push_i64(psize +(psize >= 0 ? 4 : 0));
    mw_prim_ptr_alloc();
}

static void mw_prim_str_base (void) { }

static void mw_prim_str_size (void) {
    VAL v = stack[stack_counter];
    if (!v.data.u64) {
        push_u64(0);
    } else if (v.tag == TAG_INT) {
        push_u64((uint64_t)strlen(v.data.ptr));
    } else {
        push_i64(value_ptr_size(v)-4);
    }
}

static void mw_prim_pack_nil (void) {
    push_u64(0);
}

static void mw_prim_pack_cons (void) {
    VAL cdr = pop_value();
    VAL car = pop_value();
    push_value(mkcell(car,cdr));
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
    ASSERT(mut.tag == TAG_INT);
    ASSERT(mut.data.ptr);
    VAL v = *mut.data.valptr;
    push_value(v);
    incref(v);
}
static void mw_prim_mut_set (void) {
    VAL mut = pop_value();
    VAL newval = pop_value();
    push_value(mut);
    ASSERT(mut.tag == TAG_INT);
    ASSERT(mut.data.ptr);
    VAL oldval = *mut.data.valptr;
    *mut.data.valptr = newval;
    decref(oldval);
}

/* GENERATED C99 */
