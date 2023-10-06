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

typedef struct CONS {
    uint32_t refs;
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

static void incref(VAL v) {
    size_t i = get_cell_index(v);
    if (i) {
        heap[i].refs++;
    }
}

static void decref(VAL v) {
    size_t i = get_cell_index(v);
    if (i) {
        ASSERT(heap[i].refs);
        heap[i].refs--;
        if (heap[i].refs == 0) {
            CONS cons = heap[i];
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
    }
}

static void value_uncons(VAL val, VAL* car, VAL* cdr) {
    size_t i = get_cell_index(val);
    if (i) {
        *car = heap[i].car;
        *cdr = heap[i].cdr;
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
    size_t i = get_cell_index(v);
    return i && heap[i].freecdr;
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
    size_t i = get_cell_index(vp);
    if (i) {
        CONS *cell = heap+i;
        ASSERT(!cell->freecdr || (cell->refs > 1));
        cell->refs--;
        void* ptr = (void*)(cell->cdr.data.charptr + y);
        size_t size = (cell->car.data.usize > y ? cell->car.data.usize - y : 0);
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

static void mw_prim_mut_new (void) {
    VAL car = pop_value();
    VAL cdr = {0};
    push_value(mkcell_raw(car,cdr,false));
}
static void mw_prim_mut_get (void) {
    VAL mut = pop_value();
    if ((mut.tag == TAG_INT) && mut.data.ptr) {
        VAL v = *mut.data.valptr;
        push_value(v);
        incref(v);
    } else {
        VAL car,cdr;
        value_uncons(mut, &car, &cdr);
        ASSERT((cdr.tag == 0) && (cdr.data.u64 == 0));
        push_value(car);
        incref(car);
        decref(mut);
    }
}
static void mw_prim_mut_set (void) {
    VAL mut = pop_value();
    VAL newval = pop_value();
    push_value(mut);
    if ((mut.tag == TAG_INT) && mut.data.ptr) {
        VAL oldval = *mut.data.valptr;
        *mut.data.valptr = newval;
        decref(oldval);
    } else {
        size_t i = get_cell_index(mut);
        if (i) {
            CONS *cell = heap+i;
            VAL oldval = cell->car;
            cell->car = newval;
            decref(oldval);
        } else {
            decref(newval);
        }
    }
}

/* GENERATED C99 */

static void mw_RAWPTR (void) {
}
static void mw_OS_UNKNOWN (void) {
    push_u64(0LL);
}
static void mw_OS_WINDOWS (void) {
    push_u64(1LL);
}
static void mw_OS_LINUX (void) {
    push_u64(2LL);
}
static void mw_OS_MACOS (void) {
    push_u64(3LL);
}
static void mw_EQ (void) {
    push_u64(0LL);
}
static void mw_LT (void) {
    push_u64(1LL);
}
static void mw_GT (void) {
    push_u64(2LL);
}
static void mw_NONE (void) {
    push_u64(0LL);
}
static void mw_SOME (void) {
    VAL car = pop_value();
    VAL tag = mku64(1LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_L0 (void) {
    push_u64(0LL);
}
static void mw_L1 (void) {
    VAL car = pop_value();
    VAL tag = mku64(1LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_L2 (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    VAL tag = mku64(2LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_L3 (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    car = mkcell(car, pop_value());
    VAL tag = mku64(3LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_LCAT (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    car = mkcell(car, pop_value());
    VAL tag = mku64(4LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_L1_2B_ (void) {
    VAL car = pop_value();
    VAL tag = mku64(0LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_L2_2B_ (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    VAL tag = mku64(1LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_L3_2B_ (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    car = mkcell(car, pop_value());
    VAL tag = mku64(2LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_LCAT_2B_ (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    car = mkcell(car, pop_value());
    VAL tag = mku64(3LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TS_STOP (void) {
    push_u64(0LL);
}
static void mw_TS_SKIP (void) {
    push_u64(1LL);
}
static void mw_TS_CHAR (void) {
    VAL car = pop_value();
    VAL tag = mku64(2LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TS_PUSH (void) {
    VAL car = pop_value();
    VAL tag = mku64(3LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TS_COPY (void) {
    VAL car = pop_value();
    VAL tag = mku64(4LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_PATH (void) {
}
static void mw_FILE (void) {
}
static void mw_STACK_NIL (void) {
    push_u64(0LL);
}
static void mw_STACK_CONS (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    VAL tag = mku64(1LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_DEF_NONE (void) {
    push_u64(0LL);
}
static void mw_DEF_MODULE (void) {
    VAL car = pop_value();
    VAL tag = mku64(1LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_DEF_TYPE (void) {
    VAL car = pop_value();
    VAL tag = mku64(2LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_DEF_TAG (void) {
    VAL car = pop_value();
    VAL tag = mku64(3LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_DEF_PRIM (void) {
    VAL car = pop_value();
    VAL tag = mku64(4LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_DEF_WORD (void) {
    VAL car = pop_value();
    VAL tag = mku64(5LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_DEF_BUFFER (void) {
    VAL car = pop_value();
    VAL tag = mku64(6LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_DEF_VARIABLE (void) {
    VAL car = pop_value();
    VAL tag = mku64(7LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_DEF_CONSTANT (void) {
    VAL car = pop_value();
    VAL tag = mku64(8LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_DEF_EXTERNAL (void) {
    VAL car = pop_value();
    VAL tag = mku64(9LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_DEF_FIELD (void) {
    VAL car = pop_value();
    VAL tag = mku64(10LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_HASH (void) {
}
static void mw_SET (void) {
}
static void mw_ROW (void) {
}
static void mw_COL (void) {
}
static void mw_LOCATION (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    car = mkcell(car, pop_value());
    VAL tag = mku64(0LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TOKEN_NONE (void) {
    push_u64(0LL);
}
static void mw_TOKEN_COMMA (void) {
    push_u64(1LL);
}
static void mw_TOKEN_LPAREN (void) {
    VAL car = pop_value();
    VAL tag = mku64(2LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TOKEN_RPAREN (void) {
    VAL car = pop_value();
    VAL tag = mku64(3LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TOKEN_LSQUARE (void) {
    VAL car = pop_value();
    VAL tag = mku64(4LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TOKEN_RSQUARE (void) {
    VAL car = pop_value();
    VAL tag = mku64(5LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TOKEN_LCURLY (void) {
    VAL car = pop_value();
    VAL tag = mku64(6LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TOKEN_RCURLY (void) {
    VAL car = pop_value();
    VAL tag = mku64(7LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TOKEN_INT (void) {
    VAL car = pop_value();
    VAL tag = mku64(8LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TOKEN_STR (void) {
    VAL car = pop_value();
    VAL tag = mku64(9LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TOKEN_NAME (void) {
    VAL car = pop_value();
    VAL tag = mku64(10LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_BAG (void) {
}
static void mw_BAG_2B_ (void) {
}
static void mw_MAP (void) {
}
static void mw_TYPE_ERROR (void) {
    push_u64(0LL);
}
static void mw_TYPE_DONT_CARE (void) {
    push_u64(1LL);
}
static void mw_TPrim (void) {
    VAL car = pop_value();
    VAL tag = mku64(2LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TMeta (void) {
    VAL car = pop_value();
    VAL tag = mku64(3LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_THole (void) {
    VAL car = pop_value();
    VAL tag = mku64(4LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TVar (void) {
    VAL car = pop_value();
    VAL tag = mku64(5LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TTable (void) {
    VAL car = pop_value();
    VAL tag = mku64(6LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TData (void) {
    VAL car = pop_value();
    VAL tag = mku64(7LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TTensor (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    VAL tag = mku64(8LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TMorphism (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    VAL tag = mku64(9LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TApp (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    VAL tag = mku64(10LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_TValue (void) {
    VAL car = pop_value();
    VAL tag = mku64(11LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_VALUE_INT (void) {
    VAL car = pop_value();
    VAL tag = mku64(0LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_VALUE_STR (void) {
    VAL car = pop_value();
    VAL tag = mku64(1LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_VALUE_BLOCK (void) {
    VAL car = pop_value();
    VAL tag = mku64(2LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_PRIM_TYPE_UNIT (void) {
    push_u64(0LL);
}
static void mw_PRIM_TYPE_TYPE (void) {
    push_u64(1LL);
}
static void mw_PRIM_TYPE_STACK (void) {
    push_u64(2LL);
}
static void mw_PRIM_TYPE_EFFECT (void) {
    push_u64(3LL);
}
static void mw_PRIM_TYPE_INT (void) {
    push_u64(4LL);
}
static void mw_PRIM_TYPE_PTR (void) {
    push_u64(5LL);
}
static void mw_PRIM_TYPE_STR (void) {
    push_u64(6LL);
}
static void mw_PRIM_TYPE_CHAR (void) {
    push_u64(7LL);
}
static void mw_PRIM_TYPE_BOOL (void) {
    push_u64(8LL);
}
static void mw_PRIM_TYPE_U64 (void) {
    push_u64(9LL);
}
static void mw_PRIM_TYPE_U32 (void) {
    push_u64(10LL);
}
static void mw_PRIM_TYPE_U16 (void) {
    push_u64(11LL);
}
static void mw_PRIM_TYPE_U8 (void) {
    push_u64(12LL);
}
static void mw_PRIM_TYPE_I64 (void) {
    push_u64(13LL);
}
static void mw_PRIM_TYPE_I32 (void) {
    push_u64(14LL);
}
static void mw_PRIM_TYPE_I16 (void) {
    push_u64(15LL);
}
static void mw_PRIM_TYPE_I8 (void) {
    push_u64(16LL);
}
static void mw_PRIM_TYPE_MUT (void) {
    push_u64(17LL);
}
static void mw_GAMMA (void) {
}
static void mw_SUBST (void) {
}
static void mw_ARG_BLOCK (void) {
    VAL car = pop_value();
    VAL tag = mku64(0LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_ARG_VAR_RUN (void) {
    VAL car = pop_value();
    VAL tag = mku64(1LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_NONE (void) {
    push_u64(0LL);
}
static void mw_OP_PRIM (void) {
    VAL car = pop_value();
    VAL tag = mku64(1LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_WORD (void) {
    VAL car = pop_value();
    VAL tag = mku64(2LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_EXTERNAL (void) {
    VAL car = pop_value();
    VAL tag = mku64(3LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_BUFFER (void) {
    VAL car = pop_value();
    VAL tag = mku64(4LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_VARIABLE (void) {
    VAL car = pop_value();
    VAL tag = mku64(5LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_CONSTANT (void) {
    VAL car = pop_value();
    VAL tag = mku64(6LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_FIELD (void) {
    VAL car = pop_value();
    VAL tag = mku64(7LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_INT (void) {
    VAL car = pop_value();
    VAL tag = mku64(8LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_STR (void) {
    VAL car = pop_value();
    VAL tag = mku64(9LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_TAG (void) {
    VAL car = pop_value();
    VAL tag = mku64(10LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_MATCH (void) {
    VAL car = pop_value();
    VAL tag = mku64(11LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_LAMBDA (void) {
    VAL car = pop_value();
    VAL tag = mku64(12LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_VAR (void) {
    VAL car = pop_value();
    VAL tag = mku64(13LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OP_BLOCK (void) {
    VAL car = pop_value();
    VAL tag = mku64(14LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_PARAM (void) {
}
static void mw_PATTERN_UNDERSCORE (void) {
    push_u64(0LL);
}
static void mw_PATTERN_TAG (void) {
    VAL car = pop_value();
    VAL tag = mku64(1LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_LAZY_READY (void) {
    VAL car = pop_value();
    VAL tag = mku64(0LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_LAZY_DELAY (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    VAL tag = mku64(1LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_LAZY_WAIT (void) {
    push_u64(2LL);
}
static void mw_CTX (void) {
}
static void mw_TYPE_ELAB (void) {
    VAL car = pop_value();
    car = mkcell(car, pop_value());
    VAL tag = mku64(0LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OPSIG_ID (void) {
    push_u64(0LL);
}
static void mw_OPSIG_PUSH (void) {
    VAL car = pop_value();
    VAL tag = mku64(1LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_OPSIG_APPLY (void) {
    VAL car = pop_value();
    VAL tag = mku64(2LL);
    car = mkcell(car, tag);
    push_value(car);
}
static void mw_PRIM_CORE_ID (void) {
    push_u64(0LL);
}
static void mw_PRIM_CORE_DUP (void) {
    push_u64(1LL);
}
static void mw_PRIM_CORE_DROP (void) {
    push_u64(2LL);
}
static void mw_PRIM_CORE_SWAP (void) {
    push_u64(3LL);
}
static void mw_PRIM_CORE_DIP (void) {
    push_u64(4LL);
}
static void mw_PRIM_CORE_IF (void) {
    push_u64(5LL);
}
static void mw_PRIM_CORE_WHILE (void) {
    push_u64(6LL);
}
static void mw_PRIM_CORE_DEBUG (void) {
    push_u64(7LL);
}
static void mw_PRIM_CORE_RUN (void) {
    push_u64(8LL);
}
static void mw_PRIM_CORE_MATCH (void) {
    push_u64(9LL);
}
static void mw_PRIM_CORE_LAMBDA (void) {
    push_u64(10LL);
}
static void mw_PRIM_UNSAFE_CAST (void) {
    push_u64(11LL);
}
static void mw_PRIM_VALUE_EQ (void) {
    push_u64(12LL);
}
static void mw_PRIM_VALUE_LT (void) {
    push_u64(13LL);
}
static void mw_PRIM_VALUE_LE (void) {
    push_u64(14LL);
}
static void mw_PRIM_VALUE_GET (void) {
    push_u64(15LL);
}
static void mw_PRIM_VALUE_SET (void) {
    push_u64(16LL);
}
static void mw_PRIM_INT_ADD (void) {
    push_u64(17LL);
}
static void mw_PRIM_INT_SUB (void) {
    push_u64(18LL);
}
static void mw_PRIM_INT_MUL (void) {
    push_u64(19LL);
}
static void mw_PRIM_INT_DIV (void) {
    push_u64(20LL);
}
static void mw_PRIM_INT_MOD (void) {
    push_u64(21LL);
}
static void mw_PRIM_INT_AND (void) {
    push_u64(22LL);
}
static void mw_PRIM_INT_OR (void) {
    push_u64(23LL);
}
static void mw_PRIM_INT_XOR (void) {
    push_u64(24LL);
}
static void mw_PRIM_INT_SHL (void) {
    push_u64(25LL);
}
static void mw_PRIM_INT_SHR (void) {
    push_u64(26LL);
}
static void mw_PRIM_INT_GET (void) {
    push_u64(27LL);
}
static void mw_PRIM_INT_SET (void) {
    push_u64(28LL);
}
static void mw_PRIM_BOOL_TRUE (void) {
    push_u64(29LL);
}
static void mw_PRIM_BOOL_FALSE (void) {
    push_u64(30LL);
}
static void mw_PRIM_BOOL_AND (void) {
    push_u64(31LL);
}
static void mw_PRIM_BOOL_OR (void) {
    push_u64(32LL);
}
static void mw_PRIM_PACK_NIL (void) {
    push_u64(33LL);
}
static void mw_PRIM_PACK_CONS (void) {
    push_u64(34LL);
}
static void mw_PRIM_PACK_UNCONS (void) {
    push_u64(35LL);
}
static void mw_PRIM_MUT_NEW (void) {
    push_u64(36LL);
}
static void mw_PRIM_MUT_GET (void) {
    push_u64(37LL);
}
static void mw_PRIM_MUT_SET (void) {
    push_u64(38LL);
}
static void mw_PRIM_PTR_ADD (void) {
    push_u64(39LL);
}
static void mw_PRIM_PTR_SIZE (void) {
    push_u64(40LL);
}
static void mw_PRIM_PTR_GET (void) {
    push_u64(41LL);
}
static void mw_PRIM_PTR_SET (void) {
    push_u64(42LL);
}
static void mw_PRIM_PTR_ALLOC (void) {
    push_u64(43LL);
}
static void mw_PRIM_PTR_REALLOC (void) {
    push_u64(44LL);
}
static void mw_PRIM_PTR_COPY (void) {
    push_u64(45LL);
}
static void mw_PRIM_PTR_FILL (void) {
    push_u64(46LL);
}
static void mw_PRIM_PTR_RAW (void) {
    push_u64(47LL);
}
static void mw_PRIM_STR_ALLOC (void) {
    push_u64(48LL);
}
static void mw_PRIM_STR_SIZE (void) {
    push_u64(49LL);
}
static void mw_PRIM_STR_BASE (void) {
    push_u64(50LL);
}
static void mw_PRIM_STR_EQ (void) {
    push_u64(51LL);
}
static void mw_PRIM_U8_GET (void) {
    push_u64(52LL);
}
static void mw_PRIM_U8_SET (void) {
    push_u64(53LL);
}
static void mw_PRIM_U16_GET (void) {
    push_u64(54LL);
}
static void mw_PRIM_U16_SET (void) {
    push_u64(55LL);
}
static void mw_PRIM_U32_GET (void) {
    push_u64(56LL);
}
static void mw_PRIM_U32_SET (void) {
    push_u64(57LL);
}
static void mw_PRIM_U64_GET (void) {
    push_u64(58LL);
}
static void mw_PRIM_U64_SET (void) {
    push_u64(59LL);
}
static void mw_PRIM_I8_GET (void) {
    push_u64(60LL);
}
static void mw_PRIM_I8_SET (void) {
    push_u64(61LL);
}
static void mw_PRIM_I16_GET (void) {
    push_u64(62LL);
}
static void mw_PRIM_I16_SET (void) {
    push_u64(63LL);
}
static void mw_PRIM_I32_GET (void) {
    push_u64(64LL);
}
static void mw_PRIM_I32_SET (void) {
    push_u64(65LL);
}
static void mw_PRIM_I64_GET (void) {
    push_u64(66LL);
}
static void mw_PRIM_I64_SET (void) {
    push_u64(67LL);
}
static void mw_PRIM_SYS_OS (void) {
    push_u64(68LL);
}
static void mw_PRIM_SYS_ARGC (void) {
    push_u64(69LL);
}
static void mw_PRIM_SYS_ARGV (void) {
    push_u64(70LL);
}
static void mw_PRIM_POSIX_READ (void) {
    push_u64(71LL);
}
static void mw_PRIM_POSIX_WRITE (void) {
    push_u64(72LL);
}
static void mw_PRIM_POSIX_OPEN (void) {
    push_u64(73LL);
}
static void mw_PRIM_POSIX_CLOSE (void) {
    push_u64(74LL);
}
static void mw_PRIM_POSIX_EXIT (void) {
    push_u64(75LL);
}
static void mw_PRIM_POSIX_MMAP (void) {
    push_u64(76LL);
}
static void mw_PRIM_SYNTAX_MODULE (void) {
    push_u64(77LL);
}
static void mw_PRIM_SYNTAX_IMPORT (void) {
    push_u64(78LL);
}
static void mw_PRIM_SYNTAX_DEF (void) {
    push_u64(79LL);
}
static void mw_PRIM_SYNTAX_DEF_MISSING (void) {
    push_u64(80LL);
}
static void mw_PRIM_SYNTAX_DEF_TYPE (void) {
    push_u64(81LL);
}
static void mw_PRIM_SYNTAX_BUFFER (void) {
    push_u64(82LL);
}
static void mw_PRIM_SYNTAX_VARIABLE (void) {
    push_u64(83LL);
}
static void mw_PRIM_SYNTAX_DEF_EXTERNAL (void) {
    push_u64(84LL);
}
static void mw_PRIM_SYNTAX_TARGET_C99 (void) {
    push_u64(85LL);
}
static void mw_PRIM_SYNTAX_EMBED_STR (void) {
    push_u64(86LL);
}
static void mw_PRIM_SYNTAX_TABLE (void) {
    push_u64(87LL);
}
static void mw_PRIM_SYNTAX_FIELD (void) {
    push_u64(88LL);
}
static void mw_PRIM_SYNTAX_DATA (void) {
    push_u64(89LL);
}
static void mw_PRIM_SYNTAX_DASHES (void) {
    push_u64(90LL);
}
static void mw_PRIM_SYNTAX_ARROW (void) {
    push_u64(91LL);
}

static void mw_STR_BUF (void) {
    static uint8_t b[8192] = {0};
    push_ptr(&b);
}
static void mw_INPUT_BUFFER (void) {
    static uint8_t b[8208] = {0};
    push_ptr(&b);
}
static void mw_Name_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_NAME_HASH_TABLE (void) {
    static uint8_t b[524288] = {0};
    push_ptr(&b);
}
static void mw_Module_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Token_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Buffer_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_MetaVar_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Data_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Tag_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Atom_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Arrow_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Lambda_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Block_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Match_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Case_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Var_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Word_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Table_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Field_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_CODEGEN_BUF (void) {
    static uint8_t b[256] = {0};
    push_ptr(&b);
}
static void mw_External_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}
static void mw_Variable_2E_NUM (void) {
    static uint8_t b[8] = {0};
    push_ptr(&b);
}

void mw_STR_BUF_LEN() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_source_path_root() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_output_path_root() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_input_isopen() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_input_length() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_input_offset() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_input_handle() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_num_warnings() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_num_errors() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_lexer_module() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_lexer_row() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_lexer_col() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_lexer_stack() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_codegen_file() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_codegen_length() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_c99_depth() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_ab_home() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_ab_homeidx() {
    static VAL v = {0};
    push_ptr(&v);
}
void mw_ab_arrow() {
    static VAL v = {0};
    push_ptr(&v);
}

int64_t stat (int64_t, int64_t);
static void mw_stat (void) {
    int64_t x1 = pop_i64();
    int64_t x0 = pop_i64();
    push_i64(stat(x0, x1));
}

static void mw_id (void);
static void mw__3F__3F_ (void);
static void mw_swap (void);
static void mw_dup (void);
static void mw_drop (void);
static void mw_run (void);
static void mw__3D__3D_ (void);
static void mw__3C__3D_ (void);
static void mw__3C_ (void);
static void mw_value_40_ (void);
static void mw_value_21_ (void);
static void mw__2B_ (void);
static void mw__ (void);
static void mw__2A_ (void);
static void mw__2F_ (void);
static void mw__25_ (void);
static void mw__26_ (void);
static void mw__7C_ (void);
static void mw__5E_ (void);
static void mw__3C__3C_ (void);
static void mw__3E__3E_ (void);
static void mw_int_40_ (void);
static void mw_int_21_ (void);
static void mw_true (void);
static void mw_false (void);
static void mw__26__26_ (void);
static void mw__7C__7C_ (void);
static void mw_ptr_2B_ (void);
static void mw_with_ptr_2B_ (void);
static void mw_ptr_40_ (void);
static void mw_ptr_21_ (void);
static void mw__7C_ptr_7C_ (void);
static void mw_with_raw_ptr (void);
static void mw_u8_40_ (void);
static void mw_u8_21_ (void);
static void mw_u16_40_ (void);
static void mw_u16_21_ (void);
static void mw_u32_40_ (void);
static void mw_u32_21_ (void);
static void mw_u64_40_ (void);
static void mw_u64_21_ (void);
static void mw_i8_40_ (void);
static void mw_i8_21_ (void);
static void mw_i16_40_ (void);
static void mw_i16_21_ (void);
static void mw_i32_40_ (void);
static void mw_i32_21_ (void);
static void mw_i64_40_ (void);
static void mw_i64_21_ (void);
static void mw_OS__3E_Int (void);
static void mw_Int__3E_OS (void);
static void mw_RUNNING_OS (void);
static void mw_argc (void);
static void mw_argv (void);
static void mw_posix_read_21_ (void);
static void mw_posix_write_21_ (void);
static void mw_posix_open_21_ (void);
static void mw_posix_close_21_ (void);
static void mw_posix_exit_21_ (void);
static void mw_rotr (void);
static void mw_rotl (void);
static void mw_over (void);
static void mw_over2 (void);
static void mw_over3 (void);
static void mw_tuck (void);
static void mw_nip (void);
static void mw_dup2 (void);
static void mw_dup3 (void);
static void mw_dip_3F_ (void);
static void mw_dip_27_ (void);
static void mw_dip2 (void);
static void mw_dip3 (void);
static void mw_sip (void);
static void mw_sip2 (void);
static void mw_drop2 (void);
static void mw_drop3 (void);
static void mw_drop4 (void);
static void mw_rot4r (void);
static void mw_rot4l (void);
static void mw_or (void);
static void mw_and (void);
static void mw_repeat (void);
static void mw_count (void);
static void mw_countdown (void);
static void mw_Str__3E_Ptr (void);
static void mw_Ptr__3E_Str (void);
static void mw_U8_MAX (void);
static void mw_U16_MAX (void);
static void mw_U32_MAX (void);
static void mw_I8_MAX (void);
static void mw_I16_MAX (void);
static void mw_I32_MAX (void);
static void mw_U8_MIN (void);
static void mw_U16_MIN (void);
static void mw_U32_MIN (void);
static void mw_I8_MIN (void);
static void mw_I16_MIN (void);
static void mw_I32_MIN (void);
static void mw_not (void);
static void mw_cmp (void);
static void mw_cmp_3F_ (void);
static void mw__3C__3E_ (void);
static void mw__3E_ (void);
static void mw__3E__3D_ (void);
static void mw_0_3D_ (void);
static void mw_0_3C_ (void);
static void mw_0_3E_ (void);
static void mw_0_3C__3E_ (void);
static void mw_1_2B_ (void);
static void mw_1_ (void);
static void mw_max (void);
static void mw_min (void);
static void mw_ptrs (void);
static void mw_ptr_40__40_ (void);
static void mw_ptr_21__21_ (void);
static void mw_u8_40__40_ (void);
static void mw_u8_21__21_ (void);
static void mw_ints (void);
static void mw_int_40__40_ (void);
static void mw_int_21__21_ (void);
static void mw_values (void);
static void mw_value_40__40_ (void);
static void mw_value_21__21_ (void);
static void mw_in_range (void);
static void mw_Int__3E_U8 (void);
static void mw_Int__3E_U16 (void);
static void mw_Int__3E_U32 (void);
static void mw_Int__3E_U64 (void);
static void mw_Int__3E_I8 (void);
static void mw_Int__3E_I16 (void);
static void mw_Int__3E_I32 (void);
static void mw_Int__3E_I64 (void);
static void mw_U8__3E_Int (void);
static void mw_U16__3E_Int (void);
static void mw_U32__3E_Int (void);
static void mw_U64__3E_Int (void);
static void mw_I8__3E_Int (void);
static void mw_I16__3E_Int (void);
static void mw_I32__3E_Int (void);
static void mw_I64__3E_Int (void);
static void mw_Int__3E_Char (void);
static void mw_Char__3E_Int (void);
static void mw_negate (void);
static void mw_abs (void);
static void mw_nil (void);
static void mw_is_nil (void);
static void mw_is_nil_3F_ (void);
static void mw_pack_nil (void);
static void mw_pack_nil_3F_ (void);
static void mw_pack0 (void);
static void mw_pack1 (void);
static void mw_pack2 (void);
static void mw_pack3 (void);
static void mw_pack4 (void);
static void mw_pack5 (void);
static void mw_unpack0 (void);
static void mw_unpack1 (void);
static void mw_unpack2 (void);
static void mw_unpack3 (void);
static void mw_unpack4 (void);
static void mw_unpack5 (void);
static void mw_mut (void);
static void mw__40_ (void);
static void mw__21_ (void);
static void mw_modify (void);
static void mw_is_none (void);
static void mw_is_some (void);
static void mw_is_none_3F_ (void);
static void mw_is_some_3F_ (void);
static void mw_unwrap (void);
static void mw_unwrap_or (void);
static void mw_maybe_map (void);
static void mw_maybe_bind (void);
static void mw_maybe_for (void);
static void mw_maybe_filter (void);
static void mw_while_some (void);
static void mw_L4 (void);
static void mw_L5 (void);
static void mw_L6 (void);
static void mw_L7 (void);
static void mw_L8 (void);
static void mw_L9 (void);
static void mw_L10 (void);
static void mw_L11 (void);
static void mw_L12 (void);
static void mw_L4_2B_ (void);
static void mw_L5_2B_ (void);
static void mw_L6_2B_ (void);
static void mw_L7_2B_ (void);
static void mw_L8_2B_ (void);
static void mw_L9_2B_ (void);
static void mw_L10_2B_ (void);
static void mw_L11_2B_ (void);
static void mw_L12_2B_ (void);
static void mw_List_2B___3E_List (void);
static void mw_List__3E_List_2B_ (void);
static void mw_len (void);
static void mw_len_2B_ (void);
static void mw_len_3F_ (void);
static void mw_len_2B__3F_ (void);
static void mw_cons_2B_ (void);
static void mw_snoc_2B_ (void);
static void mw_cons_2B__2B_ (void);
static void mw_snoc_2B__2B_ (void);
static void mw_cons (void);
static void mw_snoc (void);
static void mw_uncons (void);
static void mw_unsnoc (void);
static void mw_cat (void);
static void mw_cat__2B_ (void);
static void mw_cat_2B__ (void);
static void mw_cat_2B_ (void);
static void mw_cat_aux (void);
static void mw_rebalance_2B_ (void);
static void mw_split_half_left (void);
static void mw_split_half_right (void);
static void mw_split_half (void);
static void mw_first (void);
static void mw_last (void);
static void mw_middle (void);
static void mw_first_2B_ (void);
static void mw_last_2B_ (void);
static void mw_middle_2B_ (void);
static void mw_reverse (void);
static void mw_reverse_2B_ (void);
static void mw_map (void);
static void mw_map_2B_ (void);
static void mw_for (void);
static void mw_for_2B_ (void);
static void mw_reverse_for (void);
static void mw_reverse_for_2B_ (void);
static void mw_reduce (void);
static void mw_reduce_2B_ (void);
static void mw_filter (void);
static void mw_filter_2B_ (void);
static void mw_find (void);
static void mw_find_2B_ (void);
static void mw_find_3F_ (void);
static void mw_reverse_find (void);
static void mw_reverse_find_3F_ (void);
static void mw_any (void);
static void mw_any_3F_ (void);
static void mw_all (void);
static void mw_all_3F_ (void);
static void mw_collect (void);
static void mw_collect_while (void);
static void mw_char_bytes (void);
static void mw_char_valid_wobbly (void);
static void mw_char_valid_3_wobbly (void);
static void mw_char_valid_3F_ (void);
static void mw_char_valid (void);
static void mw_char_valid_1 (void);
static void mw_char_valid_2 (void);
static void mw_char_valid_3 (void);
static void mw_char_valid_4 (void);
static void mw_char_codepoint (void);
static void mw_char_codepoint_1 (void);
static void mw_char_codepoint_2 (void);
static void mw_char_codepoint_3 (void);
static void mw_char_codepoint_4 (void);
static void mw_char_21_ (void);
static void mw_char_21__precise (void);
static void mw_char_21__2B__2B_ (void);
static void mw_char_3F__2B__2B_ (void);
static void mw_char_3F_ (void);
static void mw_char_40_ (void);
static void mw_char_40__width (void);
static void mw_char_width (void);
static void mw_char_width_3F_ (void);
static void mw_is_nul_3F_ (void);
static void mw_is_tab_3F_ (void);
static void mw_is_newline_3F_ (void);
static void mw_is_vtab_3F_ (void);
static void mw_is_return_3F_ (void);
static void mw_is_space_3F_ (void);
static void mw_is_quote_3F_ (void);
static void mw_is_pound_3F_ (void);
static void mw_is_lparen_3F_ (void);
static void mw_is_rparen_3F_ (void);
static void mw_is_asterisk_3F_ (void);
static void mw_is_plus_3F_ (void);
static void mw_is_comma_3F_ (void);
static void mw_is_minus_3F_ (void);
static void mw_is_dash_3F_ (void);
static void mw_is_dot_3F_ (void);
static void mw_is_digit_3F_ (void);
static void mw_is_colon_3F_ (void);
static void mw_is_semicolon_3F_ (void);
static void mw_is_lt_3F_ (void);
static void mw_is_eq_3F_ (void);
static void mw_is_gt_3F_ (void);
static void mw_is_question_mark_3F_ (void);
static void mw_is_upper_3F_ (void);
static void mw_is_upper_hexdigit_3F_ (void);
static void mw_is_lsquare_3F_ (void);
static void mw_is_backslash_3F_ (void);
static void mw_is_rsquare_3F_ (void);
static void mw_is_underscore_3F_ (void);
static void mw_is_lower_3F_ (void);
static void mw_is_lower_hexdigit_3F_ (void);
static void mw_is_n_3F_ (void);
static void mw_is_r_3F_ (void);
static void mw_is_t_3F_ (void);
static void mw_is_lcurly_3F_ (void);
static void mw_is_pipe_3F_ (void);
static void mw_is_rcurly_3F_ (void);
static void mw_is_whitespace_3F_ (void);
static void mw_is_hexdigit_3F_ (void);
static void mw_is_sign_3F_ (void);
static void mw_is_alpha_3F_ (void);
static void mw_is_string_end_3F_ (void);
static void mw_underscore (void);
static void mw_is_visible_3F_ (void);
static void mw_is_name_char_3F_ (void);
static void mw_is_special_char_3F_ (void);
static void mw_str_head (void);
static void mw_str_head_width (void);
static void mw_str_tail (void);
static void mw_str_size_3F_ (void);
static void mw_str_size (void);
static void mw_str_length (void);
static void mw_str_length_3F_ (void);
static void mw_str_concat (void);
static void mw_str_cat (void);
static void mw_str_is_empty (void);
static void mw_str_is_empty_3F_ (void);
static void mw_str_copy_partial_21_ (void);
static void mw_STR_BUF_SIZE (void);
static void mw_build_str_21_ (void);
static void mw_str_buf_dup_21_ (void);
static void mw_str_buf_length_3F_ (void);
static void mw_str_buf_length_21_ (void);
static void mw_str_buf_u8_40_ (void);
static void mw_str_buf_u8_21_ (void);
static void mw_str_buf_char_40_ (void);
static void mw_str_buf_empty_3F_ (void);
static void mw_str_buf_full_3F_ (void);
static void mw_str_buf_clear_21_ (void);
static void mw_str_buf_push_u8_21_ (void);
static void mw_str_buf_push_char_21_ (void);
static void mw_str_buf_push_str_21_ (void);
static void mw_str_buf_push_ptr_21_ (void);
static void mw_str_buf_21_ (void);
static void mw_str_buf_char_21_ (void);
static void mw_to_digit (void);
static void mw_str_buf_int_21_ (void);
static void mw_int_show (void);
static void mw_str_buf_swap_u8_21_ (void);
static void mw_str_buf_reverse_21_ (void);
static void mw_str_eq (void);
static void mw_str_eq_3F_ (void);
static void mw_str_for (void);
static void mw_str_transduce (void);
static void mw_str_transduce_step (void);
static void mw_str_chars (void);
static void mw_str_codepoints (void);
static void mw_str_bytes (void);
static void mw_Str__3E_Path (void);
static void mw_Path__3E_Str (void);
static void mw_path_40_ (void);
static void mw_path_21_ (void);
static void mw_init_paths_21_ (void);
static void mw_path_separator (void);
static void mw_path_is_empty_3F_ (void);
static void mw_path_join (void);
static void mw_make_source_path (void);
static void mw_make_output_path (void);
static void mw_panic_21_ (void);
static void mw_Int__3E_File (void);
static void mw_File__3E_Int (void);
static void mw_file_40_ (void);
static void mw_file_21_ (void);
static void mw_stdin (void);
static void mw_stdout (void);
static void mw_stderr (void);
static void mw_str_write_21_ (void);
static void mw_str_print_21_ (void);
static void mw_str_trace_21_ (void);
static void mw_str_print_ln_21_ (void);
static void mw_str_trace_ln_21_ (void);
static void mw_print_ln_21_ (void);
static void mw_trace_ln_21_ (void);
static void mw_str_buf_write_21_ (void);
static void mw_str_buf_print_21_ (void);
static void mw_str_buf_trace_21_ (void);
static void mw_str_buf_read_21_ (void);
static void mw_str_buf_input_21_ (void);
static void mw_print_char_21_ (void);
static void mw_trace_char_21_ (void);
static void mw_int_write_21_ (void);
static void mw_int_print_21_ (void);
static void mw_int_trace_21_ (void);
static void mw_int_print_ln_21_ (void);
static void mw_int_trace_ln_21_ (void);
static void mw_with_open_file_21_ (void);
static void mw_read_file_21_ (void);
static void mw_open_file_21_ (void);
static void mw_create_file_21_ (void);
static void mw_O_WRONLY_7C_O_CREAT_7C_O_TRUNC (void);
static void mw_close_file_21_ (void);
static void mw_with_raw_path (void);
static void mw_is_directory_3F_ (void);
static void mw_S_IFMT (void);
static void mw_S_IFDIR (void);
static void mw_S_IFREG (void);
static void mw_S_ISDIR (void);
static void mw_st_mode_40_ (void);
static void mw_INPUT_BUFFER_SIZE (void);
static void mw_input_start_21_ (void);
static void mw_input_end_21_ (void);
static void mw_input_done_3F_ (void);
static void mw_input_fill_buffer_21_ (void);
static void mw_input_peek (void);
static void mw_input_move_21_ (void);
static void mw_input_prepare_for_more_21_ (void);
static void mw_input_fill_buffer_tragic_21_ (void);
static void mw_stack_push_21_ (void);
static void mw_stack_pop_21_ (void);
static void mw_stack_uncons (void);
static void mw_stack_reset_21_ (void);
static void mw_stack_is_empty (void);
static void mw_Name_2E_MAX (void);
static void mw_Name_2E_id (void);
static void mw_Name_2E_succ (void);
static void mw_Name_2E_pred (void);
static void mw_Name_2E_for (void);
static void mw_Name_2E_alloc_21_ (void);
static void mw_Int__3E_Hash (void);
static void mw_Hash__3E_Int (void);
static void mw_NAME_HASH_MAX (void);
static void mw_hash_name_21_ (void);
static void mw_hash_name_40_ (void);
static void mw_hash_name_3F_ (void);
static void mw_hash (void);
static void mw_name_hash (void);
static void mw_next_hash (void);
static void mw_name_keep_going_3F_ (void);
static void mw_name_new_21_ (void);
static void mw_name_cat_21_ (void);
static void mw_name_trace_21_ (void);
static void mw_name_print_21_ (void);
static void mw_name_could_be_type (void);
static void mw_name_could_be_type_var (void);
static void mw_str_could_be_type_var (void);
static void mw_name_could_be_type_con (void);
static void mw_name_is_type_hole (void);
static void mw_name_is_underscore (void);
static void mw_name_could_be_stack_var (void);
static void mw_name_could_be_effect_con (void);
static void mw_name_print_mangled_21_ (void);
static void mw_name_mangle_21_ (void);
static void mw_name_mangle_compute_21_ (void);
static void mw_char_hexdigits (void);
static void mw_char_hexdigits_first (void);
static void mw_char_hexdigits_next (void);
static void mw_hexdigit (void);
static void mw_name_undefined_3F_ (void);
static void mw_name_defined_3F_ (void);
static void mw_unSET (void);
static void mw_set_snoc (void);
static void mw_set_cons (void);
static void mw_Module_2E_MAX (void);
static void mw_Module_2E_id (void);
static void mw_Module_2E_succ (void);
static void mw_Module_2E_pred (void);
static void mw_Module_2E_for (void);
static void mw_Module_2E_alloc_21_ (void);
static void mw_Token_2E_MAX (void);
static void mw_Token_2E_id (void);
static void mw_Token_2E_succ (void);
static void mw_Token_2E_pred (void);
static void mw_Token_2E_for (void);
static void mw_Token_2E_alloc_21_ (void);
static void mw_module_new_21_ (void);
static void mw_module_add_import_21_ (void);
static void mw_module_source_path (void);
static void mw_module_path_from_name (void);
static void mw_Int__3E_Row (void);
static void mw_Row__3E_Int (void);
static void mw_Int__3E_Col (void);
static void mw_Col__3E_Int (void);
static void mw_location_pack (void);
static void mw_location_unpack (void);
static void mw_location_trace_21_ (void);
static void mw_emit_warning_at_21_ (void);
static void mw_emit_error_at_21_ (void);
static void mw_emit_fatal_error_at_21_ (void);
static void mw_token_alloc_21_ (void);
static void mw_token_is_int_3F_ (void);
static void mw_token_is_str_3F_ (void);
static void mw_token_is_name_3F_ (void);
static void mw_token_is_comma_3F_ (void);
static void mw_token_is_arrow_3F_ (void);
static void mw_token_is_dashes_3F_ (void);
static void mw_token_is_lparen_3F_ (void);
static void mw_token_is_rparen_3F_ (void);
static void mw_token_is_lsquare_3F_ (void);
static void mw_token_is_rsquare_3F_ (void);
static void mw_token_is_lcurly_3F_ (void);
static void mw_token_is_rcurly_3F_ (void);
static void mw_token_name_40_ (void);
static void mw_token_name_3F_ (void);
static void mw_token_str_40_ (void);
static void mw_token_str_3F_ (void);
static void mw_token_int_40_ (void);
static void mw_token_int_3F_ (void);
static void mw_token_is_arg_end_3F_ (void);
static void mw_token_is_left_enclosure_3F_ (void);
static void mw_token_is_right_enclosure_3F_ (void);
static void mw_token_location (void);
static void mw_token_location_3F_ (void);
static void mw_token_succ (void);
static void mw_token_pred (void);
static void mw_token_next (void);
static void mw_token_prev (void);
static void mw_token_next_arg_end (void);
static void mw_token_has_args_3F_ (void);
static void mw_token_num_args (void);
static void mw_token_num_args_3F_ (void);
static void mw_token_args_0 (void);
static void mw_token_args_1 (void);
static void mw_token_args_2 (void);
static void mw_token_args_3 (void);
static void mw_token_args (void);
static void mw_token_is_args_end_3F_ (void);
static void mw_token_args_2B_ (void);
static void mw_token_args_2_2B_ (void);
static void mw_emit_warning_21_ (void);
static void mw_emit_error_21_ (void);
static void mw_emit_fatal_error_21_ (void);
static void mw_token_is_module_end_3F_ (void);
static void mw_token_run_end_3F_ (void);
static void mw_token_run (void);
static void mw_token_run_has_arrow (void);
static void mw_token_run_has_dashes (void);
static void mw_sig_is_stack_end_3F_ (void);
static void mw_sig_is_stack_end2_3F_ (void);
static void mw_sig_next_stack_end (void);
static void mw_sig_has_dashes (void);
static void mw_sig_has_dashes_3F_ (void);
static void mw_sig_arity (void);
static void mw_sig_count_types (void);
static void mw_sig_token_is_type_3F_ (void);
static void mw_sig_token_is_type_con_3F_ (void);
static void mw_sig_token_is_type_hole_3F_ (void);
static void mw_token_is_underscore_3F_ (void);
static void mw_sig_token_is_type_var_3F_ (void);
static void mw_token_is_param_name_3F_ (void);
static void mw_sig_token_is_stack_var_3F_ (void);
static void mw_sig_token_is_effect_con_3F_ (void);
static void mw_sig_skip_dashes (void);
static void mw_run_lexer_21_ (void);
static void mw_lexer_done_3F_ (void);
static void mw_lexer_make_21_ (void);
static void mw_lexer_emit_21_ (void);
static void mw_lexer_next_21_ (void);
static void mw_lexer_newline_21_ (void);
static void mw_lexer_emit_lparen_21_ (void);
static void mw_lexer_emit_rparen_21_ (void);
static void mw_lexer_emit_lsquare_21_ (void);
static void mw_lexer_emit_rsquare_21_ (void);
static void mw_lexer_emit_lcurly_21_ (void);
static void mw_lexer_emit_rcurly_21_ (void);
static void mw_lexer_emit_name_21_ (void);
static void mw_str_buf_is_doc_start_3F_ (void);
static void mw_str_buf_is_arrow_3F_ (void);
static void mw_str_buf_is_dashes_3F_ (void);
static void mw_str_buf_is_equal_3F_ (void);
static void mw_str_buf_is_int_3F_ (void);
static void mw_str_buf_is_dec_int_3F_ (void);
static void mw_is_zero_char (void);
static void mw_is_xX_char (void);
static void mw_str_buf_is_hex_int_3F_ (void);
static void mw_str_buf_int_3F_ (void);
static void mw_str_buf_dec_int_3F_ (void);
static void mw_str_buf_hex_int_3F_ (void);
static void mw_hexdigit_value (void);
static void mw_lexer_emit_string_21_ (void);
static void mw_lexer_push_string_char_21_ (void);
static void mw_lexer_skip_comment_21_ (void);
static void mw_lexer_skip_doc_21_ (void);
static void mw_lexer_comment_end_3F_ (void);
static void mw_lexer_peek (void);
static void mw_lexer_move_21_ (void);
static void mw_lexer_location (void);
static void mw_lexer_emit_warning_21_ (void);
static void mw_lexer_emit_error_21_ (void);
static void mw_lexer_emit_fatal_error_21_ (void);
static void mw_Buffer_2E_MAX (void);
static void mw_Buffer_2E_id (void);
static void mw_Buffer_2E_succ (void);
static void mw_Buffer_2E_pred (void);
static void mw_Buffer_2E_for (void);
static void mw_Buffer_2E_alloc_21_ (void);
static void mw_buffer_new_21_ (void);
static void mw_unBAG (void);
static void mw_B0 (void);
static void mw_B1 (void);
static void mw_B2 (void);
static void mw_B3 (void);
static void mw_unBAG_2B_ (void);
static void mw_B1_2B_ (void);
static void mw_B2_2B_ (void);
static void mw_B3_2B_ (void);
static void mw_Bag_2B___3E_Bag (void);
static void mw_Bag__3E_Bag_2B_ (void);
static void mw_bag_empty (void);
static void mw_bag_is_empty (void);
static void mw_bag_singleton (void);
static void mw_bag_is_singleton (void);
static void mw_bag_is_singleton_2B_ (void);
static void mw_bag_len (void);
static void mw_bag_len_2B_ (void);
static void mw_bag_len_3F_ (void);
static void mw_bag_len_2B__3F_ (void);
static void mw_bag_first (void);
static void mw_bag_last (void);
static void mw_bag_middle (void);
static void mw_bag_first_2B_ (void);
static void mw_bag_last_2B_ (void);
static void mw_bag_middle_2B_ (void);
static void mw_bag_split_half_left (void);
static void mw_bag_split_half_right (void);
static void mw_bag_split_half (void);
static void mw_bag_uncons (void);
static void mw_bag_unsnoc (void);
static void mw_bag_cons (void);
static void mw_bag_snoc (void);
static void mw_bag_cons_2B_ (void);
static void mw_bag_snoc_2B_ (void);
static void mw_bag_cons_2B__2B_ (void);
static void mw_bag_snoc_2B__2B_ (void);
static void mw_bag_has (void);
static void mw_bag_has_2B_ (void);
static void mw_bag_has_3F_ (void);
static void mw_bag_insert (void);
static void mw_bag_insert_2B_ (void);
static void mw_bag_insert_2B__2B_ (void);
static void mw_bag_replace (void);
static void mw_bag_cat_unsafe (void);
static void mw_bag_cat_unsafe__2B_ (void);
static void mw_bag_cat_unsafe_2B_ (void);
static void mw_order2 (void);
static void mw_order3 (void);
static void mw_bag_lookup_key (void);
static void mw_bag_lookup_key_2B_ (void);
static void mw_bag_lookup_key_3F_ (void);
static void mw_bag_replace_key (void);
static void mw_bag_replace_key_2B_ (void);
static void mw_bag_replace_key_2B__2B_ (void);
static void mw__3D__3D_key (void);
static void mw__3C__3D_key (void);
static void mw_bag_values (void);
static void mw_bag_values_2B_ (void);
static void mw_unMAP (void);
static void mw_map_empty (void);
static void mw_map_is_empty (void);
static void mw_map_is_empty_3F_ (void);
static void mw_map_singleton (void);
static void mw_map_is_singleton (void);
static void mw_map_has (void);
static void mw_map_has_3F_ (void);
static void mw_map_lookup (void);
static void mw_map_lookup_3F_ (void);
static void mw_map_insert (void);
static void mw_map_cons (void);
static void mw_map_snoc (void);
static void mw_map_pairs (void);
static void mw_map_keys (void);
static void mw_map_values (void);
static void mw_prim_type_is_physical (void);
static void mw_MetaVar_2E_MAX (void);
static void mw_MetaVar_2E_id (void);
static void mw_MetaVar_2E_succ (void);
static void mw_MetaVar_2E_pred (void);
static void mw_MetaVar_2E_for (void);
static void mw_MetaVar_2E_alloc_21_ (void);
static void mw_Data_2E_MAX (void);
static void mw_Data_2E_id (void);
static void mw_Data_2E_succ (void);
static void mw_Data_2E_pred (void);
static void mw_Data_2E_for (void);
static void mw_Data_2E_alloc_21_ (void);
static void mw_Tag_2E_MAX (void);
static void mw_Tag_2E_id (void);
static void mw_Tag_2E_succ (void);
static void mw_Tag_2E_pred (void);
static void mw_Tag_2E_for (void);
static void mw_Tag_2E_alloc_21_ (void);
static void mw_def_type_21_ (void);
static void mw_init_types_21_ (void);
static void mw_T_2A_ (void);
static void mw_T__3E_ (void);
static void mw_TMut (void);
static void mw_T0 (void);
static void mw_T1 (void);
static void mw_T2 (void);
static void mw_T3 (void);
static void mw_T4 (void);
static void mw_T5 (void);
static void mw_T6 (void);
static void mw_type_is_morphism (void);
static void mw_type_is_morphism_3F_ (void);
static void mw_type_is_physical (void);
static void mw_TYPE_TYPE (void);
static void mw_TYPE_STACK (void);
static void mw_TYPE_EFFECT (void);
static void mw_TYPE_UNIT (void);
static void mw_TYPE_BOOL (void);
static void mw_TYPE_MUT (void);
static void mw_TYPE_INT (void);
static void mw_TYPE_PTR (void);
static void mw_TYPE_STR (void);
static void mw_TYPE_CHAR (void);
static void mw_TYPE_U8 (void);
static void mw_TYPE_U16 (void);
static void mw_TYPE_U32 (void);
static void mw_TYPE_U64 (void);
static void mw_TYPE_I8 (void);
static void mw_TYPE_I16 (void);
static void mw_TYPE_I32 (void);
static void mw_TYPE_I64 (void);
static void mw_type_expand (void);
static void mw_gamma_token_40_ (void);
static void mw_gamma_token_3F_ (void);
static void mw_type_unify_failed_21_ (void);
static void mw_type_unify_21_ (void);
static void mw_value_unify_21_ (void);
static void mw_type_value_unify_21_ (void);
static void mw_value_type_unify_21_ (void);
static void mw_arrow_type (void);
static void mw_block_infer_type_21_ (void);
static void mw_type_unify_pair_21_ (void);
static void mw_type_prim_unify_21_ (void);
static void mw_type_data_unify_21_ (void);
static void mw_type_table_unify_21_ (void);
static void mw_type_var_unify_21_ (void);
static void mw_type_has_meta_3F_ (void);
static void mw_type_has_meta (void);
static void mw_type2_has_meta (void);
static void mw_meta_has_meta (void);
static void mw_value_type_has_meta (void);
static void mw_type_trace_sig_21_ (void);
static void mw_type_trace_stack_dom_21_ (void);
static void mw_type_trace_stack_cod_21_ (void);
static void mw_type_trace_stack_21_ (void);
static void mw_type_trace_21_ (void);
static void mw_value_as_type (void);
static void mw_type_trace_prim_21_ (void);
static void mw_type_semifreshen_sig (void);
static void mw_type_semifreshen_sig_aux (void);
static void mw_type_semifreshen_sig_stack (void);
static void mw_type_freshen_sig (void);
static void mw_type_stack_rest (void);
static void mw_type_sig_needs_fresh_stack_rest (void);
static void mw_type_freshen_sig_aux (void);
static void mw_type_freshen_sig_stack (void);
static void mw_type_freshen (void);
static void mw_type_pair_freshen (void);
static void mw_meta_freshen (void);
static void mw_type_var_freshen (void);
static void mw_type_rigidify_sig_21_ (void);
static void mw_type_rigidify_stack_21_ (void);
static void mw_type_rigidify_21_ (void);
static void mw_type_rigidify_value_21_ (void);
static void mw_prim_type_arity (void);
static void mw_type_arity (void);
static void mw_type_head (void);
static void mw_type_max_count (void);
static void mw_meta_trace_21_ (void);
static void mw_meta_alloc_21_ (void);
static void mw_meta_expand_if (void);
static void mw_meta_expand (void);
static void mw_meta_unify_21_ (void);
static void mw_meta_expand_or_update_21_ (void);
static void mw_type_hole_unify_21_ (void);
static void mw_type_max_num_params (void);
static void mw_type_num_morphisms_on_top (void);
static void mw_app_type_trace_21_ (void);
static void mw_app_type_trace_open_21_ (void);
static void mw_unSUBST (void);
static void mw_subst_nil (void);
static void mw_subst_is_nil (void);
static void mw_subst_is_nil_3F_ (void);
static void mw_subst_new_21_ (void);
static void mw_subst_has_var (void);
static void mw_subst_has_var_3F_ (void);
static void mw_subst_get_var (void);
static void mw_subst_get_var_3F_ (void);
static void mw_subst_match_var (void);
static void mw_tag_num_inputs_3F_ (void);
static void mw_tag_is_transparent_3F_ (void);
static void mw_data_num_tags (void);
static void mw_data_add_tag_21_ (void);
static void mw_data_is_enum_3F_ (void);
static void mw_data_is_transparent (void);
static void mw_Atom_2E_MAX (void);
static void mw_Atom_2E_id (void);
static void mw_Atom_2E_succ (void);
static void mw_Atom_2E_pred (void);
static void mw_Atom_2E_for (void);
static void mw_Atom_2E_alloc_21_ (void);
static void mw_Arrow_2E_MAX (void);
static void mw_Arrow_2E_id (void);
static void mw_Arrow_2E_succ (void);
static void mw_Arrow_2E_pred (void);
static void mw_Arrow_2E_for (void);
static void mw_Arrow_2E_alloc_21_ (void);
static void mw_Lambda_2E_MAX (void);
static void mw_Lambda_2E_id (void);
static void mw_Lambda_2E_succ (void);
static void mw_Lambda_2E_pred (void);
static void mw_Lambda_2E_for (void);
static void mw_Lambda_2E_alloc_21_ (void);
static void mw_Block_2E_MAX (void);
static void mw_Block_2E_id (void);
static void mw_Block_2E_succ (void);
static void mw_Block_2E_pred (void);
static void mw_Block_2E_for (void);
static void mw_Block_2E_alloc_21_ (void);
static void mw_unPARAM (void);
static void mw_Var__3E_Param (void);
static void mw_Param__3E_Var (void);
static void mw_atom_arg_add_left_21_ (void);
static void mw_arrow_atom_add_21_ (void);
static void mw_block_new_21_ (void);
static void mw_block_new_deferred_21_ (void);
static void mw_block_force_21_ (void);
static void mw_block_unify_type_21_ (void);
static void mw_Match_2E_MAX (void);
static void mw_Match_2E_id (void);
static void mw_Match_2E_succ (void);
static void mw_Match_2E_pred (void);
static void mw_Match_2E_for (void);
static void mw_Match_2E_alloc_21_ (void);
static void mw_Case_2E_MAX (void);
static void mw_Case_2E_id (void);
static void mw_Case_2E_succ (void);
static void mw_Case_2E_pred (void);
static void mw_Case_2E_for (void);
static void mw_Case_2E_alloc_21_ (void);
static void mw_match_add_case_21_ (void);
static void mw_match_is_exhaustive_3F_ (void);
static void mw_match_has_default_case_3F_ (void);
static void mw_match_scrutinee_type_3F_ (void);
static void mw_match_scrutinee_data_3F_ (void);
static void mw_match_is_transparent_3F_ (void);
static void mw_cases_cover_case_3F_ (void);
static void mw_cases_cover_case (void);
static void mw_case_is_covered (void);
static void mw_cases_have_default_case (void);
static void mw_case_is_default_case (void);
static void mw_pattern_is_covered (void);
static void mw_ready (void);
static void mw_ready2 (void);
static void mw_delay (void);
static void mw_delay0 (void);
static void mw_delay2 (void);
static void mw_delay3 (void);
static void mw_delay4 (void);
static void mw_force_21_ (void);
static void mw_force_or_21_ (void);
static void mw_force2_21_ (void);
static void mw_force_or2_21_ (void);
static void mw_Var_2E_MAX (void);
static void mw_Var_2E_id (void);
static void mw_Var_2E_succ (void);
static void mw_Var_2E_pred (void);
static void mw_Var_2E_for (void);
static void mw_Var_2E_alloc_21_ (void);
static void mw_var_new_21_ (void);
static void mw_var_new_implicit_21_ (void);
static void mw_var_is_physical (void);
static void mw_Word_2E_MAX (void);
static void mw_Word_2E_id (void);
static void mw_Word_2E_succ (void);
static void mw_Word_2E_pred (void);
static void mw_Word_2E_for (void);
static void mw_Word_2E_alloc_21_ (void);
static void mw_Table_2E_MAX (void);
static void mw_Table_2E_id (void);
static void mw_Table_2E_succ (void);
static void mw_Table_2E_pred (void);
static void mw_Table_2E_for (void);
static void mw_Table_2E_alloc_21_ (void);
static void mw_Field_2E_MAX (void);
static void mw_Field_2E_id (void);
static void mw_Field_2E_succ (void);
static void mw_Field_2E_pred (void);
static void mw_Field_2E_for (void);
static void mw_Field_2E_alloc_21_ (void);
static void mw_CODEGEN_BUF_SIZE (void);
static void mw_codegen_u8_40_ (void);
static void mw_codegen_u8_21_ (void);
static void mw_codegen_full_3F_ (void);
static void mw_codegen_flush_21_ (void);
static void mw__2E_b (void);
static void mw__2E_c (void);
static void mw__2E_ (void);
static void mw_codegen_start_21_ (void);
static void mw_codegen_end_21_ (void);
static void mw_run_output_c99_21_ (void);
static void mw__2E_lf (void);
static void mw__3B_ (void);
static void mw__3B__3B_ (void);
static void mw__2E_n (void);
static void mw__2E_name (void);
static void mw__2E_w (void);
static void mw__2E_p (void);
static void mw__2E_pm (void);
static void mw_c99_header_21_ (void);
static void mw_c99_buffers_21_ (void);
static void mw_c99_buffer_21_ (void);
static void mw_c99_variables_21_ (void);
static void mw_c99_variable_21_ (void);
static void mw_c99_tags_21_ (void);
static void mw_c99_tag_21_ (void);
static void mw_c99_externals_21_ (void);
static void mw_c99_external_21_ (void);
static void mw_c99_nest (void);
static void mw_c99_indent (void);
static void mw_c99_line (void);
static void mw_c99_call_21_ (void);
static void mw_c99_arrow_21_ (void);
static void mw_c99_atom_21_ (void);
static void mw_c99_args_op_21_ (void);
static void mw_c99_int_21_ (void);
static void mw_c99_str_21_ (void);
static void mw_c99_constant_21_ (void);
static void mw_c99_string_char_21_ (void);
static void mw_c99_prim_21_ (void);
static void mw_c99_prim_default_21_ (void);
static void mw_c99_args_push_21_ (void);
static void mw_c99_arg_push_21_ (void);
static void mw_c99_arg_run_21_ (void);
static void mw__2E_var (void);
static void mw__2E_param (void);
static void mw_c99_pack_ctx_21_ (void);
static void mw_c99_unpack_ctx_21_ (void);
static void mw_c99_decref_ctx_21_ (void);
static void mw_c99_block_push_21_ (void);
static void mw_c99_var_21_ (void);
static void mw_c99_var_push_21_ (void);
static void mw_c99_lambda_21_ (void);
static void mw_c99_match_21_ (void);
static void mw_c99_case_21_ (void);
static void mw_c99_pattern_21_ (void);
static void mw_c99_word_sigs_21_ (void);
static void mw_c99_word_sig_21_ (void);
static void mw_c99_block_sigs_21_ (void);
static void mw_c99_block_sig_21_ (void);
static void mw_c99_field_sigs_21_ (void);
static void mw_c99_field_sig_21_ (void);
static void mw_c99_block_defs_21_ (void);
static void mw_c99_block_def_21_ (void);
static void mw__2E_block (void);
static void mw_c99_word_defs_21_ (void);
static void mw_c99_word_def_21_ (void);
static void mw_c99_field_defs_21_ (void);
static void mw_c99_field_def_21_ (void);
static void mw_c99_main_21_ (void);
static void mw_unCTX (void);
static void mw_ctx_empty (void);
static void mw_ctx_is_physically_empty (void);
static void mw_ctx_new_21_ (void);
static void mw_ctx_vars (void);
static void mw_ctx_physical_vars (void);
static void mw_ctx_lookup (void);
static void mw_ctx_len (void);
static void mw_ctx_fresh_name_21_ (void);
static void mw_ctx_make_fresh_stack_type_var_21_ (void);
static void mw_ctx_make_fresh_type_var_21_ (void);
static void mw_ctx_make_fresh_var_21_ (void);
static void mw_External_2E_MAX (void);
static void mw_External_2E_id (void);
static void mw_External_2E_succ (void);
static void mw_External_2E_pred (void);
static void mw_External_2E_for (void);
static void mw_External_2E_alloc_21_ (void);
static void mw_Variable_2E_MAX (void);
static void mw_Variable_2E_id (void);
static void mw_Variable_2E_succ (void);
static void mw_Variable_2E_pred (void);
static void mw_Variable_2E_for (void);
static void mw_Variable_2E_alloc_21_ (void);
static void mw_variable_new_21_ (void);
static void mw_type_elab_default (void);
static void mw_type_elab_stack_assertion (void);
static void mw_type_elab_holes_allowed (void);
static void mw_type_elab_ctx (void);
static void mw_type_elab_ctx_3F_ (void);
static void mw_type_elab_ctx_replace (void);
static void mw_elab_type_sig_21_ (void);
static void mw_elab_type_sig_params_21_ (void);
static void mw_elab_type_stack_21_ (void);
static void mw_elab_type_stack_rest_21_ (void);
static void mw_elab_type_arg_21_ (void);
static void mw_elab_type_atom_21_ (void);
static void mw_elab_stack_var_21_ (void);
static void mw_elab_type_var_21_ (void);
static void mw_elab_implicit_var_21_ (void);
static void mw_elab_type_con_21_ (void);
static void mw_elab_type_args_21_ (void);
static void mw_elab_type_hole_21_ (void);
static void mw_elab_type_dont_care_21_ (void);
static void mw_elab_type_quote_21_ (void);
static void mw_elab_type_unify_21_ (void);
static void mw_elab_simple_type_arg_21_ (void);
static void mw_ab_ctx (void);
static void mw_ab_token (void);
static void mw_ab_type (void);
static void mw_ab_save_21_ (void);
static void mw_ab_build_21_ (void);
static void mw_ab_build_hom_21_ (void);
static void mw_ab_build_word_arrow_21_ (void);
static void mw_ab_build_word_21_ (void);
static void mw_ab_unify_type_21_ (void);
static void mw_ab_atom_21_ (void);
static void mw_ab_optimized_snoc_21_ (void);
static void mw_atom_accepts_args_3F_ (void);
static void mw_atoms_has_last_block_3F_ (void);
static void mw_atoms_turn_last_block_to_arg (void);
static void mw_block_to_arg (void);
static void mw_arrow_to_run_var (void);
static void mw_atom_to_run_var (void);
static void mw_ab_op_21_ (void);
static void mw_ab_expand_opsig_21_ (void);
static void mw_ab_int_21_ (void);
static void mw_ab_str_21_ (void);
static void mw_ab_buffer_21_ (void);
static void mw_ab_variable_21_ (void);
static void mw_ab_constant_21_ (void);
static void mw_ab_field_21_ (void);
static void mw_ab_var_21_ (void);
static void mw_ab_tag_21_ (void);
static void mw_ab_prim_21_ (void);
static void mw_ab_word_21_ (void);
static void mw_ab_external_21_ (void);
static void mw_ab_block_at_21_ (void);
static void mw_ab_block_21_ (void);
static void mw_ab_dip_21_ (void);
static void mw_ab_if_21_ (void);
static void mw_ab_while_21_ (void);
static void mw_ab_lambda_21_ (void);
static void mw_elab_op_fresh_sig_21_ (void);
static void mw_elab_block_sig_21_ (void);
static void mw_elab_match_sig_21_ (void);
static void mw_elab_lambda_sig_21_ (void);
static void mw_elab_var_sig_21_ (void);
static void mw_elab_tag_sig_21_ (void);
static void mw_elab_tag_ctx_21_ (void);
static void mw_elab_tag_ctx_sig_21_ (void);
static void mw_elab_external_sig_21_ (void);
static void mw_elab_external_ctx_21_ (void);
static void mw_elab_external_ctx_sig_21_ (void);
static void mw_elab_word_sig_21_ (void);
static void mw_elab_word_ctx_21_ (void);
static void mw_elab_word_ctx_sig_21_ (void);
static void mw_emit_recursive_word_fatal_error_21_ (void);
static void mw_elab_word_ctx_sig_weak_21_ (void);
static void mw_elab_word_body_21_ (void);
static void mw_elab_arrow_21_ (void);
static void mw_elab_arrow_hom_21_ (void);
static void mw_elab_arrow_fwd_21_ (void);
static void mw_elab_atoms_21_ (void);
static void mw_elab_atoms_done_3F_ (void);
static void mw_elab_atom_21_ (void);
static void mw_elab_atom_block_21_ (void);
static void mw_elab_block_at_21_ (void);
static void mw_elab_args_21_ (void);
static void mw_elab_no_args_21_ (void);
static void mw_elab_atom_name_21_ (void);
static void mw_elab_prim_21_ (void);
static void mw_elab_atom_assert_21_ (void);
static void mw_elab_atom_lambda_21_ (void);
static void mw_elab_match_at_21_ (void);
static void mw_elab_atom_match_21_ (void);
static void mw_elab_lambda_21_ (void);
static void mw_elab_expand_tensor_21_ (void);
static void mw_elab_expand_morphism_21_ (void);
static void mw_elab_lambda_pop_from_mid_21_ (void);
static void mw_token_is_lambda_param_3F_ (void);
static void mw_elab_lambda_params_21_ (void);
static void mw_elab_lambda_body_21_ (void);
static void mw_elab_match_exhaustive_21_ (void);
static void mw_elab_match_cases_21_ (void);
static void mw_elab_match_case_21_ (void);
static void mw_elab_case_pattern_21_ (void);
static void mw_elab_case_body_21_ (void);
static void mw_elab_module_21_ (void);
static void mw_elab_module_header_21_ (void);
static void mw_elab_module_decl_21_ (void);
static void mw_elab_module_import_21_ (void);
static void mw_elab_data_21_ (void);
static void mw_elab_data_header_21_ (void);
static void mw_elab_data_tag_21_ (void);
static void mw_expect_token_comma (void);
static void mw_expect_token_rparen (void);
static void mw_expect_token_arrow (void);
static void mw_token_def_args (void);
static void mw_elab_def_missing_21_ (void);
static void mw_elab_def_21_ (void);
static void mw_elab_def_params_21_ (void);
static void mw_elab_def_body_21_ (void);
static void mw_elab_def_external_21_ (void);
static void mw_elab_def_type_21_ (void);
static void mw_elab_buffer_21_ (void);
static void mw_elab_variable_21_ (void);
static void mw_elab_table_21_ (void);
static void mw_elab_target_c99_21_ (void);
static void mw_elab_embed_str_21_ (void);
static void mw_typecheck_everything_21_ (void);
static void mw_typecheck_def_21_ (void);
static void mw_TABLE_MAX_SIZE (void);
static void mw_table_new_21_ (void);
static void mw_elab_field_21_ (void);
static void mw_field_new_21_ (void);
static void mw_elab_field_type_21_ (void);
static void mw_name_prim_3D_ (void);
static void mw_token_prim_3D__3F_ (void);
static void mw_token_prim_3D_ (void);
static void mw_def_prim_21_ (void);
static void mw_init_prims_21_ (void);
static void mw_init_21_ (void);
static void mw_compile_21_ (void);
static void mw_main (void);

static void mb_Name_2E_pred_1 (void);
static void mb_Name_2E_pred_2 (void);
static void mb_Name_2E_for_2 (void);
static void mb_Name_2E_for_4 (void);
static void mb_Name_2E_for_3 (void);
static void mb_Module_2E_pred_1 (void);
static void mb_Module_2E_pred_2 (void);
static void mb_Module_2E_for_2 (void);
static void mb_Module_2E_for_4 (void);
static void mb_Module_2E_for_3 (void);
static void mb_Token_2E_pred_1 (void);
static void mb_Token_2E_pred_2 (void);
static void mb_Token_2E_for_2 (void);
static void mb_Token_2E_for_4 (void);
static void mb_Token_2E_for_3 (void);
static void mb_Buffer_2E_pred_1 (void);
static void mb_Buffer_2E_pred_2 (void);
static void mb_Buffer_2E_for_2 (void);
static void mb_Buffer_2E_for_4 (void);
static void mb_Buffer_2E_for_3 (void);
static void mb_MetaVar_2E_pred_1 (void);
static void mb_MetaVar_2E_pred_2 (void);
static void mb_MetaVar_2E_for_2 (void);
static void mb_MetaVar_2E_for_4 (void);
static void mb_MetaVar_2E_for_3 (void);
static void mb_Data_2E_pred_1 (void);
static void mb_Data_2E_pred_2 (void);
static void mb_Data_2E_for_2 (void);
static void mb_Data_2E_for_4 (void);
static void mb_Data_2E_for_3 (void);
static void mb_Tag_2E_pred_1 (void);
static void mb_Tag_2E_pred_2 (void);
static void mb_Tag_2E_for_2 (void);
static void mb_Tag_2E_for_4 (void);
static void mb_Tag_2E_for_3 (void);
static void mb_Atom_2E_pred_1 (void);
static void mb_Atom_2E_pred_2 (void);
static void mb_Atom_2E_for_2 (void);
static void mb_Atom_2E_for_4 (void);
static void mb_Atom_2E_for_3 (void);
static void mb_Arrow_2E_pred_1 (void);
static void mb_Arrow_2E_pred_2 (void);
static void mb_Arrow_2E_for_2 (void);
static void mb_Arrow_2E_for_4 (void);
static void mb_Arrow_2E_for_3 (void);
static void mb_Lambda_2E_pred_1 (void);
static void mb_Lambda_2E_pred_2 (void);
static void mb_Lambda_2E_for_2 (void);
static void mb_Lambda_2E_for_4 (void);
static void mb_Lambda_2E_for_3 (void);
static void mb_Block_2E_pred_1 (void);
static void mb_Block_2E_pred_2 (void);
static void mb_Block_2E_for_2 (void);
static void mb_Block_2E_for_4 (void);
static void mb_Block_2E_for_3 (void);
static void mb_Match_2E_pred_1 (void);
static void mb_Match_2E_pred_2 (void);
static void mb_Match_2E_for_2 (void);
static void mb_Match_2E_for_4 (void);
static void mb_Match_2E_for_3 (void);
static void mb_Case_2E_pred_1 (void);
static void mb_Case_2E_pred_2 (void);
static void mb_Case_2E_for_2 (void);
static void mb_Case_2E_for_4 (void);
static void mb_Case_2E_for_3 (void);
static void mb_Var_2E_pred_1 (void);
static void mb_Var_2E_pred_2 (void);
static void mb_Var_2E_for_2 (void);
static void mb_Var_2E_for_4 (void);
static void mb_Var_2E_for_3 (void);
static void mb_Word_2E_pred_1 (void);
static void mb_Word_2E_pred_2 (void);
static void mb_Word_2E_for_2 (void);
static void mb_Word_2E_for_4 (void);
static void mb_Word_2E_for_3 (void);
static void mb_Table_2E_pred_1 (void);
static void mb_Table_2E_pred_2 (void);
static void mb_Table_2E_for_2 (void);
static void mb_Table_2E_for_4 (void);
static void mb_Table_2E_for_3 (void);
static void mb_Field_2E_pred_1 (void);
static void mb_Field_2E_pred_2 (void);
static void mb_Field_2E_for_2 (void);
static void mb_Field_2E_for_4 (void);
static void mb_Field_2E_for_3 (void);
static void mb_External_2E_pred_1 (void);
static void mb_External_2E_pred_2 (void);
static void mb_External_2E_for_2 (void);
static void mb_External_2E_for_4 (void);
static void mb_External_2E_for_3 (void);
static void mb_Variable_2E_pred_1 (void);
static void mb_Variable_2E_pred_2 (void);
static void mb_Variable_2E_for_2 (void);
static void mb_Variable_2E_for_4 (void);
static void mb_Variable_2E_for_3 (void);
static void mb_init_prims_21__1 (void);
static void mb_init_prims_21__2 (void);
static void mb_init_prims_21__3 (void);
static void mb_init_prims_21__4 (void);
static void mb_init_prims_21__5 (void);
static void mb_init_prims_21__6 (void);
static void mb_init_prims_21__7 (void);
static void mb_init_prims_21__8 (void);
static void mb_init_prims_21__9 (void);
static void mb_init_prims_21__10 (void);
static void mb_init_prims_21__11 (void);
static void mb_init_prims_21__12 (void);
static void mb_init_prims_21__13 (void);
static void mb_init_prims_21__14 (void);
static void mb_init_prims_21__15 (void);
static void mb_init_prims_21__16 (void);
static void mb_init_prims_21__17 (void);
static void mb_init_prims_21__18 (void);
static void mb_init_prims_21__19 (void);
static void mb_init_prims_21__20 (void);
static void mb_init_prims_21__21 (void);
static void mb_init_prims_21__22 (void);
static void mb_init_prims_21__23 (void);
static void mb_init_prims_21__24 (void);
static void mb_init_prims_21__25 (void);
static void mb_init_prims_21__26 (void);
static void mb_init_prims_21__27 (void);
static void mb_init_prims_21__28 (void);
static void mb_init_prims_21__29 (void);
static void mb_init_prims_21__30 (void);
static void mb_init_prims_21__31 (void);
static void mb_init_prims_21__32 (void);
static void mb_init_prims_21__33 (void);
static void mb_init_prims_21__34 (void);
static void mb_init_prims_21__35 (void);
static void mb_init_prims_21__36 (void);
static void mb_compile_21__1 (void);
static void mb_compile_21__2 (void);
static void mb_run_lexer_21__1 (void);
static void mb_run_lexer_21__2 (void);
static void mb_elab_module_21__1 (void);
static void mb_elab_module_21__2 (void);
static void mb_typecheck_everything_21__1 (void);
static void mb_typecheck_everything_21__2 (void);
static void mb_main_1 (void);
static void mb_main_2 (void);
static void mb_ptr_40__40__1 (void);
static void mb_ptr_40__40__2 (void);
static void mb_with_ptr_2B__2 (void);
static void mb_with_raw_ptr_2 (void);
static void mb_Int__3E_OS_1 (void);
static void mb_Int__3E_OS_2 (void);
static void mb_Int__3E_OS_3 (void);
static void mb_Int__3E_OS_4 (void);
static void mb_Int__3E_OS_5 (void);
static void mb_Int__3E_OS_6 (void);
static void mb_posix_open_21__1 (void);
static void mb_dip2_2 (void);
static void mb_dip2_3 (void);
static void mb_rotr_1 (void);
static void mb_rotl_1 (void);
static void mb_over_1 (void);
static void mb_over2_1 (void);
static void mb_over3_1 (void);
static void mb_tuck_1 (void);
static void mb_nip_1 (void);
static void mb_dup3_1 (void);
static void mb_dup3_2 (void);
static void mb_dip_3F__2 (void);
static void mb_dip_27__2 (void);
static void mb_dip3_2 (void);
static void mb_dip3_3 (void);
static void mb_dip3_4 (void);
static void mb_sip_2 (void);
static void mb_sip2_2 (void);
static void mb_sip2_3 (void);
static void mb_rot4r_1 (void);
static void mb_rot4l_1 (void);
static void mb_or_2 (void);
static void mb_or_3 (void);
static void mb_and_2 (void);
static void mb_and_3 (void);
static void mb_repeat_2 (void);
static void mb_repeat_3 (void);
static void mb_repeat_4 (void);
static void mb_count_2 (void);
static void mb_count_3 (void);
static void mb_countdown_2 (void);
static void mb_countdown_3 (void);
static void mb_cmp_1 (void);
static void mb_cmp_2 (void);
static void mb_cmp_3 (void);
static void mb_cmp_4 (void);
static void mb_max_1 (void);
static void mb_max_2 (void);
static void mb_min_1 (void);
static void mb_min_2 (void);
static void mb_ptr_21__21__1 (void);
static void mb_ptr_21__21__2 (void);
static void mb_u8_40__40__1 (void);
static void mb_u8_21__21__1 (void);
static void mb_int_40__40__1 (void);
static void mb_int_40__40__2 (void);
static void mb_int_21__21__1 (void);
static void mb_int_21__21__2 (void);
static void mb_value_40__40__1 (void);
static void mb_value_40__40__2 (void);
static void mb_value_21__21__1 (void);
static void mb_value_21__21__2 (void);
static void mb_in_range_1 (void);
static void mb_in_range_2 (void);
static void mb_abs_1 (void);
static void mb_abs_2 (void);
static void mb_pack1_1 (void);
static void mb_pack2_1 (void);
static void mb_pack3_1 (void);
static void mb_pack4_1 (void);
static void mb_pack5_1 (void);
static void mb_unpack2_1 (void);
static void mb_unpack3_1 (void);
static void mb_unpack4_1 (void);
static void mb_unpack5_1 (void);
static void mb_modify_2 (void);
static void mb_file_21__1 (void);
static void mb_str_write_21__1 (void);
static void mb_str_write_21__2 (void);
static void mb_str_write_21__3 (void);
static void mb_str_write_21__4 (void);
static void mb_str_write_21__5 (void);
static void mb_str_write_21__6 (void);
static void mb_str_buf_write_21__1 (void);
static void mb_str_buf_write_21__2 (void);
static void mb_str_buf_write_21__3 (void);
static void mb_str_buf_write_21__4 (void);
static void mb_str_buf_read_21__1 (void);
static void mb_str_buf_read_21__2 (void);
static void mb_str_buf_length_21__1 (void);
static void mb_print_char_21__1 (void);
static void mb_build_str_21__2 (void);
static void mb_str_buf_push_char_21__1 (void);
static void mb_trace_char_21__1 (void);
static void mb_int_write_21__1 (void);
static void mb_str_buf_int_21__1 (void);
static void mb_str_buf_int_21__2 (void);
static void mb_str_buf_int_21__3 (void);
static void mb_str_buf_int_21__4 (void);
static void mb_str_buf_int_21__5 (void);
static void mb_str_buf_int_21__6 (void);
static void mb_str_buf_int_21__7 (void);
static void mb_with_open_file_21__2 (void);
static void mb_with_open_file_21__3 (void);
static void mb_with_open_file_21__4 (void);
static void mb_read_file_21__2 (void);
static void mb_read_file_21__3 (void);
static void mb_read_file_21__4 (void);
static void mb_read_file_21__5 (void);
static void mb_read_file_21__6 (void);
static void mb_read_file_21__7 (void);
static void mb_read_file_21__8 (void);
static void mb_read_file_21__9 (void);
static void mb_open_file_21__1 (void);
static void mb_open_file_21__2 (void);
static void mb_create_file_21__1 (void);
static void mb_create_file_21__2 (void);
static void mb_close_file_21__1 (void);
static void mb_close_file_21__2 (void);
static void mb_with_raw_path_2 (void);
static void mb_is_directory_3F__1 (void);
static void mb_is_directory_3F__2 (void);
static void mb_is_directory_3F__3 (void);
static void mb_is_directory_3F__4 (void);
static void mb_st_mode_40__4 (void);
static void mb_char_40__1 (void);
static void mb_char_40__2 (void);
static void mb_str_tail_1 (void);
static void mb_str_length_1 (void);
static void mb_str_for_2 (void);
static void mb_str_for_3 (void);
static void mb_str_for_4 (void);
static void mb_str_for_5 (void);
static void mb_str_concat_1 (void);
static void mb_str_concat_2 (void);
static void mb_str_concat_3 (void);
static void mb_for_5 (void);
static void mb_for_7 (void);
static void mb_for_8 (void);
static void mb_for_10 (void);
static void mb_for_11 (void);
static void mb_for_12 (void);
static void mb_str_copy_partial_21__1 (void);
static void mb_str_copy_partial_21__2 (void);
static void mb_str_is_empty_1 (void);
static void mb_str_is_empty_2 (void);
static void mb_str_buf_dup_21__1 (void);
static void mb_str_buf_char_40__1 (void);
static void mb_char_21__1 (void);
static void mb_char_width_1 (void);
static void mb_str_buf_push_ptr_21__1 (void);
static void mb_str_buf_reverse_21__1 (void);
static void mb_str_buf_reverse_21__2 (void);
static void mb_str_buf_reverse_21__3 (void);
static void mb_int_show_1 (void);
static void mb_str_buf_swap_u8_21__1 (void);
static void mb_str_buf_swap_u8_21__2 (void);
static void mb_str_transduce_2 (void);
static void mb_str_transduce_3 (void);
static void mb_str_transduce_4 (void);
static void mb_str_transduce_5 (void);
static void mb_str_transduce_6 (void);
static void mb_str_transduce_step_5 (void);
static void mb_str_chars_1 (void);
static void mb_str_chars_2 (void);
static void mb_str_codepoints_1 (void);
static void mb_str_codepoints_2 (void);
static void mb_char_codepoint_1 (void);
static void mb_char_codepoint_2 (void);
static void mb_char_codepoint_3 (void);
static void mb_char_codepoint_4 (void);
static void mb_char_codepoint_5 (void);
static void mb_char_codepoint_6 (void);
static void mb_str_bytes_1 (void);
static void mb_str_bytes_2 (void);
static void mb_str_bytes_3 (void);
static void mb_str_bytes_4 (void);
static void mb_str_bytes_5 (void);
static void mb_L4_2B__1 (void);
static void mb_L5_2B__1 (void);
static void mb_L6_2B__1 (void);
static void mb_L7_2B__1 (void);
static void mb_L8_2B__1 (void);
static void mb_L9_2B__1 (void);
static void mb_L10_2B__1 (void);
static void mb_L11_2B__1 (void);
static void mb_L12_2B__1 (void);
static void mb_len_6 (void);
static void mb_len_2B__5 (void);
static void mb_cons_2B__6 (void);
static void mb_cons_2B__7 (void);
static void mb_rebalance_2B__1 (void);
static void mb_rebalance_2B__2 (void);
static void mb_rebalance_2B__4 (void);
static void mb_rebalance_2B__3 (void);
static void mb_rebalance_2B__5 (void);
static void mb_rebalance_2B__6 (void);
static void mb_rebalance_2B__8 (void);
static void mb_rebalance_2B__7 (void);
static void mb_snoc_2B__6 (void);
static void mb_snoc_2B__2B__1 (void);
static void mb_uncons_5 (void);
static void mb_unsnoc_2 (void);
static void mb_unsnoc_4 (void);
static void mb_unsnoc_6 (void);
static void mb_unsnoc_8 (void);
static void mb_cat_2B__7 (void);
static void mb_cat_2B__13 (void);
static void mb_cat_aux_1 (void);
static void mb_split_half_left_2 (void);
static void mb_split_half_left_4 (void);
static void mb_split_half_left_6 (void);
static void mb_split_half_right_2 (void);
static void mb_split_half_right_4 (void);
static void mb_split_half_right_6 (void);
static void mb_split_half_right_8 (void);
static void mb_split_half_3 (void);
static void mb_split_half_5 (void);
static void mb_split_half_7 (void);
static void mb_split_half_9 (void);
static void mb_first_1 (void);
static void mb_last_1 (void);
static void mb_last_2B__3 (void);
static void mb_last_2B__5 (void);
static void mb_middle_1 (void);
static void mb_reverse_6 (void);
static void mb_reverse_2B__5 (void);
static void mb_map_5 (void);
static void mb_map_6 (void);
static void mb_map_8 (void);
static void mb_map_9 (void);
static void mb_map_10 (void);
static void mb_map_11 (void);
static void mb_map_12 (void);
static void mb_map_13 (void);
static void mb_map_15 (void);
static void mb_map_16 (void);
static void mb_map_17 (void);
static void mb_map_18 (void);
static void mb_map_19 (void);
static void mb_map_2B__4 (void);
static void mb_map_2B__5 (void);
static void mb_map_2B__7 (void);
static void mb_map_2B__8 (void);
static void mb_map_2B__9 (void);
static void mb_map_2B__10 (void);
static void mb_map_2B__11 (void);
static void mb_map_2B__12 (void);
static void mb_map_2B__14 (void);
static void mb_map_2B__15 (void);
static void mb_map_2B__16 (void);
static void mb_map_2B__17 (void);
static void mb_map_2B__18 (void);
static void mb_for_2B__4 (void);
static void mb_for_2B__6 (void);
static void mb_for_2B__7 (void);
static void mb_for_2B__9 (void);
static void mb_for_2B__10 (void);
static void mb_for_2B__11 (void);
static void mb_reverse_for_5 (void);
static void mb_reverse_for_7 (void);
static void mb_reverse_for_8 (void);
static void mb_reverse_for_10 (void);
static void mb_reverse_for_11 (void);
static void mb_reverse_for_12 (void);
static void mb_reverse_for_2B__4 (void);
static void mb_reverse_for_2B__6 (void);
static void mb_reverse_for_2B__7 (void);
static void mb_reverse_for_2B__9 (void);
static void mb_reverse_for_2B__10 (void);
static void mb_reverse_for_2B__11 (void);
static void mb_reduce_2 (void);
static void mb_reduce_3 (void);
static void mb_reduce_2B__6 (void);
static void mb_reduce_2B__7 (void);
static void mb_reduce_2B__8 (void);
static void mb_filter_4 (void);
static void mb_filter_2B__3 (void);
static void mb_filter_2B__4 (void);
static void mb_filter_2B__5 (void);
static void mb_filter_2B__6 (void);
static void mb_filter_2B__8 (void);
static void mb_filter_2B__9 (void);
static void mb_filter_2B__12 (void);
static void mb_filter_2B__10 (void);
static void mb_filter_2B__11 (void);
static void mb_filter_2B__13 (void);
static void mb_find_4 (void);
static void mb_find_2B__3 (void);
static void mb_find_2B__4 (void);
static void mb_find_2B__7 (void);
static void mb_find_2B__9 (void);
static void mb_find_2B__10 (void);
static void mb_find_2B__11 (void);
static void mb_find_2B__12 (void);
static void mb_find_3F__2 (void);
static void mb_find_3F__3 (void);
static void mb_reverse_find_2 (void);
static void mb_reverse_find_3F__2 (void);
static void mb_reverse_find_3F__3 (void);
static void mb_any_2 (void);
static void mb_any_3F__2 (void);
static void mb_all_2 (void);
static void mb_all_3F__2 (void);
static void mb_collect_2 (void);
static void mb_collect_3 (void);
static void mb_collect_4 (void);
static void mb_while_some_2 (void);
static void mb_while_some_3 (void);
static void mb_collect_while_2 (void);
static void mb_collect_while_4 (void);
static void mb_collect_while_3 (void);
static void mb_collect_while_5 (void);
static void mb_maybe_filter_4 (void);
static void mb_maybe_filter_5 (void);
static void mb_char_bytes_1 (void);
static void mb_char_bytes_2 (void);
static void mb_char_bytes_3 (void);
static void mb_char_bytes_5 (void);
static void mb_char_bytes_4 (void);
static void mb_char_bytes_6 (void);
static void mb_char_bytes_9 (void);
static void mb_char_bytes_7 (void);
static void mb_char_bytes_8 (void);
static void mb_char_bytes_10 (void);
static void mb_char_bytes_11 (void);
static void mb_char_bytes_12 (void);
static void mb_char_valid_wobbly_1 (void);
static void mb_char_valid_wobbly_2 (void);
static void mb_char_valid_wobbly_3 (void);
static void mb_char_valid_wobbly_4 (void);
static void mb_char_valid_wobbly_5 (void);
static void mb_char_valid_wobbly_6 (void);
static void mb_char_codepoint_3_1 (void);
static void mb_char_codepoint_3_2 (void);
static void mb_char_valid_1 (void);
static void mb_char_valid_2 (void);
static void mb_char_valid_3 (void);
static void mb_char_valid_4 (void);
static void mb_char_valid_5 (void);
static void mb_char_valid_6 (void);
static void mb_char_codepoint_4_1 (void);
static void mb_char_codepoint_4_2 (void);
static void mb_char_codepoint_4_3 (void);
static void mb_char_codepoint_2_1 (void);
static void mb_char_21__precise_1 (void);
static void mb_char_21__precise_2 (void);
static void mb_char_21__precise_4 (void);
static void mb_char_21__precise_3 (void);
static void mb_char_21__precise_5 (void);
static void mb_char_21__precise_7 (void);
static void mb_char_21__precise_6 (void);
static void mb_char_21__precise_8 (void);
static void mb_char_21__precise_12 (void);
static void mb_char_21__precise_9 (void);
static void mb_char_21__precise_10 (void);
static void mb_char_21__precise_11 (void);
static void mb_char_21__precise_13 (void);
static void mb_char_21__2B__2B__1 (void);
static void mb_char_3F__2B__2B__1 (void);
static void mb_is_whitespace_3F__1 (void);
static void mb_is_whitespace_3F__2 (void);
static void mb_is_hexdigit_3F__1 (void);
static void mb_is_hexdigit_3F__2 (void);
static void mb_is_sign_3F__1 (void);
static void mb_is_string_end_3F__1 (void);
static void mb_is_name_char_3F__1 (void);
static void mb_is_special_char_3F__1 (void);
static void mb_is_special_char_3F__2 (void);
static void mb_is_special_char_3F__3 (void);
static void mb_is_special_char_3F__4 (void);
static void mb_path_21__1 (void);
static void mb_path_separator_1 (void);
static void mb_path_separator_2 (void);
static void mb_path_join_1 (void);
static void mb_path_join_2 (void);
static void mb_input_fill_buffer_21__1 (void);
static void mb_input_fill_buffer_21__6 (void);
static void mb_input_fill_buffer_21__2 (void);
static void mb_input_fill_buffer_21__5 (void);
static void mb_input_fill_buffer_21__3 (void);
static void mb_input_fill_buffer_21__4 (void);
static void mb_input_end_21__1 (void);
static void mb_input_end_21__2 (void);
static void mb_input_peek_1 (void);
static void mb_input_peek_3 (void);
static void mb_input_peek_2 (void);
static void mb_input_move_21__1 (void);
static void mb_input_move_21__3 (void);
static void mb_input_move_21__2 (void);
static void mb_input_prepare_for_more_21__1 (void);
static void mb_input_prepare_for_more_21__7 (void);
static void mb_input_prepare_for_more_21__2 (void);
static void mb_input_prepare_for_more_21__3 (void);
static void mb_input_prepare_for_more_21__4 (void);
static void mb_input_prepare_for_more_21__5 (void);
static void mb_input_prepare_for_more_21__6 (void);
static void mb_input_fill_buffer_tragic_21__1 (void);
static void mb_input_fill_buffer_tragic_21__2 (void);
static void mb_input_fill_buffer_tragic_21__9 (void);
static void mb_input_fill_buffer_tragic_21__3 (void);
static void mb_input_fill_buffer_tragic_21__4 (void);
static void mb_input_fill_buffer_tragic_21__8 (void);
static void mb_input_fill_buffer_tragic_21__5 (void);
static void mb_input_fill_buffer_tragic_21__7 (void);
static void mb_input_fill_buffer_tragic_21__6 (void);
static void mb_module_source_path_1 (void);
static void mb_module_source_path_2 (void);
static void mb_lexer_next_21__1 (void);
static void mb_lexer_next_21__2 (void);
static void mb_lexer_next_21__3 (void);
static void mb_lexer_next_21__4 (void);
static void mb_lexer_next_21__5 (void);
static void mb_lexer_next_21__6 (void);
static void mb_lexer_next_21__7 (void);
static void mb_lexer_next_21__8 (void);
static void mb_lexer_next_21__9 (void);
static void mb_lexer_next_21__10 (void);
static void mb_lexer_next_21__11 (void);
static void mb_lexer_next_21__12 (void);
static void mb_lexer_next_21__13 (void);
static void mb_lexer_next_21__14 (void);
static void mb_lexer_next_21__15 (void);
static void mb_lexer_next_21__16 (void);
static void mb_lexer_next_21__17 (void);
static void mb_lexer_next_21__18 (void);
static void mb_lexer_next_21__19 (void);
static void mb_lexer_next_21__20 (void);
static void mb_lexer_next_21__21 (void);
static void mb_lexer_next_21__22 (void);
static void mb_lexer_next_21__23 (void);
static void mb_lexer_next_21__24 (void);
static void mb_lexer_next_21__25 (void);
static void mb_lexer_next_21__26 (void);
static void mb_stack_pop_21__1 (void);
static void mb_emit_fatal_error_21__1 (void);
static void mb_lexer_emit_fatal_error_21__1 (void);
static void mb_lexer_emit_name_21__1 (void);
static void mb_lexer_emit_name_21__2 (void);
static void mb_lexer_emit_name_21__3 (void);
static void mb_lexer_emit_name_21__4 (void);
static void mb_lexer_emit_name_21__5 (void);
static void mb_lexer_emit_name_21__6 (void);
static void mb_lexer_emit_name_21__7 (void);
static void mb_lexer_emit_name_21__8 (void);
static void mb_lexer_newline_21__1 (void);
static void mb_lexer_skip_comment_21__1 (void);
static void mb_lexer_skip_comment_21__2 (void);
static void mb_lexer_skip_comment_21__3 (void);
static void mb_lexer_skip_comment_21__4 (void);
static void mb_lexer_emit_rparen_21__3 (void);
static void mb_lexer_emit_rparen_21__4 (void);
static void mb_lexer_emit_rsquare_21__3 (void);
static void mb_lexer_emit_rsquare_21__4 (void);
static void mb_lexer_emit_rcurly_21__3 (void);
static void mb_lexer_emit_rcurly_21__4 (void);
static void mb_lexer_emit_string_21__1 (void);
static void mb_lexer_emit_string_21__2 (void);
static void mb_lexer_emit_string_21__3 (void);
static void mb_lexer_emit_string_21__4 (void);
static void mb_lexer_move_21__1 (void);
static void mb_stack_push_21__1 (void);
static void mb_str_buf_is_doc_start_3F__1 (void);
static void mb_str_buf_is_doc_start_3F__2 (void);
static void mb_str_buf_is_doc_start_3F__3 (void);
static void mb_lexer_skip_doc_21__1 (void);
static void mb_lexer_skip_doc_21__2 (void);
static void mb_str_buf_is_int_3F__1 (void);
static void mb_str_buf_is_int_3F__2 (void);
static void mb_str_buf_int_3F__1 (void);
static void mb_str_buf_int_3F__2 (void);
static void mb_name_new_21__1 (void);
static void mb_name_new_21__2 (void);
static void mb_name_new_21__3 (void);
static void mb_name_new_21__5 (void);
static void mb_name_new_21__4 (void);
static void mb_str_buf_is_arrow_3F__1 (void);
static void mb_str_buf_is_dashes_3F__1 (void);
static void mb_str_buf_is_dashes_3F__2 (void);
static void mb_str_buf_is_equal_3F__1 (void);
static void mb_str_buf_is_equal_3F__2 (void);
static void mb_str_buf_is_dec_int_3F__1 (void);
static void mb_str_buf_is_dec_int_3F__2 (void);
static void mb_str_buf_is_dec_int_3F__3 (void);
static void mb_str_buf_is_dec_int_3F__4 (void);
static void mb_str_buf_is_dec_int_3F__5 (void);
static void mb_str_buf_is_dec_int_3F__6 (void);
static void mb_str_buf_is_dec_int_3F__7 (void);
static void mb_str_buf_is_hex_int_3F__1 (void);
static void mb_str_buf_is_hex_int_3F__2 (void);
static void mb_str_buf_is_hex_int_3F__3 (void);
static void mb_str_buf_is_hex_int_3F__11 (void);
static void mb_str_buf_is_hex_int_3F__4 (void);
static void mb_str_buf_is_hex_int_3F__10 (void);
static void mb_str_buf_is_hex_int_3F__5 (void);
static void mb_str_buf_is_hex_int_3F__6 (void);
static void mb_str_buf_is_hex_int_3F__7 (void);
static void mb_str_buf_is_hex_int_3F__8 (void);
static void mb_str_buf_is_hex_int_3F__9 (void);
static void mb_is_xX_char_1 (void);
static void mb_str_buf_dec_int_3F__1 (void);
static void mb_str_buf_dec_int_3F__5 (void);
static void mb_str_buf_dec_int_3F__2 (void);
static void mb_str_buf_dec_int_3F__4 (void);
static void mb_str_buf_dec_int_3F__3 (void);
static void mb_str_buf_dec_int_3F__6 (void);
static void mb_str_buf_dec_int_3F__7 (void);
static void mb_str_buf_dec_int_3F__8 (void);
static void mb_str_buf_dec_int_3F__9 (void);
static void mb_str_buf_hex_int_3F__1 (void);
static void mb_str_buf_hex_int_3F__5 (void);
static void mb_str_buf_hex_int_3F__2 (void);
static void mb_str_buf_hex_int_3F__4 (void);
static void mb_str_buf_hex_int_3F__3 (void);
static void mb_str_buf_hex_int_3F__6 (void);
static void mb_str_buf_hex_int_3F__7 (void);
static void mb_str_buf_hex_int_3F__8 (void);
static void mb_str_buf_hex_int_3F__9 (void);
static void mb_hexdigit_value_1 (void);
static void mb_hexdigit_value_2 (void);
static void mb_hexdigit_value_3 (void);
static void mb_hexdigit_value_4 (void);
static void mb_lexer_push_string_char_21__1 (void);
static void mb_lexer_push_string_char_21__14 (void);
static void mb_lexer_push_string_char_21__2 (void);
static void mb_lexer_push_string_char_21__3 (void);
static void mb_lexer_push_string_char_21__4 (void);
static void mb_lexer_push_string_char_21__5 (void);
static void mb_lexer_push_string_char_21__6 (void);
static void mb_lexer_push_string_char_21__7 (void);
static void mb_lexer_push_string_char_21__8 (void);
static void mb_lexer_push_string_char_21__9 (void);
static void mb_lexer_push_string_char_21__10 (void);
static void mb_lexer_push_string_char_21__11 (void);
static void mb_lexer_push_string_char_21__12 (void);
static void mb_lexer_push_string_char_21__13 (void);
static void mb_lexer_emit_warning_21__1 (void);
static void mb_lexer_comment_end_3F__1 (void);
static void mb_lexer_comment_end_3F__2 (void);
static void mb_emit_warning_at_21__1 (void);
static void mb_emit_warning_at_21__2 (void);
static void mb_lexer_emit_error_21__1 (void);
static void mb_emit_error_at_21__1 (void);
static void mb_emit_error_at_21__2 (void);
static void mb_stack_uncons_3 (void);
static void mb_hash_1 (void);
static void mb_name_keep_going_3F__1 (void);
static void mb_name_keep_going_3F__2 (void);
static void mb_name_keep_going_3F__3 (void);
static void mb_name_mangle_compute_21__1 (void);
static void mb_name_mangle_compute_21__2 (void);
static void mb_name_mangle_compute_21__3 (void);
static void mb_name_mangle_compute_21__4 (void);
static void mb_name_mangle_compute_21__5 (void);
static void mb_name_mangle_compute_21__6 (void);
static void mb_name_mangle_compute_21__7 (void);
static void mb_name_mangle_compute_21__8 (void);
static void mb_name_mangle_compute_21__9 (void);
static void mb_name_cat_21__1 (void);
static void mb_name_is_type_hole_1 (void);
static void mb_name_is_type_hole_4 (void);
static void mb_name_is_type_hole_2 (void);
static void mb_name_is_type_hole_3 (void);
static void mb_name_is_underscore_1 (void);
static void mb_name_is_underscore_2 (void);
static void mb_name_could_be_stack_var_1 (void);
static void mb_name_could_be_stack_var_2 (void);
static void mb_name_could_be_effect_con_1 (void);
static void mb_name_could_be_effect_con_2 (void);
static void mb_force_21__3 (void);
static void mb_char_hexdigits_1 (void);
static void mb_char_hexdigits_2 (void);
static void mb_char_hexdigits_first_1 (void);
static void mb_char_hexdigits_next_1 (void);
static void mb_char_hexdigits_next_2 (void);
static void mb_char_hexdigits_next_3 (void);
static void mb_char_hexdigits_next_4 (void);
static void mb_hexdigit_1 (void);
static void mb_hexdigit_2 (void);
static void mb_token_prim_3D__3F__1 (void);
static void mb_token_prev_4 (void);
static void mb_token_prev_5 (void);
static void mb_token_next_arg_end_1 (void);
static void mb_token_next_arg_end_2 (void);
static void mb_token_has_args_3F__1 (void);
static void mb_token_has_args_3F__2 (void);
static void mb_token_num_args_1 (void);
static void mb_token_num_args_4 (void);
static void mb_token_num_args_2 (void);
static void mb_token_num_args_3 (void);
static void mb_token_num_args_5 (void);
static void mb_token_num_args_10 (void);
static void mb_token_num_args_6 (void);
static void mb_token_num_args_7 (void);
static void mb_token_num_args_8 (void);
static void mb_token_num_args_9 (void);
static void mb_token_args_0_1 (void);
static void mb_token_args_0_2 (void);
static void mb_token_args_1_1 (void);
static void mb_token_args_1_4 (void);
static void mb_token_args_1_2 (void);
static void mb_token_args_1_3 (void);
static void mb_token_args_1_5 (void);
static void mb_token_args_1_6 (void);
static void mb_token_args_2_1 (void);
static void mb_token_args_2_4 (void);
static void mb_token_args_2_2 (void);
static void mb_token_args_2_3 (void);
static void mb_token_args_2_5 (void);
static void mb_token_args_2_6 (void);
static void mb_token_args_3_1 (void);
static void mb_token_args_3_4 (void);
static void mb_token_args_3_2 (void);
static void mb_token_args_3_3 (void);
static void mb_token_args_3_5 (void);
static void mb_token_args_3_6 (void);
static void mb_token_args_1 (void);
static void mb_token_args_7 (void);
static void mb_token_args_2 (void);
static void mb_token_args_3 (void);
static void mb_token_args_4 (void);
static void mb_token_args_5 (void);
static void mb_token_args_6 (void);
static void mb_token_is_args_end_3F__1 (void);
static void mb_token_is_args_end_3F__2 (void);
static void mb_token_args_2_2B__1 (void);
static void mb_token_args_2_2B__2 (void);
static void mb_emit_warning_21__1 (void);
static void mb_emit_error_21__1 (void);
static void mb_token_run_1 (void);
static void mb_token_run_2 (void);
static void mb_token_run_3 (void);
static void mb_token_run_has_arrow_1 (void);
static void mb_token_run_has_dashes_1 (void);
static void mb_sig_is_stack_end_3F__1 (void);
static void mb_sig_is_stack_end_3F__2 (void);
static void mb_sig_is_stack_end2_3F__1 (void);
static void mb_sig_is_stack_end2_3F__2 (void);
static void mb_sig_next_stack_end_1 (void);
static void mb_sig_next_stack_end_2 (void);
static void mb_sig_arity_1 (void);
static void mb_sig_arity_2 (void);
static void mb_sig_count_types_1 (void);
static void mb_sig_count_types_2 (void);
static void mb_sig_count_types_3 (void);
static void mb_sig_count_types_5 (void);
static void mb_sig_count_types_4 (void);
static void mb_sig_skip_dashes_1 (void);
static void mb_sig_skip_dashes_2 (void);
static void mb_module_add_import_21__1 (void);
static void mb_module_path_from_name_1 (void);
static void mb_module_path_from_name_2 (void);
static void mb_module_path_from_name_3 (void);
static void mb_module_path_from_name_4 (void);
static void mb_module_path_from_name_5 (void);
static void mb_set_snoc_1 (void);
static void mb_bag_replace_1 (void);
static void mb_bag_replace_2 (void);
static void mb_codegen_flush_21__1 (void);
static void mb_codegen_flush_21__6 (void);
static void mb_codegen_flush_21__2 (void);
static void mb_codegen_flush_21__3 (void);
static void mb_codegen_flush_21__4 (void);
static void mb_codegen_flush_21__5 (void);
static void mb__2E_b_1 (void);
static void mb__2E_b_2 (void);
static void mb__2E_b_3 (void);
static void mb__2E_c_1 (void);
static void mb__2E_c_2 (void);
static void mb__2E_c_3 (void);
static void mb__2E_c_4 (void);
static void mb__2E__1 (void);
static void mb__2E__5 (void);
static void mb__2E__2 (void);
static void mb__2E__3 (void);
static void mb__2E__4 (void);
static void mb__2E__6 (void);
static void mb__2E__7 (void);
static void mb_run_output_c99_21__1 (void);
static void mb_run_output_c99_21__2 (void);
static void mb_c99_tags_21__1 (void);
static void mb_c99_buffers_21__1 (void);
static void mb_c99_variables_21__1 (void);
static void mb_c99_externals_21__1 (void);
static void mb_c99_word_sigs_21__1 (void);
static void mb_c99_block_sigs_21__1 (void);
static void mb_c99_field_sigs_21__1 (void);
static void mb_c99_main_21__1 (void);
static void mb_c99_word_defs_21__1 (void);
static void mb_c99_block_defs_21__1 (void);
static void mb_c99_field_defs_21__1 (void);
static void mb_c99_tag_21__1 (void);
static void mb_c99_tag_21__2 (void);
static void mb_c99_tag_21__3 (void);
static void mb_c99_tag_21__4 (void);
static void mb_c99_tag_21__5 (void);
static void mb_tag_num_inputs_3F__3 (void);
static void mb_tag_num_inputs_3F__4 (void);
static void mb_tag_num_inputs_3F__5 (void);
static void mb_c99_external_21__1 (void);
static void mb_c99_external_21__2 (void);
static void mb_c99_external_21__3 (void);
static void mb_c99_external_21__4 (void);
static void mb_c99_external_21__5 (void);
static void mb_c99_external_21__6 (void);
static void mb_c99_external_21__8 (void);
static void mb_c99_external_21__7 (void);
static void mb_c99_external_21__9 (void);
static void mb_c99_external_21__10 (void);
static void mb_c99_external_21__11 (void);
static void mb_c99_external_21__12 (void);
static void mb_c99_external_21__13 (void);
static void mb_c99_external_21__14 (void);
static void mb_c99_external_21__15 (void);
static void mb_c99_external_21__17 (void);
static void mb_c99_external_21__16 (void);
static void mb_c99_external_21__18 (void);
static void mb_c99_external_21__19 (void);
static void mb_c99_nest_2 (void);
static void mb_c99_nest_3 (void);
static void mb_c99_indent_1 (void);
static void mb_c99_call_21__1 (void);
static void mb_c99_call_21__2 (void);
static void mb_c99_args_push_21__1 (void);
static void mb_c99_arrow_21__1 (void);
static void mb_c99_atom_21__1 (void);
static void mb_c99_int_21__1 (void);
static void mb_c99_str_21__1 (void);
static void mb_c99_str_21__14 (void);
static void mb_c99_str_21__2 (void);
static void mb_c99_str_21__3 (void);
static void mb_c99_str_21__4 (void);
static void mb_c99_str_21__5 (void);
static void mb_c99_str_21__6 (void);
static void mb_c99_str_21__7 (void);
static void mb_c99_str_21__8 (void);
static void mb_c99_str_21__9 (void);
static void mb_c99_str_21__10 (void);
static void mb_c99_str_21__11 (void);
static void mb_c99_str_21__12 (void);
static void mb_c99_str_21__13 (void);
static void mb_c99_str_21__15 (void);
static void mb_c99_str_21__16 (void);
static void mb_c99_prim_21__3 (void);
static void mb_c99_prim_21__4 (void);
static void mb_c99_prim_21__5 (void);
static void mb_c99_prim_21__6 (void);
static void mb_c99_prim_21__7 (void);
static void mb_c99_prim_21__11 (void);
static void mb_c99_prim_21__12 (void);
static void mb_c99_prim_21__13 (void);
static void mb_c99_prim_21__14 (void);
static void mb_c99_prim_21__15 (void);
static void mb_c99_prim_21__19 (void);
static void mb_c99_prim_21__20 (void);
static void mb_c99_prim_21__21 (void);
static void mb_c99_prim_21__22 (void);
static void mb_c99_match_21__1 (void);
static void mb_c99_match_21__4 (void);
static void mb_c99_match_21__5 (void);
static void mb_c99_match_21__6 (void);
static void mb_c99_match_21__7 (void);
static void mb_c99_match_21__8 (void);
static void mb_c99_match_21__9 (void);
static void mb_c99_match_21__10 (void);
static void mb_c99_match_21__11 (void);
static void mb_c99_lambda_21__1 (void);
static void mb_c99_lambda_21__2 (void);
static void mb_c99_lambda_21__3 (void);
static void mb_c99_lambda_21__4 (void);
static void mb_c99_lambda_21__5 (void);
static void mb_c99_lambda_21__6 (void);
static void mb_c99_lambda_21__7 (void);
static void mb_c99_var_21__1 (void);
static void mb_c99_var_21__2 (void);
static void mb_c99_var_21__4 (void);
static void mb_c99_var_21__3 (void);
static void mb_c99_block_push_21__1 (void);
static void mb_c99_block_push_21__2 (void);
static void mb_c99_string_char_21__1 (void);
static void mb_c99_string_char_21__2 (void);
static void mb_c99_string_char_21__3 (void);
static void mb_c99_string_char_21__4 (void);
static void mb_c99_string_char_21__5 (void);
static void mb_c99_string_char_21__6 (void);
static void mb_c99_string_char_21__7 (void);
static void mb_c99_string_char_21__8 (void);
static void mb_c99_string_char_21__9 (void);
static void mb_c99_string_char_21__10 (void);
static void mb_c99_string_char_21__11 (void);
static void mb_c99_string_char_21__12 (void);
static void mb_c99_string_char_21__13 (void);
static void mb_c99_var_push_21__1 (void);
static void mb_c99_var_push_21__2 (void);
static void mb_c99_pack_ctx_21__1 (void);
static void mb_c99_pack_ctx_21__2 (void);
static void mb_c99_pack_ctx_21__3 (void);
static void mb_ctx_physical_vars_1 (void);
static void mb_c99_unpack_ctx_21__1 (void);
static void mb_c99_unpack_ctx_21__2 (void);
static void mb_c99_unpack_ctx_21__3 (void);
static void mb_c99_unpack_ctx_21__4 (void);
static void mb_c99_decref_ctx_21__1 (void);
static void mb_c99_decref_ctx_21__2 (void);
static void mb__2E_block_1 (void);
static void mb__2E_block_2 (void);
static void mb_c99_case_21__1 (void);
static void mb_c99_case_21__2 (void);
static void mb_c99_pattern_21__2 (void);
static void mb_c99_pattern_21__4 (void);
static void mb_c99_pattern_21__5 (void);
static void mb_c99_pattern_21__6 (void);
static void mb_c99_pattern_21__10 (void);
static void mb_c99_pattern_21__7 (void);
static void mb_c99_pattern_21__8 (void);
static void mb_c99_pattern_21__9 (void);
static void mb_c99_pattern_21__11 (void);
static void mb_c99_word_sig_21__1 (void);
static void mb_c99_block_sig_21__1 (void);
static void mb_c99_field_sig_21__1 (void);
static void mb_c99_block_def_21__1 (void);
static void mb_c99_block_def_21__2 (void);
static void mb_c99_block_def_21__3 (void);
static void mb_c99_word_def_21__1 (void);
static void mb_atom_arg_add_left_21__1 (void);
static void mb_arrow_atom_add_21__1 (void);
static void mb_block_new_deferred_21__1 (void);
static void mb_elab_arrow_hom_21__1 (void);
static void mb_block_unify_type_21__1 (void);
static void mb_block_unify_type_21__2 (void);
static void mb_block_unify_type_21__3 (void);
static void mb_elab_expand_morphism_21__2 (void);
static void mb_elab_expand_morphism_21__5 (void);
static void mb_elab_expand_morphism_21__7 (void);
static void mb_type_unify_21__8 (void);
static void mb_type_unify_21__14 (void);
static void mb_type_unify_21__16 (void);
static void mb_type_unify_21__19 (void);
static void mb_type_unify_21__24 (void);
static void mb_type_unify_21__26 (void);
static void mb_type_unify_21__28 (void);
static void mb_type_unify_21__31 (void);
static void mb_type_unify_21__36 (void);
static void mb_type_unify_21__38 (void);
static void mb_type_unify_21__40 (void);
static void mb_type_unify_21__43 (void);
static void mb_type_unify_21__48 (void);
static void mb_type_unify_21__50 (void);
static void mb_type_unify_21__52 (void);
static void mb_type_unify_21__55 (void);
static void mb_type_unify_21__60 (void);
static void mb_type_unify_21__62 (void);
static void mb_type_unify_21__64 (void);
static void mb_type_unify_21__67 (void);
static void mb_type_unify_21__72 (void);
static void mb_type_unify_21__74 (void);
static void mb_type_unify_21__76 (void);
static void mb_type_unify_21__79 (void);
static void mb_type_unify_21__84 (void);
static void mb_type_unify_21__86 (void);
static void mb_type_unify_21__88 (void);
static void mb_type_unify_21__91 (void);
static void mb_type_unify_21__96 (void);
static void mb_type_unify_21__98 (void);
static void mb_match_add_case_21__1 (void);
static void mb_match_add_case_21__2 (void);
static void mb_match_add_case_21__3 (void);
static void mb_match_is_exhaustive_3F__1 (void);
static void mb_match_is_exhaustive_3F__2 (void);
static void mb_match_scrutinee_data_3F__1 (void);
static void mb_cases_have_default_case_1 (void);
static void mb_type_head_2 (void);
static void mb_type_head_3 (void);
static void mb_cases_cover_case_1 (void);
static void mb_case_is_covered_1 (void);
static void mb_pattern_is_covered_1 (void);
static void mb_pattern_is_covered_2 (void);
static void mb_def_type_21__1 (void);
static void mb_T1_1 (void);
static void mb_T2_1 (void);
static void mb_T3_1 (void);
static void mb_T4_1 (void);
static void mb_T5_1 (void);
static void mb_T6_1 (void);
static void mb_type_is_physical_2 (void);
static void mb_type_is_physical_3 (void);
static void mb_meta_expand_1 (void);
static void mb_meta_expand_2 (void);
static void mb_type_unify_failed_21__1 (void);
static void mb_type_unify_failed_21__2 (void);
static void mb_type_unify_failed_21__3 (void);
static void mb_type_hole_unify_21__1 (void);
static void mb_type_hole_unify_21__2 (void);
static void mb_meta_unify_21__3 (void);
static void mb_meta_unify_21__4 (void);
static void mb_meta_unify_21__5 (void);
static void mb_meta_unify_21__6 (void);
static void mb_type_var_unify_21__1 (void);
static void mb_type_var_unify_21__2 (void);
static void mb_type_var_unify_21__3 (void);
static void mb_type_prim_unify_21__1 (void);
static void mb_type_prim_unify_21__2 (void);
static void mb_type_prim_unify_21__3 (void);
static void mb_type_data_unify_21__1 (void);
static void mb_type_data_unify_21__2 (void);
static void mb_type_data_unify_21__3 (void);
static void mb_type_table_unify_21__1 (void);
static void mb_type_table_unify_21__2 (void);
static void mb_type_table_unify_21__3 (void);
static void mb_type_unify_pair_21__1 (void);
static void mb_type_unify_pair_21__2 (void);
static void mb_type_unify_pair_21__3 (void);
static void mb_value_unify_21__3 (void);
static void mb_value_unify_21__4 (void);
static void mb_value_unify_21__9 (void);
static void mb_value_unify_21__10 (void);
static void mb_value_unify_21__15 (void);
static void mb_value_unify_21__16 (void);
static void mb_type2_has_meta_1 (void);
static void mb_type2_has_meta_2 (void);
static void mb_value_type_has_meta_4 (void);
static void mb_type_trace_sig_21__2 (void);
static void mb_type_trace_sig_21__3 (void);
static void mb_type_trace_stack_dom_21__1 (void);
static void mb_type_trace_stack_dom_21__2 (void);
static void mb_type_trace_stack_cod_21__1 (void);
static void mb_type_trace_stack_cod_21__2 (void);
static void mb_type_trace_stack_21__2 (void);
static void mb_type_trace_stack_21__3 (void);
static void mb_type_trace_stack_21__6 (void);
static void mb_type_trace_stack_21__7 (void);
static void mb_value_as_type_4 (void);
static void mb_type_semifreshen_sig_1 (void);
static void mb_type_semifreshen_sig_2 (void);
static void mb_type_sig_needs_fresh_stack_rest_2 (void);
static void mb_type_sig_needs_fresh_stack_rest_3 (void);
static void mb_type_sig_needs_fresh_stack_rest_5 (void);
static void mb_type_sig_needs_fresh_stack_rest_6 (void);
static void mb_type_semifreshen_sig_aux_2 (void);
static void mb_type_semifreshen_sig_aux_3 (void);
static void mb_type_semifreshen_sig_aux_5 (void);
static void mb_type_semifreshen_sig_aux_6 (void);
static void mb_type_semifreshen_sig_stack_2 (void);
static void mb_type_semifreshen_sig_stack_3 (void);
static void mb_type_semifreshen_sig_stack_5 (void);
static void mb_type_freshen_sig_1 (void);
static void mb_type_freshen_sig_2 (void);
static void mb_type_freshen_sig_aux_2 (void);
static void mb_type_freshen_sig_aux_3 (void);
static void mb_type_freshen_sig_aux_5 (void);
static void mb_type_freshen_sig_aux_6 (void);
static void mb_type_freshen_sig_aux_7 (void);
static void mb_type_stack_rest_2 (void);
static void mb_type_stack_rest_3 (void);
static void mb_type_freshen_sig_stack_2 (void);
static void mb_type_freshen_sig_stack_3 (void);
static void mb_type_freshen_sig_stack_5 (void);
static void mb_type_freshen_sig_stack_6 (void);
static void mb_meta_freshen_1 (void);
static void mb_meta_freshen_2 (void);
static void mb_type_var_freshen_1 (void);
static void mb_type_var_freshen_2 (void);
static void mb_type_var_freshen_3 (void);
static void mb_type_pair_freshen_1 (void);
static void mb_type_pair_freshen_2 (void);
static void mb_type_rigidify_sig_21__2 (void);
static void mb_meta_expand_or_update_21__3 (void);
static void mb_type_rigidify_21__2 (void);
static void mb_type_rigidify_21__11 (void);
static void mb_type_rigidify_21__12 (void);
static void mb_type_rigidify_21__14 (void);
static void mb_type_rigidify_21__15 (void);
static void mb_type_rigidify_21__17 (void);
static void mb_type_rigidify_21__18 (void);
static void mb_type_rigidify_stack_21__2 (void);
static void mb_type_rigidify_stack_21__4 (void);
static void mb_type_rigidify_stack_21__5 (void);
static void mb_type_arity_2 (void);
static void mb_type_arity_3 (void);
static void mb_type_max_count_2 (void);
static void mb_type_max_count_3 (void);
static void mb_type_max_count_7 (void);
static void mb_type_max_count_8 (void);
static void mb_data_is_enum_3F__1 (void);
static void mb_type_max_num_params_2 (void);
static void mb_type_max_num_params_3 (void);
static void mb_type_num_morphisms_on_top_2 (void);
static void mb_type_num_morphisms_on_top_3 (void);
static void mb_type_num_morphisms_on_top_5 (void);
static void mb_type_num_morphisms_on_top_6 (void);
static void mb_map_insert_1 (void);
static void mb_map_lookup_1 (void);
static void mb_subst_match_var_1 (void);
static void mb_subst_match_var_3 (void);
static void mb_subst_match_var_2 (void);
static void mb_data_add_tag_21__1 (void);
static void mb_map_keys_1 (void);
static void mb_map_values_1 (void);
static void mb_order2_1 (void);
static void mb_order2_2 (void);
static void mb_order3_1 (void);
static void mb_order3_2 (void);
static void mb_order3_3 (void);
static void mb_order3_4 (void);
static void mb_Bag__3E_Bag_2B__1 (void);
static void mb_bag_split_half_left_1 (void);
static void mb_bag_split_half_right_1 (void);
static void mb_bag_split_half_1 (void);
static void mb_bag_unsnoc_1 (void);
static void mb_bag_insert_2B__2B__1 (void);
static void mb_bag_insert_2B__2B__3 (void);
static void mb_bag_insert_2B__2B__2 (void);
static void mb_bag_insert_2B__2B__4 (void);
static void mb_bag_insert_2B__2B__5 (void);
static void mb_bag_insert_2B__2B__6 (void);
static void mb_bag_insert_2B__2B__7 (void);
static void mb_bag_has_2B__1 (void);
static void mb_bag_has_2B__3 (void);
static void mb_bag_has_2B__2 (void);
static void mb_bag_has_2B__4 (void);
static void mb_bag_has_2B__8 (void);
static void mb_bag_cat_unsafe__2B__1 (void);
static void mb_bag_cat_unsafe_2B__1 (void);
static void mb_bag_cat_unsafe_1 (void);
static void mb_bag_lookup_key_2B__1 (void);
static void mb_bag_lookup_key_2B__5 (void);
static void mb_bag_lookup_key_2B__2 (void);
static void mb_bag_lookup_key_2B__3 (void);
static void mb_bag_lookup_key_2B__4 (void);
static void mb_bag_lookup_key_2B__6 (void);
static void mb_bag_lookup_key_2B__8 (void);
static void mb_bag_lookup_key_2B__10 (void);
static void mb_bag_lookup_key_2B__12 (void);
static void mb_bag_replace_key_2B__2B__1 (void);
static void mb_bag_replace_key_2B__2B__5 (void);
static void mb_bag_replace_key_2B__2B__2 (void);
static void mb_bag_replace_key_2B__2B__3 (void);
static void mb_bag_replace_key_2B__2B__4 (void);
static void mb_bag_replace_key_2B__2B__6 (void);
static void mb_bag_replace_key_2B__2B__7 (void);
static void mb_bag_replace_key_2B__2B__8 (void);
static void mb_bag_replace_key_2B__2B__9 (void);
static void mb__3D__3D_key_1 (void);
static void mb__3C__3D_key_1 (void);
static void mb_delay0_1 (void);
static void mb_delay2_1 (void);
static void mb_delay3_1 (void);
static void mb_delay4_1 (void);
static void mb_force_or2_21__2 (void);
static void mb_type_elab_stack_assertion_1 (void);
static void mb_elab_type_sig_21__1 (void);
static void mb_elab_type_sig_21__2 (void);
static void mb_elab_type_sig_21__3 (void);
static void mb_elab_type_sig_21__4 (void);
static void mb_elab_type_sig_21__5 (void);
static void mb_elab_type_sig_21__7 (void);
static void mb_elab_type_sig_21__6 (void);
static void mb_elab_type_sig_21__8 (void);
static void mb_elab_type_sig_21__9 (void);
static void mb_elab_type_sig_21__10 (void);
static void mb_elab_type_sig_21__11 (void);
static void mb_elab_type_sig_21__12 (void);
static void mb_elab_type_sig_21__13 (void);
static void mb_elab_type_sig_params_21__1 (void);
static void mb_elab_type_sig_params_21__5 (void);
static void mb_elab_type_sig_params_21__2 (void);
static void mb_elab_type_sig_params_21__3 (void);
static void mb_elab_type_sig_params_21__4 (void);
static void mb_elab_type_stack_21__1 (void);
static void mb_elab_type_stack_21__3 (void);
static void mb_elab_type_stack_21__2 (void);
static void mb_elab_type_stack_21__4 (void);
static void mb_elab_type_stack_rest_21__1 (void);
static void mb_elab_type_stack_rest_21__2 (void);
static void mb_elab_type_stack_rest_21__3 (void);
static void mb_elab_type_stack_rest_21__4 (void);
static void mb_elab_type_atom_21__1 (void);
static void mb_elab_type_atom_21__3 (void);
static void mb_elab_type_atom_21__2 (void);
static void mb_elab_type_atom_21__4 (void);
static void mb_elab_type_atom_21__5 (void);
static void mb_elab_type_atom_21__6 (void);
static void mb_elab_type_atom_21__7 (void);
static void mb_elab_type_atom_21__8 (void);
static void mb_elab_type_atom_21__9 (void);
static void mb_elab_type_atom_21__10 (void);
static void mb_elab_type_atom_21__11 (void);
static void mb_elab_type_atom_21__12 (void);
static void mb_elab_type_arg_21__1 (void);
static void mb_elab_type_arg_21__2 (void);
static void mb_elab_type_con_21__2 (void);
static void mb_elab_type_con_21__3 (void);
static void mb_elab_type_dont_care_21__1 (void);
static void mb_elab_type_dont_care_21__4 (void);
static void mb_elab_type_dont_care_21__2 (void);
static void mb_elab_type_dont_care_21__3 (void);
static void mb_elab_type_hole_21__1 (void);
static void mb_elab_type_hole_21__4 (void);
static void mb_elab_type_hole_21__2 (void);
static void mb_elab_type_hole_21__3 (void);
static void mb_elab_type_quote_21__1 (void);
static void mb_elab_type_quote_21__2 (void);
static void mb_elab_implicit_var_21__1 (void);
static void mb_elab_implicit_var_21__2 (void);
static void mb_elab_implicit_var_21__4 (void);
static void mb_elab_implicit_var_21__6 (void);
static void mb_elab_implicit_var_21__7 (void);
static void mb_elab_implicit_var_21__8 (void);
static void mb_elab_implicit_var_21__9 (void);
static void mb_ctx_lookup_1 (void);
static void mb_elab_type_unify_21__1 (void);
static void mb_ctx_new_21__1 (void);
static void mb_elab_type_args_21__1 (void);
static void mb_elab_type_args_21__2 (void);
static void mb_elab_type_args_21__9 (void);
static void mb_elab_type_args_21__3 (void);
static void mb_elab_type_args_21__4 (void);
static void mb_elab_type_args_21__5 (void);
static void mb_elab_type_args_21__6 (void);
static void mb_elab_type_args_21__7 (void);
static void mb_elab_type_args_21__8 (void);
static void mb_elab_simple_type_arg_21__1 (void);
static void mb_ab_save_21__2 (void);
static void mb_ab_build_21__2 (void);
static void mb_ab_build_21__3 (void);
static void mb_ab_build_hom_21__2 (void);
static void mb_ab_build_hom_21__3 (void);
static void mb_ab_unify_type_21__1 (void);
static void mb_ab_build_word_arrow_21__2 (void);
static void mb_ab_build_word_arrow_21__3 (void);
static void mb_ab_build_word_21__2 (void);
static void mb_ab_build_word_21__3 (void);
static void mb_ab_build_word_21__4 (void);
static void mb_ab_atom_21__1 (void);
static void mb_ab_optimized_snoc_21__1 (void);
static void mb_ab_optimized_snoc_21__4 (void);
static void mb_ab_optimized_snoc_21__2 (void);
static void mb_ab_optimized_snoc_21__3 (void);
static void mb_atom_accepts_args_3F__2 (void);
static void mb_atoms_turn_last_block_to_arg_4 (void);
static void mb_atom_to_run_var_2 (void);
static void mb_atom_to_run_var_3 (void);
static void mb_ab_op_21__1 (void);
static void mb_ab_op_21__2 (void);
static void mb_ab_expand_opsig_21__3 (void);
static void mb_ab_expand_opsig_21__5 (void);
static void mb_ab_expand_opsig_21__6 (void);
static void mb_ab_prim_21__1 (void);
static void mb_ab_prim_21__2 (void);
static void mb_ab_block_at_21__2 (void);
static void mb_ab_block_21__2 (void);
static void mb_ab_dip_21__2 (void);
static void mb_ab_if_21__2 (void);
static void mb_ab_if_21__3 (void);
static void mb_ab_while_21__2 (void);
static void mb_ab_while_21__3 (void);
static void mb_ab_lambda_21__2 (void);
static void mb_ab_lambda_21__3 (void);
static void mb_ab_lambda_21__4 (void);
static void mb_ab_lambda_21__5 (void);
static void mb_ab_lambda_21__6 (void);
static void mb_ab_lambda_21__7 (void);
static void mb_ab_lambda_21__8 (void);
static void mb_elab_expand_tensor_21__2 (void);
static void mb_elab_expand_tensor_21__5 (void);
static void mb_elab_expand_tensor_21__7 (void);
static void mb_elab_field_type_21__1 (void);
static void mb_elab_var_sig_21__1 (void);
static void mb_elab_var_sig_21__2 (void);
static void mb_elab_match_sig_21__1 (void);
static void mb_elab_lambda_sig_21__1 (void);
static void mb_elab_word_ctx_sig_21__1 (void);
static void mb_elab_arrow_fwd_21__1 (void);
static void mb_elab_atoms_21__1 (void);
static void mb_elab_atoms_21__2 (void);
static void mb_elab_atoms_21__3 (void);
static void mb_elab_args_21__1 (void);
static void mb_elab_match_cases_21__1 (void);
static void mb_elab_match_cases_21__2 (void);
static void mb_elab_match_exhaustive_21__1 (void);
static void mb_elab_match_exhaustive_21__2 (void);
static void mb_elab_lambda_params_21__1 (void);
static void mb_elab_lambda_params_21__2 (void);
static void mb_elab_lambda_params_21__3 (void);
static void mb_elab_lambda_params_21__4 (void);
static void mb_elab_lambda_params_21__5 (void);
static void mb_elab_lambda_params_21__6 (void);
static void mb_elab_lambda_params_21__7 (void);
static void mb_elab_lambda_params_21__8 (void);
static void mb_elab_lambda_params_21__9 (void);
static void mb_elab_lambda_params_21__10 (void);
static void mb_elab_lambda_params_21__11 (void);
static void mb_elab_lambda_params_21__12 (void);
static void mb_elab_lambda_params_21__13 (void);
static void mb_elab_lambda_body_21__1 (void);
static void mb_elab_lambda_body_21__2 (void);
static void mb_elab_lambda_body_21__3 (void);
static void mb_elab_lambda_body_21__4 (void);
static void mb_elab_lambda_pop_from_mid_21__1 (void);
static void mb_elab_lambda_pop_from_mid_21__2 (void);
static void mb_token_is_lambda_param_3F__1 (void);
static void mb_token_is_lambda_param_3F__2 (void);
static void mb_token_is_lambda_param_3F__3 (void);
static void mb_token_is_lambda_param_3F__6 (void);
static void mb_token_is_lambda_param_3F__4 (void);
static void mb_token_is_lambda_param_3F__5 (void);
static void mb_expect_token_arrow_1 (void);
static void mb_expect_token_arrow_2 (void);
static void mb_elab_match_case_21__1 (void);
static void mb_elab_match_case_21__2 (void);
static void mb_elab_match_case_21__3 (void);
static void mb_elab_match_case_21__4 (void);
static void mb_elab_case_pattern_21__1 (void);
static void mb_elab_case_pattern_21__5 (void);
static void mb_elab_case_pattern_21__2 (void);
static void mb_elab_case_pattern_21__3 (void);
static void mb_elab_case_pattern_21__4 (void);
static void mb_elab_case_pattern_21__6 (void);
static void mb_elab_case_pattern_21__16 (void);
static void mb_elab_case_pattern_21__8 (void);
static void mb_elab_case_pattern_21__9 (void);
static void mb_elab_case_pattern_21__10 (void);
static void mb_elab_case_pattern_21__11 (void);
static void mb_elab_case_pattern_21__12 (void);
static void mb_elab_case_pattern_21__13 (void);
static void mb_elab_case_body_21__1 (void);
static void mb_elab_case_body_21__2 (void);
static void mb_elab_case_body_21__3 (void);
static void mb_elab_case_body_21__4 (void);
static void mb_elab_case_body_21__5 (void);
static void mb_elab_module_header_21__1 (void);
static void mb_elab_module_header_21__8 (void);
static void mb_elab_module_header_21__2 (void);
static void mb_elab_module_header_21__3 (void);
static void mb_elab_module_header_21__4 (void);
static void mb_elab_module_header_21__5 (void);
static void mb_elab_module_header_21__6 (void);
static void mb_elab_module_header_21__7 (void);
static void mb_elab_module_decl_21__3 (void);
static void mb_elab_module_decl_21__4 (void);
static void mb_elab_module_import_21__1 (void);
static void mb_elab_module_import_21__4 (void);
static void mb_elab_module_import_21__6 (void);
static void mb_elab_data_21__1 (void);
static void mb_elab_data_21__2 (void);
static void mb_elab_data_21__3 (void);
static void mb_elab_data_header_21__1 (void);
static void mb_elab_data_header_21__2 (void);
static void mb_elab_data_header_21__3 (void);
static void mb_elab_data_header_21__4 (void);
static void mb_elab_data_tag_21__1 (void);
static void mb_elab_data_tag_21__2 (void);
static void mb_elab_data_tag_21__3 (void);
static void mb_elab_data_tag_21__4 (void);
static void mb_elab_data_tag_21__5 (void);
static void mb_elab_data_tag_21__6 (void);
static void mb_elab_data_tag_21__7 (void);
static void mb_elab_data_tag_21__8 (void);
static void mb_elab_data_tag_21__9 (void);
static void mb_elab_data_tag_21__10 (void);
static void mb_elab_data_tag_21__11 (void);
static void mb_elab_data_tag_21__12 (void);
static void mb_elab_data_tag_21__15 (void);
static void mb_elab_data_tag_21__16 (void);
static void mb_elab_data_tag_21__17 (void);
static void mb_expect_token_comma_1 (void);
static void mb_expect_token_comma_2 (void);
static void mb_expect_token_rparen_1 (void);
static void mb_expect_token_rparen_2 (void);
static void mb_token_def_args_1 (void);
static void mb_token_def_args_2 (void);
static void mb_token_def_args_3 (void);
static void mb_token_def_args_5 (void);
static void mb_token_def_args_4 (void);
static void mb_token_def_args_6 (void);
static void mb_token_def_args_8 (void);
static void mb_token_def_args_7 (void);
static void mb_token_def_args_9 (void);
static void mb_elab_def_missing_21__1 (void);
static void mb_elab_def_missing_21__2 (void);
static void mb_elab_def_21__1 (void);
static void mb_elab_def_21__2 (void);
static void mb_elab_def_21__3 (void);
static void mb_elab_def_21__4 (void);
static void mb_elab_def_21__5 (void);
static void mb_elab_def_21__6 (void);
static void mb_elab_def_21__7 (void);
static void mb_elab_def_21__8 (void);
static void mb_elab_def_21__9 (void);
static void mb_elab_def_21__10 (void);
static void mb_elab_def_21__12 (void);
static void mb_elab_def_21__14 (void);
static void mb_elab_def_21__15 (void);
static void mb_elab_def_21__16 (void);
static void mb_elab_def_21__17 (void);
static void mb_elab_def_21__18 (void);
static void mb_elab_def_21__19 (void);
static void mb_elab_def_21__20 (void);
static void mb_elab_def_params_21__1 (void);
static void mb_elab_def_params_21__2 (void);
static void mb_elab_def_params_21__3 (void);
static void mb_elab_def_params_21__4 (void);
static void mb_elab_def_params_21__5 (void);
static void mb_elab_def_params_21__6 (void);
static void mb_elab_def_params_21__7 (void);
static void mb_elab_def_params_21__8 (void);
static void mb_elab_def_body_21__1 (void);
static void mb_elab_def_body_21__2 (void);
static void mb_elab_def_external_21__1 (void);
static void mb_elab_def_external_21__2 (void);
static void mb_elab_def_external_21__7 (void);
static void mb_elab_def_external_21__3 (void);
static void mb_elab_def_external_21__6 (void);
static void mb_elab_def_external_21__4 (void);
static void mb_elab_def_external_21__5 (void);
static void mb_elab_def_type_21__1 (void);
static void mb_elab_def_type_21__2 (void);
static void mb_elab_def_type_21__5 (void);
static void mb_elab_def_type_21__3 (void);
static void mb_elab_def_type_21__4 (void);
static void mb_elab_buffer_21__1 (void);
static void mb_elab_buffer_21__2 (void);
static void mb_elab_buffer_21__5 (void);
static void mb_elab_buffer_21__3 (void);
static void mb_elab_buffer_21__4 (void);
static void mb_elab_variable_21__1 (void);
static void mb_elab_variable_21__2 (void);
static void mb_elab_variable_21__3 (void);
static void mb_elab_variable_21__4 (void);
static void mb_elab_variable_21__5 (void);
static void mb_elab_table_21__1 (void);
static void mb_elab_table_21__2 (void);
static void mb_elab_table_21__3 (void);
static void mb_table_new_21__1 (void);
static void mb_table_new_21__2 (void);
static void mb_table_new_21__3 (void);
static void mb_table_new_21__4 (void);
static void mb_table_new_21__5 (void);
static void mb_table_new_21__6 (void);
static void mb_table_new_21__9 (void);
static void mb_table_new_21__10 (void);
static void mb_table_new_21__11 (void);
static void mb_table_new_21__12 (void);
static void mb_table_new_21__13 (void);
static void mb_table_new_21__14 (void);
static void mb_elab_target_c99_21__1 (void);
static void mb_elab_target_c99_21__2 (void);
static void mb_elab_target_c99_21__3 (void);
static void mb_elab_embed_str_21__1 (void);
static void mb_elab_embed_str_21__2 (void);
static void mb_elab_embed_str_21__3 (void);
static void mb_elab_embed_str_21__4 (void);
static void mb_elab_embed_str_21__5 (void);
static void mb_elab_embed_str_21__6 (void);
static void mb_elab_embed_str_21__7 (void);
static void mb_elab_embed_str_21__8 (void);
static void mb_elab_embed_str_21__9 (void);
static void mb_elab_field_21__1 (void);
static void mb_elab_field_21__3 (void);
static void mb_elab_field_21__4 (void);
static void mb_field_new_21__1 (void);
static void mb_field_new_21__2 (void);
static void mb_ctx_make_fresh_var_21__1 (void);
static void mb_ctx_make_fresh_var_21__2 (void);
static void mb_name_prim_3D__1 (void);
static void mb_def_prim_21__1 (void);

static void mw_name_str (void);
static void mw_name_def (void);
static void mw_name_mangled (void);
static void mw_module_name (void);
static void mw_module_path (void);
static void mw_module_start (void);
static void mw_module_end (void);
static void mw_module_imports (void);
static void mw_token_value (void);
static void mw_token_module (void);
static void mw_token_row (void);
static void mw_token_col (void);
static void mw_buffer_size (void);
static void mw_buffer_name (void);
static void mw_meta_type (void);
static void mw_data_header (void);
static void mw_data_name (void);
static void mw_data_arity (void);
static void mw_data_tags (void);
static void mw_tag_data (void);
static void mw_tag_name (void);
static void mw_tag_value (void);
static void mw_tag_sig (void);
static void mw_tag_ctx_type (void);
static void mw_arrow_token_start (void);
static void mw_arrow_token_end (void);
static void mw_arrow_home (void);
static void mw_arrow_homeidx (void);
static void mw_arrow_ctx (void);
static void mw_arrow_dom (void);
static void mw_arrow_cod (void);
static void mw_arrow_atoms (void);
static void mw_atom_token (void);
static void mw_atom_ctx (void);
static void mw_atom_op (void);
static void mw_atom_args (void);
static void mw_atom_dom (void);
static void mw_atom_cod (void);
static void mw_atom_subst (void);
static void mw_lambda_token (void);
static void mw_lambda_outer_ctx (void);
static void mw_lambda_inner_ctx (void);
static void mw_lambda_dom (void);
static void mw_lambda_mid (void);
static void mw_lambda_cod (void);
static void mw_lambda_params (void);
static void mw_lambda_body (void);
static void mw_block_ctx (void);
static void mw_block_token (void);
static void mw_block_dom (void);
static void mw_block_cod (void);
static void mw_block_arrow (void);
static void mw_match_ctx (void);
static void mw_match_dom (void);
static void mw_match_cod (void);
static void mw_match_token (void);
static void mw_match_body (void);
static void mw_match_cases (void);
static void mw_case_match (void);
static void mw_case_token (void);
static void mw_case_pattern (void);
static void mw_case_subst (void);
static void mw_case_mid (void);
static void mw_case_body (void);
static void mw_var_is_implicit (void);
static void mw_var_name (void);
static void mw_var_type (void);
static void mw_var_auto_run (void);
static void mw_word_name (void);
static void mw_word_head (void);
static void mw_word_sig (void);
static void mw_word_body (void);
static void mw_word_ctx_type (void);
static void mw_word_params (void);
static void mw_word_arrow (void);
static void mw_table_name (void);
static void mw_table_num_buffer (void);
static void mw_table_max_count (void);
static void mw_field_name (void);
static void mw_field_index_type (void);
static void mw_field_value_type (void);
static void mw_external_name (void);
static void mw_external_sig (void);
static void mw_external_ctx_type (void);
static void mw_variable_name (void);
static void mw_variable_type (void);
static void mw_prim_name (void);
static void mw_prim_ctx (void);
static void mw_prim_type (void);
static void mw_prim_decl (void);

int main (int argc, char** argv) {
    global_argc = argc;
    global_argv = argv;
    mw_main();
    return 0;
}

static void mw_id (void){
    mw_prim_id();
}

static void mw__3F__3F_ (void){
    mw_prim_debug();
}

static void mw_swap (void){
    mw_prim_swap();
}

static void mw_dup (void){
    mw_prim_dup();
}

static void mw_drop (void){
    mw_prim_drop();
}

static void mw_run (void){
    {
        VAL var_f = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        decref(var_f);
    }
}

static void mw__3D__3D_ (void){
    mw_prim_value_eq();
}

static void mw__3C__3D_ (void){
    mw_prim_value_le();
}

static void mw__3C_ (void){
    mw_prim_value_lt();
}

static void mw_value_40_ (void){
    mw_prim_value_get();
}

static void mw_value_21_ (void){
    mw_prim_value_set();
}

static void mw__2B_ (void){
    mw_prim_int_add();
}

static void mw__ (void){
    mw_prim_int_sub();
}

static void mw__2A_ (void){
    mw_prim_int_mul();
}

static void mw__2F_ (void){
    mw_prim_int_div();
}

static void mw__25_ (void){
    mw_prim_int_mod();
}

static void mw__26_ (void){
    mw_prim_int_and();
}

static void mw__7C_ (void){
    mw_prim_int_or();
}

static void mw__5E_ (void){
    mw_prim_int_xor();
}

static void mw__3C__3C_ (void){
    mw_prim_int_shl();
}

static void mw__3E__3E_ (void){
    mw_prim_int_shr();
}

static void mw_int_40_ (void){
    mw_prim_int_get();
}

static void mw_int_21_ (void){
    mw_prim_int_set();
}

static void mw_true (void){
    mw_prim_bool_true();
}

static void mw_false (void){
    mw_prim_bool_false();
}

static void mw__26__26_ (void){
    mw_prim_bool_and();
}

static void mw__7C__7C_ (void){
    mw_prim_bool_or();
}

static void mw_ptr_2B_ (void){
    mw_prim_ptr_add();
}

static void mw_with_ptr_2B_ (void){
    {
        VAL var_f = pop_value();
        mw_dup();
        {
            VAL d3 = pop_value();
            mw_prim_ptr_add();
            push_value(var_f);
            incref(var_f);
            mw_prim_run();
            push_value(d3);
        }
        mw_drop();
        decref(var_f);
    }
}

static void mw_ptr_40_ (void){
    mw_prim_ptr_get();
}

static void mw_ptr_21_ (void){
    mw_prim_ptr_set();
}

static void mw__7C_ptr_7C_ (void){
    mw_prim_ptr_size();
}

static void mw_with_raw_ptr (void){
    {
        VAL var_f = pop_value();
        mw_prim_ptr_raw();
        mw_RAWPTR();
        mw_swap();
        {
            VAL d3 = pop_value();
            push_value(var_f);
            incref(var_f);
            mw_prim_run();
            push_value(d3);
        }
        decref(var_f);
    }
}

static void mw_u8_40_ (void){
    mw_prim_u8_get();
}

static void mw_u8_21_ (void){
    mw_prim_u8_set();
}

static void mw_u16_40_ (void){
    mw_prim_u16_get();
}

static void mw_u16_21_ (void){
    mw_prim_u16_set();
}

static void mw_u32_40_ (void){
    mw_prim_u32_get();
}

static void mw_u32_21_ (void){
    mw_prim_u32_set();
}

static void mw_u64_40_ (void){
    mw_prim_u64_get();
}

static void mw_u64_21_ (void){
    mw_prim_u64_set();
}

static void mw_i8_40_ (void){
    mw_prim_i8_get();
}

static void mw_i8_21_ (void){
    mw_prim_i8_set();
}

static void mw_i16_40_ (void){
    mw_prim_i16_get();
}

static void mw_i16_21_ (void){
    mw_prim_i16_set();
}

static void mw_i32_40_ (void){
    mw_prim_i32_get();
}

static void mw_i32_21_ (void){
    mw_prim_i32_set();
}

static void mw_i64_40_ (void){
    mw_prim_i64_get();
}

static void mw_i64_21_ (void){
    mw_prim_i64_set();
}

static void mw_OS__3E_Int (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            push_i64(0LL);
            break;
        case 1LL:
            mw_prim_drop();
            push_i64(1LL);
            break;
        case 2LL:
            mw_prim_drop();
            push_i64(2LL);
            break;
        case 3LL:
            mw_prim_drop();
            push_i64(3LL);
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_Int__3E_OS (void){
    mw_dup();
    push_i64(1LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_OS_WINDOWS();
    } else {
        mw_dup();
        push_i64(2LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            mw_OS_LINUX();
        } else {
            mw_dup();
            push_i64(3LL);
            mw__3D__3D_();
            if (pop_u64()) {
                mw_drop();
                mw_OS_MACOS();
            } else {
                mw_drop();
                mw_OS_UNKNOWN();
            }
        }
    }
}

static void mw_RUNNING_OS (void){
    mw_prim_sys_os();
    mw_Int__3E_OS();
}

static void mw_argc (void){
    mw_prim_sys_argc();
}

static void mw_argv (void){
    mw_prim_sys_argv();
}

static void mw_posix_read_21_ (void){
    mw_prim_posix_read();
}

static void mw_posix_write_21_ (void){
    mw_prim_posix_write();
}

static void mw_posix_open_21_ (void){
    push_u64(0);
    push_fnptr(&mb_posix_open_21__1);
    mw_prim_pack_cons();
    mw_dip2();
    mw_prim_posix_open();
}

static void mw_posix_close_21_ (void){
    mw_prim_posix_close();
}

static void mw_posix_exit_21_ (void){
    mw_prim_posix_exit();
}

static void mw_rotr (void){
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_swap();
        push_value(d2);
    }
}

static void mw_rotl (void){
    {
        VAL d2 = pop_value();
        mw_swap();
        push_value(d2);
    }
    mw_swap();
}

static void mw_over (void){
    {
        VAL d2 = pop_value();
        mw_dup();
        push_value(d2);
    }
    mw_swap();
}

static void mw_over2 (void){
    {
        VAL d2 = pop_value();
        mw_over();
        push_value(d2);
    }
    mw_swap();
}

static void mw_over3 (void){
    {
        VAL d2 = pop_value();
        mw_over2();
        push_value(d2);
    }
    mw_swap();
}

static void mw_tuck (void){
    mw_dup();
    {
        VAL d2 = pop_value();
        mw_swap();
        push_value(d2);
    }
}

static void mw_nip (void){
    {
        VAL d2 = pop_value();
        mw_drop();
        push_value(d2);
    }
}

static void mw_dup2 (void){
    mw_over();
    mw_over();
}

static void mw_dup3 (void){
    mw_dup();
    {
        VAL d2 = pop_value();
        {
            VAL d3 = pop_value();
            mw_dup2();
            push_value(d3);
        }
        mw_rotr();
        push_value(d2);
    }
}

static void mw_dip_3F_ (void){
    {
        VAL var_f = pop_value();
        {
            VAL d3 = pop_value();
            push_value(var_f);
            incref(var_f);
            mw_prim_run();
            push_value(d3);
        }
        mw_swap();
        decref(var_f);
    }
}

static void mw_dip_27_ (void){
    {
        VAL var_f = pop_value();
        mw_swap();
        {
            VAL d3 = pop_value();
            push_value(var_f);
            incref(var_f);
            mw_prim_run();
            push_value(d3);
        }
        mw_swap();
        decref(var_f);
    }
}

static void mw_dip2 (void){
    {
        VAL var_f = pop_value();
        {
            VAL d3 = pop_value();
            {
                VAL d4 = pop_value();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                push_value(d4);
            }
            push_value(d3);
        }
        decref(var_f);
    }
}

static void mw_dip3 (void){
    {
        VAL var_f = pop_value();
        {
            VAL d3 = pop_value();
            {
                VAL d4 = pop_value();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                push_value(d4);
            }
            push_value(d3);
        }
        decref(var_f);
    }
}

static void mw_sip (void){
    {
        VAL var_f = pop_value();
        mw_dup();
        {
            VAL d3 = pop_value();
            push_value(var_f);
            incref(var_f);
            mw_prim_run();
            push_value(d3);
        }
        decref(var_f);
    }
}

static void mw_sip2 (void){
    {
        VAL var_f = pop_value();
        mw_dup2();
        {
            VAL d3 = pop_value();
            {
                VAL d4 = pop_value();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                push_value(d4);
            }
            push_value(d3);
        }
        decref(var_f);
    }
}

static void mw_drop2 (void){
    mw_drop();
    mw_drop();
}

static void mw_drop3 (void){
    mw_drop();
    mw_drop();
    mw_drop();
}

static void mw_drop4 (void){
    mw_drop();
    mw_drop();
    mw_drop();
    mw_drop();
}

static void mw_rot4r (void){
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_rotr();
        push_value(d2);
    }
}

static void mw_rot4l (void){
    {
        VAL d2 = pop_value();
        mw_rotl();
        push_value(d2);
    }
    mw_swap();
}

static void mw_or (void){
    {
        VAL var_f = pop_value();
        if (pop_u64()) {
            mw_true();
        } else {
            push_value(var_f);
            incref(var_f);
            mw_prim_run();
        }
        decref(var_f);
    }
}

static void mw_and (void){
    {
        VAL var_f = pop_value();
        if (pop_u64()) {
            push_value(var_f);
            incref(var_f);
            mw_prim_run();
        } else {
            mw_false();
        }
        decref(var_f);
    }
}

static void mw_repeat (void){
    {
        VAL var_f = pop_value();
        while(1) {
            mw_dup();
            push_i64(0LL);
            mw__3E_();
            if (!pop_u64()) break;
            {
                VAL d4 = pop_value();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                push_value(d4);
            }
            mw_1_();
        }
        mw_drop();
        decref(var_f);
    }
}

static void mw_count (void){
    {
        VAL var_f = pop_value();
        push_i64(0LL);
        mw_swap();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_count_2);
        mw_prim_pack_cons();
        mw_repeat();
        mw_drop();
        decref(var_f);
    }
}

static void mw_countdown (void){
    {
        VAL var_f = pop_value();
        mw_dup();
        mw_1_();
        mw_swap();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_countdown_2);
        mw_prim_pack_cons();
        mw_repeat();
        mw_drop();
        decref(var_f);
    }
}

static void mw_Str__3E_Ptr (void){
    mw_prim_unsafe_cast();
}

static void mw_Ptr__3E_Str (void){
    mw_prim_unsafe_cast();
}

static void mw_U8_MAX (void){
    push_i64(255LL);
}

static void mw_U16_MAX (void){
    push_i64(65535LL);
}

static void mw_U32_MAX (void){
    push_i64(4294967295LL);
}

static void mw_I8_MAX (void){
    push_i64(127LL);
}

static void mw_I16_MAX (void){
    push_i64(32767LL);
}

static void mw_I32_MAX (void){
    push_i64(2147483647LL);
}

static void mw_U8_MIN (void){
    push_i64(0LL);
}

static void mw_U16_MIN (void){
    push_i64(0LL);
}

static void mw_U32_MIN (void){
    push_i64(0LL);
}

static void mw_I8_MIN (void){
    push_i64(-128LL);
}

static void mw_I16_MIN (void){
    push_i64(-32768LL);
}

static void mw_I32_MIN (void){
    push_i64(-2147483648LL);
}

static void mw_not (void){
    mw_false();
    mw__3D__3D_();
}

static void mw_cmp (void){
    mw_dup2();
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop2();
        mw_EQ();
    } else {
        mw__3C_();
        if (pop_u64()) {
            mw_LT();
        } else {
            mw_GT();
        }
    }
}

static void mw_cmp_3F_ (void){
    mw_dup2();
    mw_cmp();
}

static void mw__3C__3E_ (void){
    mw__3D__3D_();
    mw_not();
}

static void mw__3E_ (void){
    mw_swap();
    mw__3C_();
}

static void mw__3E__3D_ (void){
    mw_swap();
    mw__3C__3D_();
}

static void mw_0_3D_ (void){
    push_i64(0LL);
    mw__3D__3D_();
}

static void mw_0_3C_ (void){
    push_i64(0LL);
    mw__3C_();
}

static void mw_0_3E_ (void){
    push_i64(0LL);
    mw__3E_();
}

static void mw_0_3C__3E_ (void){
    push_i64(0LL);
    mw__3C__3E_();
}

static void mw_1_2B_ (void){
    push_i64(1LL);
    mw__2B_();
}

static void mw_1_ (void){
    push_i64(1LL);
    mw__();
}

static void mw_max (void){
    mw_dup2();
    mw__3C_();
    if (pop_u64()) {
        mw_nip();
    } else {
        mw_drop();
    }
}

static void mw_min (void){
    mw_dup2();
    mw__3C_();
    if (pop_u64()) {
        mw_drop();
    } else {
        mw_nip();
    }
}

static void mw_ptrs (void){
    mw__7C_ptr_7C_();
    mw__2A_();
}

static void mw_ptr_40__40_ (void){
    {
        VAL d2 = pop_value();
        mw_ptrs();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_ptr_40__40__2);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mw_ptr_21__21_ (void){
    {
        VAL d2 = pop_value();
        mw_ptrs();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_ptr_21__21__2);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mw_u8_40__40_ (void){
    push_u64(0);
    push_fnptr(&mb_u8_40__40__1);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mw_u8_21__21_ (void){
    push_u64(0);
    push_fnptr(&mb_u8_21__21__1);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mw_ints (void){
    push_i64(8LL);
    mw__2A_();
}

static void mw_int_40__40_ (void){
    {
        VAL d2 = pop_value();
        mw_ints();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_int_40__40__2);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mw_int_21__21_ (void){
    {
        VAL d2 = pop_value();
        mw_ints();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_int_21__21__2);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mw_values (void){
    push_i64(16LL);
    mw__2A_();
}

static void mw_value_40__40_ (void){
    {
        VAL d2 = pop_value();
        mw_values();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_value_40__40__2);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mw_value_21__21_ (void){
    {
        VAL d2 = pop_value();
        mw_values();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_value_21__21__2);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mw_in_range (void){
    {
        VAL d2 = pop_value();
        mw_over();
        {
            VAL d3 = pop_value();
            mw__3E__3D_();
            push_value(d3);
        }
        push_value(d2);
    }
    mw__3C__3D_();
    mw__26__26_();
}

static void mw_Int__3E_U8 (void){
    mw_prim_unsafe_cast();
}

static void mw_Int__3E_U16 (void){
    mw_prim_unsafe_cast();
}

static void mw_Int__3E_U32 (void){
    mw_prim_unsafe_cast();
}

static void mw_Int__3E_U64 (void){
    mw_prim_unsafe_cast();
}

static void mw_Int__3E_I8 (void){
    mw_prim_unsafe_cast();
}

static void mw_Int__3E_I16 (void){
    mw_prim_unsafe_cast();
}

static void mw_Int__3E_I32 (void){
    mw_prim_unsafe_cast();
}

static void mw_Int__3E_I64 (void){
    mw_prim_unsafe_cast();
}

static void mw_U8__3E_Int (void){
    mw_prim_unsafe_cast();
}

static void mw_U16__3E_Int (void){
    mw_prim_unsafe_cast();
}

static void mw_U32__3E_Int (void){
    mw_prim_unsafe_cast();
}

static void mw_U64__3E_Int (void){
    mw_prim_unsafe_cast();
}

static void mw_I8__3E_Int (void){
    mw_prim_unsafe_cast();
}

static void mw_I16__3E_Int (void){
    mw_prim_unsafe_cast();
}

static void mw_I32__3E_Int (void){
    mw_prim_unsafe_cast();
}

static void mw_I64__3E_Int (void){
    mw_prim_unsafe_cast();
}

static void mw_Int__3E_Char (void){
    mw_prim_unsafe_cast();
}

static void mw_Char__3E_Int (void){
    mw_prim_unsafe_cast();
}

static void mw_negate (void){
    push_i64(-1LL);
    mw__2A_();
}

static void mw_abs (void){
    mw_dup();
    mw_0_3C_();
    if (pop_u64()) {
        mw_negate();
    } else {
        mw_id();
    }
}

static void mw_nil (void){
    push_i64(0LL);
    mw_prim_unsafe_cast();
}

static void mw_is_nil (void){
    mw_nil();
    mw__3D__3D_();
}

static void mw_is_nil_3F_ (void){
    mw_dup();
    mw_nil();
    mw__3D__3D_();
}

static void mw_pack_nil (void){
    mw_nil();
}

static void mw_pack_nil_3F_ (void){
    mw_dup();
    mw_pack_nil();
    mw__3D__3D_();
}

static void mw_pack0 (void){
    mw_pack_nil();
}

static void mw_pack1 (void){
    {
        VAL d2 = pop_value();
        mw_pack0();
        push_value(d2);
    }
    mw_prim_pack_cons();
}

static void mw_pack2 (void){
    {
        VAL d2 = pop_value();
        mw_pack1();
        push_value(d2);
    }
    mw_prim_pack_cons();
}

static void mw_pack3 (void){
    {
        VAL d2 = pop_value();
        mw_pack2();
        push_value(d2);
    }
    mw_prim_pack_cons();
}

static void mw_pack4 (void){
    {
        VAL d2 = pop_value();
        mw_pack3();
        push_value(d2);
    }
    mw_prim_pack_cons();
}

static void mw_pack5 (void){
    {
        VAL d2 = pop_value();
        mw_pack4();
        push_value(d2);
    }
    mw_prim_pack_cons();
}

static void mw_unpack0 (void){
    mw_drop();
}

static void mw_unpack1 (void){
    mw_prim_pack_uncons();
    mw_nip();
}

static void mw_unpack2 (void){
    mw_prim_pack_uncons();
    {
        VAL d2 = pop_value();
        mw_unpack1();
        push_value(d2);
    }
}

static void mw_unpack3 (void){
    mw_prim_pack_uncons();
    {
        VAL d2 = pop_value();
        mw_unpack2();
        push_value(d2);
    }
}

static void mw_unpack4 (void){
    mw_prim_pack_uncons();
    {
        VAL d2 = pop_value();
        mw_unpack3();
        push_value(d2);
    }
}

static void mw_unpack5 (void){
    mw_prim_pack_uncons();
    {
        VAL d2 = pop_value();
        mw_unpack4();
        push_value(d2);
    }
}

static void mw_mut (void){
    mw_prim_mut_new();
}

static void mw__40_ (void){
    mw_prim_mut_get();
}

static void mw__21_ (void){
    mw_prim_mut_set();
    mw_drop();
}

static void mw_modify (void){
    {
        VAL var_f = pop_value();
        mw_dup();
        {
            VAL d3 = pop_value();
            mw__40_();
            push_value(var_f);
            incref(var_f);
            mw_prim_run();
            push_value(d3);
        }
        mw__21_();
        decref(var_f);
    }
}

static void mw_is_none (void){
    mw_is_nil();
}

static void mw_is_some (void){
    mw_is_nil();
    mw_not();
}

static void mw_is_none_3F_ (void){
    mw_is_nil_3F_();
}

static void mw_is_some_3F_ (void){
    mw_is_nil_3F_();
    mw_not();
}

static void mw_unwrap (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            push_ptr("tried to unwrap NONE\0\0\0\0");
            mw_panic_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_id();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_unwrap_or (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_id();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_maybe_map (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_NONE();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                mw_SOME();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_maybe_bind (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_NONE();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_maybe_for (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_id();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_maybe_filter (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_NONE();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                if (pop_u64()) {
                    mw_SOME();
                } else {
                    mw_drop();
                    mw_NONE();
                }
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_while_some (void){
    {
        VAL var_g = pop_value();
        VAL var_f = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        while(1) {
            mw_is_some_3F_();
            if (!pop_u64()) break;
            mw_unwrap();
            push_value(var_g);
            incref(var_g);
            mw_prim_run();
            push_value(var_f);
            incref(var_f);
            mw_prim_run();
        }
        mw_drop();
        decref(var_g);
        decref(var_f);
    }
}

static void mw_L4 (void){
    mw_L4_2B_();
    mw_List_2B___3E_List();
}

static void mw_L5 (void){
    mw_L5_2B_();
    mw_List_2B___3E_List();
}

static void mw_L6 (void){
    mw_L6_2B_();
    mw_List_2B___3E_List();
}

static void mw_L7 (void){
    mw_L7_2B_();
    mw_List_2B___3E_List();
}

static void mw_L8 (void){
    mw_L8_2B_();
    mw_List_2B___3E_List();
}

static void mw_L9 (void){
    mw_L9_2B_();
    mw_List_2B___3E_List();
}

static void mw_L10 (void){
    mw_L10_2B_();
    mw_List_2B___3E_List();
}

static void mw_L11 (void){
    mw_L11_2B_();
    mw_List_2B___3E_List();
}

static void mw_L12 (void){
    mw_L12_2B_();
    mw_List_2B___3E_List();
}

static void mw_L4_2B_ (void){
    mw_L2_2B_();
    {
        VAL d2 = pop_value();
        mw_L2_2B_();
        push_value(d2);
    }
    push_i64(4LL);
    mw_LCAT_2B_();
}

static void mw_L5_2B_ (void){
    mw_L3_2B_();
    {
        VAL d2 = pop_value();
        mw_L2_2B_();
        push_value(d2);
    }
    push_i64(5LL);
    mw_LCAT_2B_();
}

static void mw_L6_2B_ (void){
    mw_L3_2B_();
    {
        VAL d2 = pop_value();
        mw_L3_2B_();
        push_value(d2);
    }
    push_i64(6LL);
    mw_LCAT_2B_();
}

static void mw_L7_2B_ (void){
    mw_L4_2B_();
    {
        VAL d2 = pop_value();
        mw_L3_2B_();
        push_value(d2);
    }
    push_i64(7LL);
    mw_LCAT_2B_();
}

static void mw_L8_2B_ (void){
    mw_L5_2B_();
    {
        VAL d2 = pop_value();
        mw_L3_2B_();
        push_value(d2);
    }
    push_i64(8LL);
    mw_LCAT_2B_();
}

static void mw_L9_2B_ (void){
    mw_L6_2B_();
    {
        VAL d2 = pop_value();
        mw_L3_2B_();
        push_value(d2);
    }
    push_i64(9LL);
    mw_LCAT_2B_();
}

static void mw_L10_2B_ (void){
    mw_L5_2B_();
    {
        VAL d2 = pop_value();
        mw_L5_2B_();
        push_value(d2);
    }
    push_i64(10LL);
    mw_LCAT_2B_();
}

static void mw_L11_2B_ (void){
    mw_L6_2B_();
    {
        VAL d2 = pop_value();
        mw_L5_2B_();
        push_value(d2);
    }
    push_i64(11LL);
    mw_LCAT_2B_();
}

static void mw_L12_2B_ (void){
    mw_L6_2B_();
    {
        VAL d2 = pop_value();
        mw_L6_2B_();
        push_value(d2);
    }
    push_i64(12LL);
    mw_LCAT_2B_();
}

static void mw_List_2B___3E_List (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_L1();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L2();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L3();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_LCAT();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_List__3E_List_2B_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_NONE();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_L1_2B_();
            mw_SOME();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L2_2B_();
            mw_SOME();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L3_2B_();
            mw_SOME();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_LCAT_2B_();
            mw_SOME();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_len (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            push_i64(0LL);
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            push_i64(1LL);
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop2();
            push_i64(2LL);
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop3();
            push_i64(3LL);
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_drop2();
                push_value(d4);
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_len_2B_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            push_i64(1LL);
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop2();
            push_i64(2LL);
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop3();
            push_i64(3LL);
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_drop2();
                push_value(d4);
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_len_3F_ (void){
    mw_dup();
    mw_len();
}

static void mw_len_2B__3F_ (void){
    mw_dup();
    mw_len_2B_();
}

static void mw_cons_2B_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_L1_2B_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_L2_2B_();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L3_2B_();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L4_2B_();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_1_2B_();
            {
                VAL d4 = pop_value();
                {
                    VAL d5 = pop_value();
                    mw_cons_2B__2B_();
                    push_value(d5);
                }
                mw_rebalance_2B_();
                push_value(d4);
            }
            mw_LCAT_2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_snoc_2B_ (void){
    mw_swap();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_L1_2B_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_L2_2B_();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_rotl();
            mw_L3_2B_();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_rot4l();
            mw_L4_2B_();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_1_2B_();
            {
                VAL d4 = pop_value();
                mw_rotl();
                mw_snoc_2B__2B_();
                mw_rebalance_2B_();
                push_value(d4);
            }
            mw_LCAT_2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_cons_2B__2B_ (void){
    mw_List_2B___3E_List();
    mw_cons_2B_();
}

static void mw_snoc_2B__2B_ (void){
    {
        VAL d2 = pop_value();
        mw_List_2B___3E_List();
        push_value(d2);
    }
    mw_snoc_2B_();
}

static void mw_cons (void){
    mw_cons_2B_();
    mw_List_2B___3E_List();
}

static void mw_snoc (void){
    mw_snoc_2B_();
    mw_List_2B___3E_List();
}

static void mw_uncons (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_L0();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L1();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L2();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            {
                VAL d4 = pop_value();
                mw_uncons();
                push_value(d4);
            }
            mw_cat__2B_();
            mw_List_2B___3E_List();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_unsnoc (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            {
                VAL d4 = pop_value();
                mw_L0();
                push_value(d4);
            }
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_L1();
                push_value(d4);
            }
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_L2();
                push_value(d4);
            }
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            mw_unsnoc();
            {
                VAL d4 = pop_value();
                mw_cat_2B__();
                mw_List_2B___3E_List();
                push_value(d4);
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_cat (void){
    mw_swap();
    mw_List__3E_List_2B_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_id();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_List__3E_List_2B_();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_drop();
                    mw_List_2B___3E_List();
                    break;
                case 1LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_cat_2B_();
                    mw_List_2B___3E_List();
                    break;
                default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
            
}            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_cat__2B_ (void){
    mw_swap();
    mw_List__3E_List_2B_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_id();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_cat_2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_cat_2B__ (void){
    mw_List__3E_List_2B_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_id();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_cat_2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_cat_2B_ (void){
    mw_swap();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_cons_2B__2B_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_rotl();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_L3_2B_();
                    break;
                case 1LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_prim_pack_uncons(); mw_prim_swap();
                    mw_L4_2B_();
                    break;
                case 2LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_prim_pack_uncons(); mw_prim_swap();
                    mw_prim_pack_uncons(); mw_prim_swap();
                    mw_L5_2B_();
                    break;
                default:
                    {
                        VAL d6 = pop_value();
                        mw_L2_2B_();
                        push_value(d6);
                    }
                    mw_cat_aux();
                    break;
            
}            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_rot4l();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_L4_2B_();
                    break;
                case 1LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_prim_pack_uncons(); mw_prim_swap();
                    mw_L5_2B_();
                    break;
                case 2LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_prim_pack_uncons(); mw_prim_swap();
                    mw_prim_pack_uncons(); mw_prim_swap();
                    mw_L6_2B_();
                    break;
                default:
                    {
                        VAL d6 = pop_value();
                        mw_L3_2B_();
                        push_value(d6);
                    }
                    mw_cat_aux();
                    break;
            
}            break;
        default:
            mw_swap();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_snoc_2B__2B_();
                    break;
                default:
                    mw_cat_aux();
                    break;
            
}            break;
    
}}

static void mw_cat_aux (void){
    mw_rebalance_2B_();
    mw_dup2();
    {
        VAL d2 = pop_value();
        mw_len_2B_();
        push_value(d2);
    }
    mw_len_2B_();
    mw__2B_();
    mw_LCAT_2B_();
}

static void mw_rebalance_2B_ (void){
    mw_dup2();
    {
        VAL d2 = pop_value();
        mw_len_2B_();
        push_value(d2);
    }
    mw_len_2B_();
    mw_dup2();
    push_i64(3LL);
    mw__2A_();
    mw__3E_();
    if (pop_u64()) {
        mw_drop2();
        {
            VAL d3 = pop_value();
            mw_split_half_left();
            push_value(d3);
        }
        mw_cat__2B_();
        mw_rebalance_2B_();
    } else {
        {
            VAL d3 = pop_value();
            push_i64(3LL);
            mw__2A_();
            push_value(d3);
        }
        mw__3C_();
        if (pop_u64()) {
            mw_split_half_right();
            {
                VAL d4 = pop_value();
                mw_cat_2B__();
                push_value(d4);
            }
            mw_rebalance_2B_();
        } else {
            mw_id();
        }
    }
}

static void mw_split_half_left (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_L0();
            {
                VAL d4 = pop_value();
                mw_L1_2B_();
                push_value(d4);
            }
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L1();
            {
                VAL d4 = pop_value();
                mw_L1_2B_();
                push_value(d4);
            }
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L1();
            {
                VAL d4 = pop_value();
                mw_L2_2B_();
                push_value(d4);
            }
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            mw_List_2B___3E_List();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_split_half_right (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_L1_2B_();
            {
                VAL d4 = pop_value();
                mw_L0();
                push_value(d4);
            }
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L1_2B_();
            {
                VAL d4 = pop_value();
                mw_L1();
                push_value(d4);
            }
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L2_2B_();
            {
                VAL d4 = pop_value();
                mw_L1();
                push_value(d4);
            }
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            {
                VAL d4 = pop_value();
                mw_List_2B___3E_List();
                push_value(d4);
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_split_half (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_L0();
            mw_L0();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_L1();
            {
                VAL d4 = pop_value();
                mw_L0();
                push_value(d4);
            }
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L1();
            {
                VAL d4 = pop_value();
                mw_L1();
                push_value(d4);
            }
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_L2();
            {
                VAL d4 = pop_value();
                mw_L1();
                push_value(d4);
            }
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            {
                VAL d4 = pop_value();
                mw_List_2B___3E_List();
                push_value(d4);
            }
            mw_List_2B___3E_List();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_first (void){
    mw_List__3E_List_2B_();
    push_u64(0);
    push_fnptr(&mb_first_1);
    mw_prim_pack_cons();
    mw_maybe_map();
}

static void mw_last (void){
    mw_List__3E_List_2B_();
    push_u64(0);
    push_fnptr(&mb_last_1);
    mw_prim_pack_cons();
    mw_maybe_map();
}

static void mw_middle (void){
    mw_List__3E_List_2B_();
    push_u64(0);
    push_fnptr(&mb_middle_1);
    mw_prim_pack_cons();
    mw_maybe_map();
}

static void mw_first_2B_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_id();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop2();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop2();
            mw_first_2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_last_2B_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_id();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_drop();
                push_value(d4);
            }
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_drop2();
                push_value(d4);
            }
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            mw_nip();
            mw_last_2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_middle_2B_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_id();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_nip();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            mw_nip();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            mw_nip();
            mw_first_2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_reverse (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_L0();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_L1();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_swap();
            mw_L2();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_rotr();
            mw_swap();
            mw_L3();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_reverse_2B_();
                mw_swap();
                mw_reverse_2B_();
                push_value(d4);
            }
            mw_LCAT();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_reverse_2B_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_L1_2B_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_swap();
            mw_L2_2B_();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_rotr();
            mw_swap();
            mw_L3_2B_();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_reverse_2B_();
                mw_swap();
                mw_reverse_2B_();
                push_value(d4);
            }
            mw_LCAT_2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_map (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_L0();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                mw_L1();
                break;
            case 2LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                mw_swap();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                mw_swap();
                mw_L2();
                break;
            case 3LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                {
                    VAL d5 = pop_value();
                    {
                        VAL d6 = pop_value();
                        push_value(var_f);
                        incref(var_f);
                        mw_prim_run();
                        push_value(d6);
                    }
                    push_value(d5);
                }
                mw_rotr();
                {
                    VAL d5 = pop_value();
                    {
                        VAL d6 = pop_value();
                        push_value(var_f);
                        incref(var_f);
                        mw_prim_run();
                        push_value(d6);
                    }
                    push_value(d5);
                }
                mw_rotr();
                {
                    VAL d5 = pop_value();
                    {
                        VAL d6 = pop_value();
                        push_value(var_f);
                        incref(var_f);
                        mw_prim_run();
                        push_value(d6);
                    }
                    push_value(d5);
                }
                mw_rotr();
                mw_L3();
                break;
            case 4LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                {
                    VAL d5 = pop_value();
                    {
                        VAL d6 = pop_value();
                        push_value(var_f);
                        incref(var_f);
                        mw_map_2B_();
                        push_value(d6);
                    }
                    mw_swap();
                    {
                        VAL d6 = pop_value();
                        push_value(var_f);
                        incref(var_f);
                        mw_map_2B_();
                        push_value(d6);
                    }
                    mw_swap();
                    push_value(d5);
                }
                mw_LCAT();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_map_2B_ (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                mw_L1_2B_();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                mw_swap();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                mw_swap();
                mw_L2_2B_();
                break;
            case 2LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                {
                    VAL d5 = pop_value();
                    {
                        VAL d6 = pop_value();
                        push_value(var_f);
                        incref(var_f);
                        mw_prim_run();
                        push_value(d6);
                    }
                    push_value(d5);
                }
                mw_rotr();
                {
                    VAL d5 = pop_value();
                    {
                        VAL d6 = pop_value();
                        push_value(var_f);
                        incref(var_f);
                        mw_prim_run();
                        push_value(d6);
                    }
                    push_value(d5);
                }
                mw_rotr();
                {
                    VAL d5 = pop_value();
                    {
                        VAL d6 = pop_value();
                        push_value(var_f);
                        incref(var_f);
                        mw_prim_run();
                        push_value(d6);
                    }
                    push_value(d5);
                }
                mw_rotr();
                mw_L3_2B_();
                break;
            case 3LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                {
                    VAL d5 = pop_value();
                    {
                        VAL d6 = pop_value();
                        push_value(var_f);
                        incref(var_f);
                        mw_map_2B_();
                        push_value(d6);
                    }
                    mw_swap();
                    {
                        VAL d6 = pop_value();
                        push_value(var_f);
                        incref(var_f);
                        mw_map_2B_();
                        push_value(d6);
                    }
                    mw_swap();
                    push_value(d5);
                }
                mw_LCAT_2B_();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_for (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_id();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 2LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 3LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                {
                    VAL d5 = pop_value();
                    {
                        VAL d6 = pop_value();
                        push_value(var_f);
                        incref(var_f);
                        mw_prim_run();
                        push_value(d6);
                    }
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 4LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_drop();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_for_2B_();
                    push_value(d5);
                }
                push_value(var_f);
                incref(var_f);
                mw_for_2B_();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_for_2B_ (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 2LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                {
                    VAL d5 = pop_value();
                    {
                        VAL d6 = pop_value();
                        push_value(var_f);
                        incref(var_f);
                        mw_prim_run();
                        push_value(d6);
                    }
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 3LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_drop();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_for_2B_();
                    push_value(d5);
                }
                push_value(var_f);
                incref(var_f);
                mw_for_2B_();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_reverse_for (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_id();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 2LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_swap();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 3LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_rotr();
                push_value(var_f);
                incref(var_f);
                mw_dip2();
                mw_swap();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 4LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_drop();
                mw_swap();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_reverse_for_2B_();
                    push_value(d5);
                }
                push_value(var_f);
                incref(var_f);
                mw_reverse_for_2B_();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_reverse_for_2B_ (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_swap();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 2LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_rotr();
                push_value(var_f);
                incref(var_f);
                mw_dip2();
                mw_swap();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            case 3LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_drop();
                mw_swap();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_reverse_for_2B_();
                    push_value(d5);
                }
                push_value(var_f);
                incref(var_f);
                mw_reverse_for_2B_();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_reduce (void){
    {
        VAL var_g = pop_value();
        mw_List__3E_List_2B_();
        push_u64(0);
        push_value(var_g);
        incref(var_g);
        mw_prim_pack_cons();
        push_fnptr(&mb_reduce_2);
        mw_prim_pack_cons();
        mw_maybe_map();
        decref(var_g);
    }
}

static void mw_reduce_2B_ (void){
    {
        VAL var_g = pop_value();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_id();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                push_value(var_g);
                incref(var_g);
                mw_prim_run();
                break;
            case 2LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                push_value(var_g);
                incref(var_g);
                mw_prim_run();
                push_value(var_g);
                incref(var_g);
                mw_prim_run();
                break;
            case 3LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_drop();
                {
                    VAL d5 = pop_value();
                    push_value(var_g);
                    incref(var_g);
                    mw_reduce_2B_();
                    push_value(d5);
                }
                push_value(var_g);
                incref(var_g);
                mw_reduce_2B_();
                push_value(var_g);
                incref(var_g);
                mw_prim_run();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_g);
    }
}

static void mw_filter (void){
    {
        VAL var_f = pop_value();
        mw_List__3E_List_2B_();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_L0();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_filter_2B_();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_filter_2B_ (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 3LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_drop();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_filter_2B_();
                    push_value(d5);
                }
                push_u64(0);
                push_value(var_f);
                incref(var_f);
                mw_prim_pack_cons();
                push_fnptr(&mb_filter_2B__5);
                mw_prim_pack_cons();
                mw_dip_27_();
                mw_cat();
                break;
            default:
                mw_uncons();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                mw_swap();
                if (pop_u64()) {
                    push_u64(0);
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_pack_cons();
                    push_fnptr(&mb_filter_2B__10);
                    mw_prim_pack_cons();
                    mw_dip_27_();
                    mw_cons();
                } else {
                    mw_nip();
                    push_value(var_f);
                    incref(var_f);
                    mw_filter();
                }
                break;
        
}        decref(var_f);
    }
}

static void mw_find (void){
    {
        VAL var_f = pop_value();
        mw_List__3E_List_2B_();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_NONE();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                push_value(var_f);
                incref(var_f);
                mw_find_2B_();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_find_2B_ (void){
    {
        VAL var_f = pop_value();
        switch (get_top_data_tag()) {
            case 3LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_prim_pack_uncons(); mw_prim_swap();
                mw_drop();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_find_2B_();
                    push_value(d5);
                }
                mw_swap();
                switch (get_top_data_tag()) {
                    case 1LL:
                        mw_prim_pack_uncons(); mw_prim_drop();
                        mw_nip();
                        mw_SOME();
                        break;
                    case 0LL:
                        mw_prim_drop();
                        push_value(var_f);
                        incref(var_f);
                        mw_find_2B_();
                        break;
                    default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
                
}                break;
            default:
                mw_uncons();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    push_value(d5);
                }
                mw_swap();
                if (pop_u64()) {
                    mw_drop();
                    mw_SOME();
                } else {
                    mw_nip();
                    push_value(var_f);
                    incref(var_f);
                    mw_find();
                }
                break;
        
}        decref(var_f);
    }
}

static void mw_find_3F_ (void){
    {
        VAL var_f = pop_value();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_find_3F__2);
        mw_prim_pack_cons();
        mw_sip();
        mw_swap();
        decref(var_f);
    }
}

static void mw_reverse_find (void){
    {
        VAL var_f = pop_value();
        mw_reverse();
        push_value(var_f);
        incref(var_f);
        mw_find();
        decref(var_f);
    }
}

static void mw_reverse_find_3F_ (void){
    {
        VAL var_f = pop_value();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_reverse_find_3F__2);
        mw_prim_pack_cons();
        mw_sip();
        mw_swap();
        decref(var_f);
    }
}

static void mw_any (void){
    {
        VAL var_f = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_find();
        mw_is_some();
        decref(var_f);
    }
}

static void mw_any_3F_ (void){
    {
        VAL var_f = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_find_3F_();
        mw_is_some();
        decref(var_f);
    }
}

static void mw_all (void){
    {
        VAL var_f = pop_value();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_all_2);
        mw_prim_pack_cons();
        mw_find();
        mw_is_none();
        decref(var_f);
    }
}

static void mw_all_3F_ (void){
    {
        VAL var_f = pop_value();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_all_3F__2);
        mw_prim_pack_cons();
        mw_find_3F_();
        mw_is_none();
        decref(var_f);
    }
}

static void mw_collect (void){
    {
        VAL var_f = pop_value();
        mw_L0();
        mw_swap();
        push_value(var_f);
        incref(var_f);
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_collect_3);
        mw_prim_pack_cons();
        mw_while_some();
        mw_drop();
        decref(var_f);
    }
}

static void mw_collect_while (void){
    {
        VAL var_g = pop_value();
        VAL var_f = pop_value();
        mw_L0();
        while(1) {
            {
                VAL d4 = pop_value();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                push_value(d4);
            }
            mw_swap();
            if (!pop_u64()) break;
            {
                VAL d4 = pop_value();
                push_value(var_g);
                incref(var_g);
                mw_prim_run();
                push_value(d4);
            }
            mw_swap();
            mw_snoc();
        }
        decref(var_g);
        decref(var_f);
    }
}

static void mw_char_bytes (void){
    mw_char_width_3F_();
    mw_dup();
    push_i64(1LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_Char__3E_Int();
        mw_Int__3E_U8();
        mw_L1();
    } else {
        mw_dup();
        push_i64(2LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            mw_Char__3E_Int();
            mw_dup();
            push_i64(8LL);
            mw__3E__3E_();
            mw_Int__3E_U8();
            {
                VAL d4 = pop_value();
                push_i64(255LL);
                mw__26_();
                mw_Int__3E_U8();
                push_value(d4);
            }
            mw_L2();
        } else {
            push_i64(3LL);
            mw__3D__3D_();
            if (pop_u64()) {
                mw_Char__3E_Int();
                mw_dup();
                push_i64(16LL);
                mw__3E__3E_();
                mw_Int__3E_U8();
                {
                    VAL d5 = pop_value();
                    mw_dup();
                    push_i64(8LL);
                    mw__3E__3E_();
                    push_i64(255LL);
                    mw__26_();
                    mw_Int__3E_U8();
                    {
                        VAL d6 = pop_value();
                        push_i64(255LL);
                        mw__26_();
                        mw_Int__3E_U8();
                        push_value(d6);
                    }
                    push_value(d5);
                }
                mw_L3();
            } else {
                mw_Char__3E_Int();
                mw_dup();
                push_i64(24LL);
                mw__3E__3E_();
                mw_Int__3E_U8();
                {
                    VAL d5 = pop_value();
                    mw_dup();
                    push_i64(16LL);
                    mw__3E__3E_();
                    push_i64(255LL);
                    mw__26_();
                    mw_Int__3E_U8();
                    {
                        VAL d6 = pop_value();
                        mw_dup();
                        push_i64(8LL);
                        mw__3E__3E_();
                        push_i64(255LL);
                        mw__26_();
                        mw_Int__3E_U8();
                        {
                            VAL d7 = pop_value();
                            push_i64(255LL);
                            mw__26_();
                            mw_Int__3E_U8();
                            push_value(d7);
                        }
                        push_value(d6);
                    }
                    push_value(d5);
                }
                mw_L4();
            }
        }
    }
}

static void mw_char_valid_wobbly (void){
    mw_char_width_3F_();
    mw_dup();
    push_i64(1LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_char_valid_1();
    } else {
        mw_dup();
        push_i64(2LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            mw_char_valid_2();
        } else {
            mw_dup();
            push_i64(3LL);
            mw__3D__3D_();
            if (pop_u64()) {
                mw_drop();
                mw_char_valid_3_wobbly();
            } else {
                mw_drop();
                mw_char_valid_4();
            }
        }
    }
}

static void mw_char_valid_3_wobbly (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(4290822384LL);
    mw__26_();
    push_i64(8421600LL);
    mw__3D__3D_();
    mw_swap();
    mw_char_codepoint_3();
    push_i64(2048LL);
    push_i64(65535LL);
    mw_in_range();
    mw__26__26_();
}

static void mw_char_valid_3F_ (void){
    mw_dup();
    mw_char_valid();
}

static void mw_char_valid (void){
    mw_char_width_3F_();
    mw_dup();
    push_i64(1LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_char_valid_1();
    } else {
        mw_dup();
        push_i64(2LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            mw_char_valid_2();
        } else {
            mw_dup();
            push_i64(3LL);
            mw__3D__3D_();
            if (pop_u64()) {
                mw_drop();
                mw_char_valid_3();
            } else {
                mw_drop();
                mw_char_valid_4();
            }
        }
    }
}

static void mw_char_valid_1 (void){
    mw_Char__3E_Int();
    push_i64(4294967168LL);
    mw__26_();
    push_i64(0LL);
    mw__3D__3D_();
}

static void mw_char_valid_2 (void){
    mw_Char__3E_Int();
    mw_dup();
    push_i64(4294951136LL);
    mw__26_();
    push_i64(32960LL);
    mw__3D__3D_();
    mw_swap();
    push_i64(31LL);
    mw__26_();
    push_i64(2LL);
    mw__3E__3D_();
    mw__26__26_();
}

static void mw_char_valid_3 (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(4290822384LL);
    mw__26_();
    push_i64(8421600LL);
    mw__3D__3D_();
    mw_swap();
    mw_char_codepoint_3();
    mw_dup();
    push_i64(2048LL);
    push_i64(65535LL);
    mw_in_range();
    mw_swap();
    push_i64(55296LL);
    push_i64(57343LL);
    mw_in_range();
    mw_not();
    mw__26__26_();
    mw__26__26_();
}

static void mw_char_valid_4 (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(3233857784LL);
    mw__26_();
    push_i64(2155905264LL);
    mw__3D__3D_();
    mw_swap();
    mw_char_codepoint_4();
    push_i64(65536LL);
    push_i64(1114111LL);
    mw_in_range();
    mw__26__26_();
}

static void mw_char_codepoint (void){
    mw_char_width_3F_();
    mw_dup();
    push_i64(1LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_char_codepoint_1();
    } else {
        mw_dup();
        push_i64(2LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            mw_char_codepoint_2();
        } else {
            mw_dup();
            push_i64(3LL);
            mw__3D__3D_();
            if (pop_u64()) {
                mw_drop();
                mw_char_codepoint_3();
            } else {
                mw_drop();
                mw_char_codepoint_4();
            }
        }
    }
}

static void mw_char_codepoint_1 (void){
    mw_Char__3E_Int();
}

static void mw_char_codepoint_2 (void){
    mw_Char__3E_Int();
    mw_dup();
    push_i64(16128LL);
    mw__26_();
    push_i64(8LL);
    mw__3E__3E_();
    {
        VAL d2 = pop_value();
        push_i64(31LL);
        mw__26_();
        push_i64(6LL);
        mw__3C__3C_();
        push_value(d2);
    }
    mw__7C_();
}

static void mw_char_codepoint_3 (void){
    mw_Char__3E_Int();
    mw_dup();
    push_i64(4128768LL);
    mw__26_();
    push_i64(16LL);
    mw__3E__3E_();
    {
        VAL d2 = pop_value();
        mw_dup();
        push_i64(16128LL);
        mw__26_();
        push_i64(2LL);
        mw__3E__3E_();
        {
            VAL d3 = pop_value();
            push_i64(15LL);
            mw__26_();
            push_i64(12LL);
            mw__3C__3C_();
            push_value(d3);
        }
        push_value(d2);
    }
    mw__7C_();
    mw__7C_();
}

static void mw_char_codepoint_4 (void){
    mw_Char__3E_Int();
    mw_dup();
    push_i64(1056964608LL);
    mw__26_();
    push_i64(24LL);
    mw__3E__3E_();
    {
        VAL d2 = pop_value();
        mw_dup();
        push_i64(4128768LL);
        mw__26_();
        push_i64(10LL);
        mw__3E__3E_();
        {
            VAL d3 = pop_value();
            mw_dup();
            push_i64(16128LL);
            mw__26_();
            push_i64(4LL);
            mw__3C__3C_();
            {
                VAL d4 = pop_value();
                push_i64(7LL);
                mw__26_();
                push_i64(18LL);
                mw__3C__3C_();
                push_value(d4);
            }
            push_value(d3);
        }
        push_value(d2);
    }
    mw__7C_();
    mw__7C_();
    mw__7C_();
}

static void mw_char_21_ (void){
    {
        VAL d2 = pop_value();
        mw_Char__3E_Int();
        mw_Int__3E_U32();
        push_value(d2);
    }
    mw_u32_21_();
}

static void mw_char_21__precise (void){
    {
        VAL d2 = pop_value();
        mw_char_width_3F_();
        push_value(d2);
    }
    mw_swap();
    mw_dup();
    push_i64(1LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        {
            VAL d3 = pop_value();
            mw_Char__3E_Int();
            mw_Int__3E_U8();
            push_value(d3);
        }
        mw_u8_21_();
    } else {
        mw_dup();
        push_i64(2LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            {
                VAL d4 = pop_value();
                mw_Char__3E_Int();
                mw_Int__3E_U16();
                push_value(d4);
            }
            mw_u16_21_();
        } else {
            mw_dup();
            push_i64(3LL);
            mw__3D__3D_();
            if (pop_u64()) {
                mw_drop();
                {
                    VAL d5 = pop_value();
                    mw_Char__3E_Int();
                    mw_dup();
                    push_i64(65535LL);
                    mw__26_();
                    mw_Int__3E_U16();
                    push_value(d5);
                }
                mw_dup();
                {
                    VAL d5 = pop_value();
                    mw_u16_21_();
                    push_i64(16LL);
                    mw__3E__3E_();
                    mw_Int__3E_U8();
                    push_i64(2LL);
                    push_value(d5);
                }
                push_u64(0);
                push_fnptr(&mb_char_21__precise_11);
                mw_prim_pack_cons();
                mw_with_ptr_2B_();
            } else {
                mw_drop();
                {
                    VAL d5 = pop_value();
                    mw_Char__3E_Int();
                    mw_Int__3E_U32();
                    push_value(d5);
                }
                mw_u32_21_();
            }
        }
    }
}

static void mw_char_21__2B__2B_ (void){
    mw_dup2();
    mw_char_21_();
    {
        VAL d2 = pop_value();
        mw_char_width();
        push_value(d2);
    }
    mw_ptr_2B_();
}

static void mw_char_3F__2B__2B_ (void){
    mw_char_3F_();
    mw_char_width_3F_();
    mw_rotr();
    {
        VAL d2 = pop_value();
        mw_ptr_2B_();
        push_value(d2);
    }
}

static void mw_char_3F_ (void){
    mw_dup();
    mw_char_40_();
}

static void mw_char_40_ (void){
    mw_u32_40_();
    mw_U32__3E_Int();
    push_u64(0);
    push_fnptr(&mb_char_40__1);
    mw_prim_pack_cons();
    mw_sip();
    push_i64(248LL);
    mw__26_();
    push_i64(2LL);
    mw__3E__3E_();
    mw__3E__3E_();
    push_i64(3LL);
    mw__26_();
    {
        VAL d2 = pop_value();
        push_i64(4294967295LL);
        push_value(d2);
    }
    push_i64(3LL);
    mw__3C__3C_();
    mw__3E__3E_();
    mw__26_();
    mw_Int__3E_Char();
}

static void mw_char_40__width (void){
    mw_u8_40_();
    mw_U8__3E_Int();
    mw_Int__3E_Char();
    mw_char_width();
}

static void mw_char_width (void){
    mw_Char__3E_Int();
    {
        VAL d2 = pop_value();
        push_i64(4203265827220226048LL);
        push_value(d2);
    }
    push_i64(248LL);
    mw__26_();
    push_i64(2LL);
    mw__3E__3E_();
    mw__3E__3E_();
    push_i64(3LL);
    mw__26_();
    push_i64(1LL);
    mw__2B_();
}

static void mw_char_width_3F_ (void){
    mw_dup();
    mw_char_width();
}

static void mw_is_nul_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(0LL);
    mw__3D__3D_();
}

static void mw_is_tab_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(9LL);
    mw__3D__3D_();
}

static void mw_is_newline_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(10LL);
    mw__3D__3D_();
}

static void mw_is_vtab_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(11LL);
    mw__3D__3D_();
}

static void mw_is_return_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(13LL);
    mw__3D__3D_();
}

static void mw_is_space_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(32LL);
    mw__3D__3D_();
}

static void mw_is_quote_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(34LL);
    mw__3D__3D_();
}

static void mw_is_pound_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(35LL);
    mw__3D__3D_();
}

static void mw_is_lparen_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(40LL);
    mw__3D__3D_();
}

static void mw_is_rparen_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(41LL);
    mw__3D__3D_();
}

static void mw_is_asterisk_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(42LL);
    mw__3D__3D_();
}

static void mw_is_plus_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(43LL);
    mw__3D__3D_();
}

static void mw_is_comma_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(44LL);
    mw__3D__3D_();
}

static void mw_is_minus_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(45LL);
    mw__3D__3D_();
}

static void mw_is_dash_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(45LL);
    mw__3D__3D_();
}

static void mw_is_dot_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(46LL);
    mw__3D__3D_();
}

static void mw_is_digit_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(48LL);
    push_i64(57LL);
    mw_in_range();
}

static void mw_is_colon_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(58LL);
    mw__3D__3D_();
}

static void mw_is_semicolon_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(59LL);
    mw__3D__3D_();
}

static void mw_is_lt_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(60LL);
    mw__3D__3D_();
}

static void mw_is_eq_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(61LL);
    mw__3D__3D_();
}

static void mw_is_gt_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(62LL);
    mw__3D__3D_();
}

static void mw_is_question_mark_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(63LL);
    mw__3D__3D_();
}

static void mw_is_upper_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(65LL);
    push_i64(90LL);
    mw_in_range();
}

static void mw_is_upper_hexdigit_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(65LL);
    push_i64(70LL);
    mw_in_range();
}

static void mw_is_lsquare_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(91LL);
    mw__3D__3D_();
}

static void mw_is_backslash_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(92LL);
    mw__3D__3D_();
}

static void mw_is_rsquare_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(93LL);
    mw__3D__3D_();
}

static void mw_is_underscore_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(95LL);
    mw__3D__3D_();
}

static void mw_is_lower_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(97LL);
    push_i64(122LL);
    mw_in_range();
}

static void mw_is_lower_hexdigit_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(97LL);
    push_i64(102LL);
    mw_in_range();
}

static void mw_is_n_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(110LL);
    mw__3D__3D_();
}

static void mw_is_r_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(114LL);
    mw__3D__3D_();
}

static void mw_is_t_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(116LL);
    mw__3D__3D_();
}

static void mw_is_lcurly_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(123LL);
    mw__3D__3D_();
}

static void mw_is_pipe_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(124LL);
    mw__3D__3D_();
}

static void mw_is_rcurly_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(125LL);
    mw__3D__3D_();
}

static void mw_is_whitespace_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    mw_dup();
    push_i64(33LL);
    mw__3C_();
    {
        VAL d2 = pop_value();
        {
            VAL d3 = pop_value();
            push_i64(4295101952LL);
            push_value(d3);
        }
        mw__3E__3E_();
        push_i64(1LL);
        mw__26_();
        push_i64(0LL);
        mw__3C__3E_();
        push_value(d2);
    }
    mw__26__26_();
}

static void mw_is_hexdigit_3F_ (void){
    mw_is_digit_3F_();
    {
        VAL d2 = pop_value();
        mw_is_upper_hexdigit_3F_();
        push_value(d2);
    }
    mw__7C__7C_();
    {
        VAL d2 = pop_value();
        mw_is_lower_hexdigit_3F_();
        push_value(d2);
    }
    mw__7C__7C_();
}

static void mw_is_sign_3F_ (void){
    mw_is_plus_3F_();
    {
        VAL d2 = pop_value();
        mw_is_minus_3F_();
        push_value(d2);
    }
    mw__7C__7C_();
}

static void mw_is_alpha_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(4294967263LL);
    mw__26_();
    push_i64(65LL);
    push_i64(90LL);
    mw_in_range();
}

static void mw_is_string_end_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    mw_dup();
    push_i64(64LL);
    mw__3C_();
    push_u64(0);
    push_fnptr(&mb_is_string_end_3F__1);
    mw_prim_pack_cons();
    mw_and();
    mw_nip();
}

static void mw_underscore (void){
    push_i64(95LL);
    mw_Int__3E_Char();
}

static void mw_is_visible_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    push_i64(32LL);
    mw__3E_();
}

static void mw_is_name_char_3F_ (void){
    mw_is_visible_3F_();
    {
        VAL d2 = pop_value();
        mw_is_special_char_3F_();
        mw_not();
        push_value(d2);
    }
    mw__26__26_();
}

static void mw_is_special_char_3F_ (void){
    mw_dup();
    mw_Char__3E_Int();
    mw_dup();
    push_i64(64LL);
    mw__3C_();
    if (pop_u64()) {
        push_i64(288251318412247040LL);
        mw_swap();
        mw__3E__3E_();
    } else {
        push_i64(64LL);
        mw__();
        mw_dup();
        push_i64(64LL);
        mw__3C_();
        if (pop_u64()) {
            push_i64(2882303762188206080LL);
            mw_swap();
            mw__3E__3E_();
        } else {
            mw_drop();
            push_i64(0LL);
        }
    }
    push_i64(1LL);
    mw__26_();
    push_i64(0LL);
    mw__3C__3E_();
}

static void mw_str_head (void){
    mw_Str__3E_Ptr();
    mw_char_40_();
}

static void mw_str_head_width (void){
    mw_Str__3E_Ptr();
    mw_char_40__width();
}

static void mw_str_tail (void){
    push_u64(0);
    push_fnptr(&mb_str_tail_1);
    mw_prim_pack_cons();
    mw_sip();
    mw_Str__3E_Ptr();
    mw_ptr_2B_();
    mw_Ptr__3E_Str();
}

static void mw_str_size_3F_ (void){
    mw_prim_str_size();
}

static void mw_str_size (void){
    mw_prim_str_size();
    mw_nip();
}

static void mw_str_length (void){
    push_i64(0LL);
    mw_swap();
    push_u64(0);
    push_fnptr(&mb_str_length_1);
    mw_prim_pack_cons();
    mw_str_for();
}

static void mw_str_length_3F_ (void){
    mw_dup();
    mw_str_length();
}

static void mw_str_concat (void){
    mw_dup();
    push_i64(0LL);
    mw_swap();
    push_u64(0);
    push_fnptr(&mb_str_concat_1);
    mw_prim_pack_cons();
    mw_for();
    mw_prim_str_alloc();
    push_u64(0);
    push_fnptr(&mb_str_concat_2);
    mw_prim_pack_cons();
    mw_sip();
}

static void mw_str_cat (void){
    mw_L2();
    mw_str_concat();
}

static void mw_str_is_empty (void){
    mw_is_nil_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_true();
    } else {
        mw_str_head();
        mw_is_nil();
    }
}

static void mw_str_is_empty_3F_ (void){
    mw_dup();
    mw_str_is_empty();
}

static void mw_str_copy_partial_21_ (void){
    {
        VAL d2 = pop_value();
        mw_dup();
        mw_Str__3E_Ptr();
        mw_swap();
        mw_str_size();
        push_value(d2);
    }
    mw_dup2();
    mw_ptr_2B_();
    {
        VAL d2 = pop_value();
        mw_prim_ptr_copy();
        push_value(d2);
    }
}

static void mw_STR_BUF_SIZE (void){
    push_i64(8192LL);
}

static void mw_build_str_21_ (void){
    {
        VAL var_f = pop_value();
        mw_str_buf_dup_21_();
        mw_str_buf_clear_21_();
        {
            VAL d3 = pop_value();
            push_value(var_f);
            incref(var_f);
            mw_prim_run();
            mw_str_buf_dup_21_();
            push_value(d3);
        }
        mw_str_buf_21_();
        decref(var_f);
    }
}

static void mw_str_buf_dup_21_ (void){
    mw_STR_BUF();
    mw_str_buf_length_3F_();
    mw_dup();
    mw_prim_str_alloc();
    push_u64(0);
    push_fnptr(&mb_str_buf_dup_21__1);
    mw_prim_pack_cons();
    mw_sip();
}

static void mw_str_buf_length_3F_ (void){
    mw_STR_BUF_LEN();
    mw__40_();
}

static void mw_str_buf_length_21_ (void){
    mw_dup();
    mw_STR_BUF_LEN();
    mw__21_();
    {
        VAL d2 = pop_value();
        push_i64(0LL);
        mw_Int__3E_U8();
        push_value(d2);
    }
    mw_str_buf_u8_21_();
}

static void mw_str_buf_u8_40_ (void){
    mw_STR_BUF();
    mw_u8_40__40_();
}

static void mw_str_buf_u8_21_ (void){
    mw_STR_BUF();
    mw_u8_21__21_();
}

static void mw_str_buf_char_40_ (void){
    mw_STR_BUF();
    push_u64(0);
    push_fnptr(&mb_str_buf_char_40__1);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mw_str_buf_empty_3F_ (void){
    mw_str_buf_length_3F_();
    mw_0_3D_();
}

static void mw_str_buf_full_3F_ (void){
    mw_str_buf_length_3F_();
    mw_1_2B_();
    mw_STR_BUF_SIZE();
    mw__3E__3D_();
}

static void mw_str_buf_clear_21_ (void){
    push_i64(0LL);
    mw_str_buf_length_21_();
}

static void mw_str_buf_push_u8_21_ (void){
    mw_str_buf_length_3F_();
    mw_str_buf_u8_21_();
    mw_str_buf_length_3F_();
    mw_1_2B_();
    mw_str_buf_length_21_();
}

static void mw_str_buf_push_char_21_ (void){
    mw_dup();
    mw_str_buf_length_3F_();
    mw_STR_BUF();
    push_u64(0);
    push_fnptr(&mb_str_buf_push_char_21__1);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
    mw_char_width();
    mw_str_buf_length_3F_();
    mw__2B_();
    mw_str_buf_length_21_();
}

static void mw_str_buf_push_str_21_ (void){
    mw_dup();
    mw_Str__3E_Ptr();
    mw_swap();
    mw_str_size();
    mw_str_buf_push_ptr_21_();
}

static void mw_str_buf_push_ptr_21_ (void){
    mw_tuck();
    mw_str_buf_length_3F_();
    mw_STR_BUF();
    push_u64(0);
    push_fnptr(&mb_str_buf_push_ptr_21__1);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
    mw_str_buf_length_3F_();
    mw__2B_();
    mw_str_buf_length_21_();
}

static void mw_str_buf_21_ (void){
    mw_str_buf_clear_21_();
    mw_str_buf_push_str_21_();
}

static void mw_str_buf_char_21_ (void){
    mw_dup();
    mw_STR_BUF();
    mw_char_21_();
    mw_char_width();
    mw_str_buf_length_21_();
}

static void mw_to_digit (void){
    push_i64(10LL);
    mw__25_();
    push_i64(48LL);
    mw__2B_();
    mw_Int__3E_Char();
}

static void mw_str_buf_int_21_ (void){
    mw_dup();
    mw_0_3D_();
    if (pop_u64()) {
        mw_drop();
        push_ptr("0\0\0\0\0");
        mw_str_buf_21_();
    } else {
        push_u64(0);
        push_fnptr(&mb_str_buf_int_21__3);
        mw_prim_pack_cons();
        mw_sip();
        push_i64(0LL);
        mw__3C_();
        if (pop_u64()) {
            push_i64(45LL);
            mw_Int__3E_Char();
            mw_str_buf_push_char_21_();
        } else {
            mw_id();
        }
        mw_str_buf_reverse_21_();
    }
}

static void mw_int_show (void){
    push_u64(0);
    push_fnptr(&mb_int_show_1);
    mw_prim_pack_cons();
    mw_build_str_21_();
}

static void mw_str_buf_swap_u8_21_ (void){
    mw_dup2();
    mw_swap();
    push_u64(0);
    push_fnptr(&mb_str_buf_swap_u8_21__1);
    mw_prim_pack_cons();
    mw_dip3();
    {
        VAL d2 = pop_value();
        mw_str_buf_u8_40_();
        push_value(d2);
    }
    mw_str_buf_u8_21_();
    mw_str_buf_u8_21_();
}

static void mw_str_buf_reverse_21_ (void){
    push_i64(0LL);
    mw_str_buf_length_3F_();
    mw_1_();
    while(1) {
        mw_dup2();
        mw__3C_();
        if (!pop_u64()) break;
        mw_dup2();
        mw_str_buf_swap_u8_21_();
        {
            VAL d3 = pop_value();
            mw_1_2B_();
            push_value(d3);
        }
        mw_1_();
    }
    mw_drop2();
}

static void mw_str_eq (void){
    mw_prim_str_eq();
}

static void mw_str_eq_3F_ (void){
    mw_dup2();
    mw_str_eq();
}

static void mw_str_for (void){
    {
        VAL var_f = pop_value();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_str_for_2);
        mw_prim_pack_cons();
        mw_sip();
        mw_drop2();
        decref(var_f);
    }
}

static void mw_str_transduce (void){
    {
        VAL var_f = pop_value();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_str_transduce_2);
        mw_prim_pack_cons();
        mw_sip();
        mw_drop();
        decref(var_f);
    }
}

static void mw_str_transduce_step (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_drop();
            push_ptr("\0\0\0\0");
            break;
        case 1LL:
            mw_prim_drop();
            mw_id();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_str_buf_push_char_21_();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_str_transduce_step_5);
            mw_prim_pack_cons();
            mw_for();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_str_buf_push_str_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_str_chars (void){
    {
        VAL d2 = pop_value();
        mw_L0();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_str_chars_2);
    mw_prim_pack_cons();
    mw_str_for();
}

static void mw_str_codepoints (void){
    {
        VAL d2 = pop_value();
        mw_L0();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_str_codepoints_2);
    mw_prim_pack_cons();
    mw_str_for();
}

static void mw_str_bytes (void){
    mw_L0();
    mw_swap();
    mw_dup();
    push_u64(0);
    push_fnptr(&mb_str_bytes_1);
    mw_prim_pack_cons();
    mw_sip();
    mw_drop3();
}

static void mw_Str__3E_Path (void){
    mw_PATH();
}

static void mw_Path__3E_Str (void){
    mw_id();
}

static void mw_path_40_ (void){
    mw_ptr_40_();
    mw_Ptr__3E_Str();
    mw_Str__3E_Path();
}

static void mw_path_21_ (void){
    {
        VAL d2 = pop_value();
        mw_Path__3E_Str();
        mw_Str__3E_Ptr();
        push_value(d2);
    }
    mw_ptr_21_();
}

static void mw_init_paths_21_ (void){
    push_ptr("src\0\0\0\0");
    mw_Str__3E_Path();
    mw_source_path_root();
    mw__21_();
    push_ptr("bin\0\0\0\0");
    mw_Str__3E_Path();
    mw_output_path_root();
    mw__21_();
}

static void mw_path_separator (void){
    mw_RUNNING_OS();
    mw_OS_WINDOWS();
    mw__3D__3D_();
    if (pop_u64()) {
        push_ptr("\\\0\0\0\0");
    } else {
        push_ptr("/\0\0\0\0");
    }
}

static void mw_path_is_empty_3F_ (void){
    mw_dup();
    mw_Path__3E_Str();
    mw_str_is_empty();
}

static void mw_path_join (void){
    mw_swap();
    mw_path_is_empty_3F_();
    if (pop_u64()) {
        mw_drop();
    } else {
        mw_Path__3E_Str();
        mw_path_separator();
        mw_rotl();
        mw_Path__3E_Str();
        mw_L3();
        mw_str_concat();
        mw_Str__3E_Path();
    }
}

static void mw_make_source_path (void){
    mw_source_path_root();
    mw__40_();
    mw_swap();
    mw_path_join();
}

static void mw_make_output_path (void){
    mw_output_path_root();
    mw__40_();
    mw_swap();
    mw_path_join();
}

static void mw_panic_21_ (void){
    push_ptr("panic: \0\0\0\0");
    mw_str_trace_21_();
    mw_str_trace_ln_21_();
    push_i64(1LL);
    mw_posix_exit_21_();
}

static void mw_Int__3E_File (void){
    mw_FILE();
}

static void mw_File__3E_Int (void){
    mw_id();
}

static void mw_file_40_ (void){
    mw_int_40_();
    mw_Int__3E_File();
}

static void mw_file_21_ (void){
    {
        VAL d2 = pop_value();
        mw_File__3E_Int();
        push_value(d2);
    }
    mw_int_21_();
}

static void mw_stdin (void){
    push_i64(0LL);
    mw_Int__3E_File();
}

static void mw_stdout (void){
    push_i64(1LL);
    mw_Int__3E_File();
}

static void mw_stderr (void){
    push_i64(2LL);
    mw_Int__3E_File();
}

static void mw_str_write_21_ (void){
    mw_File__3E_Int();
    mw_swap();
    mw_dup();
    mw_str_size();
    mw_dup();
    {
        VAL d2 = pop_value();
        {
            VAL d3 = pop_value();
            mw_Str__3E_Ptr();
            push_value(d3);
        }
        mw_posix_write_21_();
        push_value(d2);
    }
    mw_swap();
    mw_dup();
    push_i64(0LL);
    mw__3C_();
    if (pop_u64()) {
        push_ptr("error: write failed!\0\0\0\0");
        mw_panic_21_();
    } else {
        mw_swap();
        mw__3C_();
        if (pop_u64()) {
            push_ptr("error: write output fewer bytes than expected!\0\0\0\0");
            mw_panic_21_();
        } else {
            mw_id();
        }
    }
}

static void mw_str_print_21_ (void){
    mw_stdout();
    mw_str_write_21_();
}

static void mw_str_trace_21_ (void){
    mw_stderr();
    mw_str_write_21_();
}

static void mw_str_print_ln_21_ (void){
    mw_str_print_21_();
    mw_print_ln_21_();
}

static void mw_str_trace_ln_21_ (void){
    mw_str_trace_21_();
    mw_trace_ln_21_();
}

static void mw_print_ln_21_ (void){
    push_ptr("\n\0\0\0\0");
    mw_str_print_21_();
}

static void mw_trace_ln_21_ (void){
    push_ptr("\n\0\0\0\0");
    mw_str_trace_21_();
}

static void mw_str_buf_write_21_ (void){
    mw_File__3E_Int();
    mw_STR_BUF();
    mw_str_buf_length_3F_();
    mw_posix_write_21_();
    mw_dup();
    push_i64(0LL);
    mw__3C_();
    if (pop_u64()) {
        push_ptr("error: str-buf write failed!\0\0\0\0");
        mw_panic_21_();
    } else {
        mw_str_buf_length_3F_();
        mw__3C_();
        if (pop_u64()) {
            push_ptr("error: str-buf write wrote fewer bytes than expected!\0\0\0\0");
            mw_panic_21_();
        } else {
            mw_id();
        }
    }
}

static void mw_str_buf_print_21_ (void){
    mw_stdout();
    mw_str_buf_write_21_();
}

static void mw_str_buf_trace_21_ (void){
    mw_stderr();
    mw_str_buf_write_21_();
}

static void mw_str_buf_read_21_ (void){
    mw_File__3E_Int();
    mw_str_buf_clear_21_();
    mw_STR_BUF();
    mw_STR_BUF_SIZE();
    mw_1_();
    mw_posix_read_21_();
    mw_dup();
    mw_0_3C_();
    if (pop_u64()) {
        push_ptr("str-buf-read! failed\0\0\0\0");
        mw_panic_21_();
    } else {
        mw_str_buf_length_21_();
    }
}

static void mw_str_buf_input_21_ (void){
    mw_stdin();
    mw_str_buf_read_21_();
}

static void mw_print_char_21_ (void){
    push_u64(0);
    push_fnptr(&mb_print_char_21__1);
    mw_prim_pack_cons();
    mw_build_str_21_();
    mw_str_print_21_();
}

static void mw_trace_char_21_ (void){
    push_u64(0);
    push_fnptr(&mb_trace_char_21__1);
    mw_prim_pack_cons();
    mw_build_str_21_();
    mw_str_trace_21_();
}

static void mw_int_write_21_ (void){
    {
        VAL d2 = pop_value();
        mw_str_buf_int_21_();
        push_value(d2);
    }
    mw_str_buf_write_21_();
}

static void mw_int_print_21_ (void){
    mw_stdout();
    mw_int_write_21_();
}

static void mw_int_trace_21_ (void){
    mw_stderr();
    mw_int_write_21_();
}

static void mw_int_print_ln_21_ (void){
    mw_int_print_21_();
    mw_print_ln_21_();
}

static void mw_int_trace_ln_21_ (void){
    mw_int_trace_21_();
    mw_trace_ln_21_();
}

static void mw_with_open_file_21_ (void){
    {
        VAL var_g = pop_value();
        VAL var_f = pop_value();
        push_i64(0LL);
        push_i64(0LL);
        mw_posix_open_21_();
        mw_dup();
        push_i64(0LL);
        mw__3C_();
        if (pop_u64()) {
            mw_drop();
            push_value(var_g);
            incref(var_g);
            mw_prim_run();
        } else {
            mw_dup();
            {
                VAL d4 = pop_value();
                mw_Int__3E_File();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                push_value(d4);
            }
            mw_posix_close_21_();
            mw_drop();
        }
        decref(var_g);
        decref(var_f);
    }
}

static void mw_read_file_21_ (void){
    mw_File__3E_Int();
    {
        VAL var_fp = pop_value();
        push_i64(0LL);
        push_i64(4096LL);
        mw_prim_ptr_alloc();
        push_value(var_fp);
        incref(var_fp);
        mw_over();
        push_i64(4096LL);
        mw_prim_posix_read();
        while(1) {
            mw_dup();
            push_i64(0LL);
            mw__3E_();
            if (!pop_u64()) break;
            mw_swap();
            {
                VAL d4 = pop_value();
                mw__2B_();
                mw_dup();
                push_value(d4);
            }
            mw_swap();
            push_i64(2LL);
            mw__2A_();
            mw_prim_ptr_realloc();
            mw_dup2();
            push_u64(0);
            push_value(var_fp);
            incref(var_fp);
            mw_prim_pack_cons();
            push_fnptr(&mb_read_file_21__5);
            mw_prim_pack_cons();
            mw_with_ptr_2B_();
        }
        push_i64(0LL);
        mw__3C_();
        if (pop_u64()) {
            push_ptr("io error while reading file\0\0\0\0");
            mw_panic_21_();
        } else {
            mw_id();
        }
        mw_over();
        push_i64(4LL);
        mw__2B_();
        mw_prim_ptr_realloc();
        mw_tuck();
        push_u64(0);
        push_value(var_fp);
        incref(var_fp);
        mw_prim_pack_cons();
        push_fnptr(&mb_read_file_21__9);
        mw_prim_pack_cons();
        mw_with_ptr_2B_();
        mw_Ptr__3E_Str();
        decref(var_fp);
    }
}

static void mw_open_file_21_ (void){
    push_i64(0LL);
    push_i64(0LL);
    mw_posix_open_21_();
    mw_dup();
    push_i64(0LL);
    mw__3C_();
    if (pop_u64()) {
        push_ptr("Failed to open file!\0\0\0\0");
        mw_panic_21_();
    } else {
        mw_Int__3E_File();
    }
}

static void mw_create_file_21_ (void){
    mw_O_WRONLY_7C_O_CREAT_7C_O_TRUNC();
    push_i64(438LL);
    mw_posix_open_21_();
    mw_dup();
    push_i64(0LL);
    mw__3C_();
    if (pop_u64()) {
        push_ptr("Failed to create file!\0\0\0\0");
        mw_panic_21_();
    } else {
        mw_Int__3E_File();
    }
}

static void mw_O_WRONLY_7C_O_CREAT_7C_O_TRUNC (void){
    mw_RUNNING_OS();
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_drop();
            push_i64(1537LL);
            break;
        case 2LL:
            mw_prim_drop();
            push_i64(577LL);
            break;
        case 1LL:
            mw_prim_drop();
            push_i64(769LL);
            break;
        case 0LL:
            mw_prim_drop();
            push_ptr("O_WRONLY|O_CREAT|O_TRUNC on unknown os\0\0\0\0");
            mw_panic_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_close_file_21_ (void){
    mw_File__3E_Int();
    mw_posix_close_21_();
    push_i64(0LL);
    mw__3C_();
    if (pop_u64()) {
        push_ptr("failed to close file.\0\0\0\0");
        mw_panic_21_();
    } else {
        mw_id();
    }
}

static void mw_with_raw_path (void){
    {
        VAL var_f = pop_value();
        mw_Path__3E_Str();
        mw_Str__3E_Ptr();
        push_value(var_f);
        incref(var_f);
        mw_with_raw_ptr();
        mw_Ptr__3E_Str();
        mw_Str__3E_Path();
        decref(var_f);
    }
}

static void mw_is_directory_3F_ (void){
    push_u64(0);
    push_fnptr(&mb_is_directory_3F__1);
    mw_prim_pack_cons();
    mw_with_raw_path();
    mw_swap();
}

static void mw_S_IFMT (void){
    push_i64(61440LL);
}

static void mw_S_IFDIR (void){
    push_i64(16384LL);
}

static void mw_S_IFREG (void){
    push_i64(32768LL);
}

static void mw_S_ISDIR (void){
    mw_U16__3E_Int();
    mw_S_IFMT();
    mw__26_();
    mw_S_IFDIR();
    mw__3D__3D_();
}

static void mw_st_mode_40_ (void){
    mw_RUNNING_OS();
    switch (get_top_data_tag()) {
        case 2LL:
            mw_prim_drop();
            push_i64(24LL);
            break;
        case 1LL:
            mw_prim_drop();
            push_i64(6LL);
            break;
        default:
            mw_drop();
            push_i64(8LL);
            break;
    
}    mw_swap();
    push_u64(0);
    push_fnptr(&mb_st_mode_40__4);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mw_INPUT_BUFFER_SIZE (void){
    push_i64(8192LL);
}

static void mw_input_start_21_ (void){
    mw_input_handle();
    mw__21_();
    push_i64(0LL);
    mw_input_length();
    mw__21_();
    push_i64(0LL);
    mw_input_offset();
    mw__21_();
    mw_true();
    mw_input_isopen();
    mw__21_();
    mw_input_fill_buffer_21_();
}

static void mw_input_end_21_ (void){
    mw_input_isopen();
    mw__40_();
    if (pop_u64()) {
        mw_input_handle();
        mw__40_();
        mw_close_file_21_();
    } else {
        mw_id();
    }
    mw_stdin();
    mw_input_handle();
    mw__21_();
    push_i64(0LL);
    mw_input_length();
    mw__21_();
    push_i64(0LL);
    mw_input_offset();
    mw__21_();
    mw_false();
    mw_input_isopen();
    mw__21_();
}

static void mw_input_done_3F_ (void){
    mw_input_isopen();
    mw__40_();
    mw_not();
}

static void mw_input_fill_buffer_21_ (void){
    mw_input_isopen();
    mw__40_();
    if (pop_u64()) {
        mw_input_handle();
        mw__40_();
        mw_File__3E_Int();
        mw_INPUT_BUFFER();
        mw_INPUT_BUFFER_SIZE();
        mw_posix_read_21_();
        mw_dup();
        push_i64(0LL);
        mw__3E__3D_();
        if (pop_u64()) {
            mw_dup();
            push_i64(0LL);
            mw__3E_();
            if (pop_u64()) {
                mw_input_length();
                mw__21_();
                push_i64(0LL);
                mw_input_offset();
                mw__21_();
            } else {
                mw_drop();
                mw_input_end_21_();
            }
        } else {
            mw_drop();
            push_ptr("error: failed to read from file\0\0\0\0");
            mw_panic_21_();
        }
    } else {
        push_ptr("error: attempted to fill input buffer when file is closed\0\0\0\0");
        mw_panic_21_();
    }
}

static void mw_input_peek (void){
    mw_input_isopen();
    mw__40_();
    if (pop_u64()) {
        mw_input_offset();
        mw__40_();
        mw_INPUT_BUFFER();
        push_u64(0);
        push_fnptr(&mb_input_peek_2);
        mw_prim_pack_cons();
        mw_with_ptr_2B_();
    } else {
        push_ptr("error: attempted to read input buffer when file is already closed\0\0\0\0");
        mw_panic_21_();
    }
}

static void mw_input_move_21_ (void){
    mw_input_isopen();
    mw__40_();
    if (pop_u64()) {
        mw_input_offset();
        mw__40_();
        mw_dup();
        mw_INPUT_BUFFER();
        push_u64(0);
        push_fnptr(&mb_input_move_21__2);
        mw_prim_pack_cons();
        mw_with_ptr_2B_();
    } else {
        push_ptr("error: attempted to move input buffer when file is already closed\0\0\0\0");
        mw_panic_21_();
    }
}

static void mw_input_prepare_for_more_21_ (void){
    mw_input_offset();
    mw__40_();
    mw_dup();
    push_i64(4LL);
    mw__2B_();
    mw_input_length();
    mw__40_();
    mw__3E_();
    if (pop_u64()) {
        mw_dup();
        mw_input_length();
        mw__40_();
        mw__3E__3D_();
        if (pop_u64()) {
            mw_drop();
            mw_input_fill_buffer_21_();
        } else {
            mw_dup();
            mw_INPUT_BUFFER();
            push_u64(0);
            push_fnptr(&mb_input_prepare_for_more_21__4);
            mw_prim_pack_cons();
            mw_with_ptr_2B_();
            mw__2B_();
            mw_input_length();
            mw__40_();
            mw__3E_();
            if (pop_u64()) {
                mw_input_fill_buffer_tragic_21_();
            } else {
                mw_id();
            }
        }
    } else {
        mw_drop();
    }
}

static void mw_input_fill_buffer_tragic_21_ (void){
    mw_input_offset();
    mw__40_();
    mw_INPUT_BUFFER();
    push_u64(0);
    push_fnptr(&mb_input_fill_buffer_tragic_21__1);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
    mw_INPUT_BUFFER();
    mw_u32_21_();
    mw_INPUT_BUFFER();
    mw_char_40__width();
    mw_input_length();
    mw__21_();
    push_i64(0LL);
    mw_input_offset();
    mw__21_();
    mw_input_isopen();
    mw__40_();
    if (pop_u64()) {
        mw_input_handle();
        mw__40_();
        mw_File__3E_Int();
        mw_input_length();
        mw__40_();
        mw_INPUT_BUFFER();
        push_u64(0);
        push_fnptr(&mb_input_fill_buffer_tragic_21__3);
        mw_prim_pack_cons();
        mw_with_ptr_2B_();
        mw_dup();
        push_i64(0LL);
        mw__3E__3D_();
        if (pop_u64()) {
            mw_dup();
            push_i64(0LL);
            mw__3E_();
            if (pop_u64()) {
                mw_input_length();
                push_u64(0);
                push_fnptr(&mb_input_fill_buffer_tragic_21__6);
                mw_prim_pack_cons();
                mw_modify();
            } else {
                mw_drop();
            }
        } else {
            mw_drop();
            push_ptr("error: failed to read from file\0\0\0\0");
            mw_panic_21_();
        }
    } else {
        push_ptr("error: attempted to fill input buffer when file is closed\0\0\0\0");
        mw_panic_21_();
    }
}

static void mw_stack_push_21_ (void){
    push_u64(0);
    push_fnptr(&mb_stack_push_21__1);
    mw_prim_pack_cons();
    mw_modify();
}

static void mw_stack_pop_21_ (void){
    push_u64(0);
    push_fnptr(&mb_stack_pop_21__1);
    mw_prim_pack_cons();
    mw_modify();
}

static void mw_stack_uncons (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_NONE();
            mw_STACK_NIL();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_SOME();
                push_value(d4);
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_stack_reset_21_ (void){
    mw_STACK_NIL();
    mw_swap();
    mw__21_();
}

static void mw_stack_is_empty (void){
    mw_is_nil();
}

static void mw_Name_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Name_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Name_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Name_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Name_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Name_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Name_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Name_2E_alloc_21_ (void){
    mw_Name_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Name_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_Int__3E_Hash (void){
    mw_HASH();
}

static void mw_Hash__3E_Int (void){
    mw_id();
}

static void mw_NAME_HASH_MAX (void){
    push_i64(32767LL);
}

static void mw_hash_name_21_ (void){
    mw_Hash__3E_Int();
    mw_NAME_HASH_TABLE();
    mw_value_21__21_();
}

static void mw_hash_name_40_ (void){
    mw_Hash__3E_Int();
    mw_NAME_HASH_TABLE();
    mw_value_40__40_();
}

static void mw_hash_name_3F_ (void){
    mw_dup();
    mw_hash_name_40_();
}

static void mw_hash (void){
    push_i64(0LL);
    mw_swap();
    push_u64(0);
    push_fnptr(&mb_hash_1);
    mw_prim_pack_cons();
    mw_str_for();
    mw_NAME_HASH_MAX();
    mw__26_();
    mw_Int__3E_Hash();
}

static void mw_name_hash (void){
    mw_name_str();
    mw__40_();
    mw_hash();
}

static void mw_next_hash (void){
    mw_Hash__3E_Int();
    mw_1_2B_();
    mw_NAME_HASH_MAX();
    mw__26_();
    mw_Int__3E_Hash();
}

static void mw_name_keep_going_3F_ (void){
    mw_hash_name_3F_();
    mw_is_nil_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_false();
    } else {
        {
            VAL d3 = pop_value();
            mw_over();
            push_value(d3);
        }
        mw_name_str();
        mw__40_();
        mw_str_eq();
        mw_not();
    }
}

static void mw_name_new_21_ (void){
    mw_dup();
    mw_hash();
    while(1) {
        mw_name_keep_going_3F_();
        if (!pop_u64()) break;
        mw_next_hash();
    }
    mw_hash_name_3F_();
    mw_is_nil_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_Name_2E_alloc_21_();
        mw_tuck();
        mw_swap();
        mw_hash_name_21_();
        mw_tuck();
        mw_name_str();
        mw__21_();
        mw_dup();
        push_u64(0);
        push_fnptr(&mb_name_new_21__4);
        mw_prim_pack_cons();
        mw_delay();
        mw_over();
        mw_name_mangled();
        mw__21_();
    } else {
        mw_nip();
        mw_nip();
    }
}

static void mw_name_cat_21_ (void){
    {
        VAL d2 = pop_value();
        mw_name_str();
        mw__40_();
        push_value(d2);
    }
    mw_str_cat();
    mw_name_new_21_();
}

static void mw_name_trace_21_ (void){
    mw_name_str();
    mw__40_();
    mw_str_trace_21_();
}

static void mw_name_print_21_ (void){
    mw_name_str();
    mw__40_();
    mw_str_print_21_();
}

static void mw_name_could_be_type (void){
    mw_name_str();
    mw__40_();
    mw_str_head();
    mw_is_alpha_3F_();
    mw_nip();
}

static void mw_name_could_be_type_var (void){
    mw_name_str();
    mw__40_();
    mw_str_could_be_type_var();
}

static void mw_str_could_be_type_var (void){
    mw_str_head();
    mw_is_lower_3F_();
    mw_nip();
}

static void mw_name_could_be_type_con (void){
    mw_name_str();
    mw__40_();
    mw_str_head();
    mw_is_upper_3F_();
    mw_nip();
}

static void mw_name_is_type_hole (void){
    mw_name_str();
    mw__40_();
    mw_dup();
    mw_str_head();
    mw_is_question_mark_3F_();
    mw_nip();
    if (pop_u64()) {
        mw_str_tail();
        mw_str_is_empty_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_true();
        } else {
            mw_str_could_be_type_var();
        }
    } else {
        mw_drop();
        mw_false();
    }
}

static void mw_name_is_underscore (void){
    mw_name_str();
    mw__40_();
    mw_dup();
    mw_str_head();
    mw_is_underscore_3F_();
    mw_nip();
    if (pop_u64()) {
        mw_str_tail();
        mw_str_is_empty();
    } else {
        mw_drop();
        mw_false();
    }
}

static void mw_name_could_be_stack_var (void){
    mw_name_str();
    mw__40_();
    mw_dup();
    mw_str_head();
    mw_is_asterisk_3F_();
    mw_nip();
    if (pop_u64()) {
        mw_str_tail();
        mw_str_could_be_type_var();
    } else {
        mw_drop();
        mw_false();
    }
}

static void mw_name_could_be_effect_con (void){
    mw_name_str();
    mw__40_();
    mw_dup();
    mw_str_head();
    mw_is_plus_3F_();
    mw_nip();
    if (pop_u64()) {
        mw_str_tail();
        mw_str_head();
        mw_is_upper_3F_();
        mw_nip();
    } else {
        mw_drop();
        mw_false();
    }
}

static void mw_name_print_mangled_21_ (void){
    mw_name_mangled();
    mw_force_21_();
    mw_str_print_21_();
}

static void mw_name_mangle_21_ (void){
    mw_name_mangled();
    mw_force_21_();
}

static void mw_name_mangle_compute_21_ (void){
    mw_name_str();
    mw__40_();
    push_u64(0);
    push_fnptr(&mb_name_mangle_compute_21__1);
    mw_prim_pack_cons();
    mw_str_transduce();
}

static void mw_char_hexdigits (void){
    mw_Char__3E_Int();
    mw_char_hexdigits_first();
    while(1) {
        mw_dup();
        push_i64(0LL);
        mw__3E_();
        if (!pop_u64()) break;
        mw_char_hexdigits_next();
    }
    mw_drop();
}

static void mw_char_hexdigits_first (void){
    {
        VAL d2 = pop_value();
        mw_L0();
        push_value(d2);
    }
    mw_char_hexdigits_next();
}

static void mw_char_hexdigits_next (void){
    push_u64(0);
    push_fnptr(&mb_char_hexdigits_next_1);
    mw_prim_pack_cons();
    mw_sip();
    push_i64(4LL);
    mw__3E__3E_();
    push_u64(0);
    push_fnptr(&mb_char_hexdigits_next_2);
    mw_prim_pack_cons();
    mw_sip();
    push_i64(4LL);
    mw__3E__3E_();
    {
        VAL d2 = pop_value();
        mw_swap();
        {
            VAL d3 = pop_value();
            mw_snoc();
            push_value(d3);
        }
        mw_snoc();
        push_value(d2);
    }
}

static void mw_hexdigit (void){
    mw_dup();
    push_i64(10LL);
    mw__3E__3D_();
    if (pop_u64()) {
        push_i64(55LL);
        mw__2B_();
        mw_Int__3E_Char();
    } else {
        push_i64(48LL);
        mw__2B_();
        mw_Int__3E_Char();
    }
}

static void mw_name_undefined_3F_ (void){
    mw_dup();
    mw_name_def();
    mw__40_();
    mw_is_nil();
}

static void mw_name_defined_3F_ (void){
    mw_name_undefined_3F_();
    mw_not();
}

static void mw_unSET (void){
    mw_id();
}

static void mw_set_snoc (void){
    {
        VAL d2 = pop_value();
        mw_unSET();
        push_value(d2);
    }
    mw_bag_replace();
    mw_SET();
}

static void mw_set_cons (void){
    mw_swap();
    mw_set_snoc();
}

static void mw_Module_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Module_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Module_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Module_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Module_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Module_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Module_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Module_2E_alloc_21_ (void){
    mw_Module_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Module_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_Token_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Token_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Token_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Token_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Token_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Token_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Token_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Token_2E_alloc_21_ (void){
    mw_Token_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Token_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_module_new_21_ (void){
    mw_Module_2E_alloc_21_();
    mw_tuck();
    mw_module_path();
    mw__21_();
}

static void mw_module_add_import_21_ (void){
    mw_swap();
    mw_module_imports();
    push_u64(0);
    push_fnptr(&mb_module_add_import_21__1);
    mw_prim_pack_cons();
    mw_modify();
}

static void mw_module_source_path (void){
    mw_is_nil_3F_();
    if (pop_u64()) {
        mw_drop();
        push_ptr("<generated>\0\0\0\0");
        mw_Str__3E_Path();
    } else {
        mw_module_path();
        mw__40_();
        mw_make_source_path();
    }
}

static void mw_module_path_from_name (void){
    mw_name_str();
    mw__40_();
    push_u64(0);
    push_fnptr(&mb_module_path_from_name_1);
    mw_prim_pack_cons();
    mw_str_transduce();
    push_ptr(".mth\0\0\0\0");
    mw_str_cat();
    mw_Str__3E_Path();
}

static void mw_Int__3E_Row (void){
    mw_ROW();
}

static void mw_Row__3E_Int (void){
    mw_id();
}

static void mw_Int__3E_Col (void){
    mw_COL();
}

static void mw_Col__3E_Int (void){
    mw_id();
}

static void mw_location_pack (void){
    mw_LOCATION();
}

static void mw_location_unpack (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_id();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_location_trace_21_ (void){
    mw_location_unpack();
    mw_rotr();
    mw_swap();
    mw_module_source_path();
    mw_Path__3E_Str();
    mw_str_trace_21_();
    push_ptr(":\0\0\0\0");
    mw_str_trace_21_();
    mw_Row__3E_Int();
    mw_int_trace_21_();
    push_ptr(":\0\0\0\0");
    mw_str_trace_21_();
    mw_Col__3E_Int();
    mw_int_trace_21_();
}

static void mw_emit_warning_at_21_ (void){
    {
        VAL d2 = pop_value();
        mw_location_trace_21_();
        push_value(d2);
    }
    push_ptr(": warning: \0\0\0\0");
    mw_str_trace_21_();
    mw_str_trace_ln_21_();
    mw_num_warnings();
    push_u64(0);
    push_fnptr(&mb_emit_warning_at_21__2);
    mw_prim_pack_cons();
    mw_modify();
}

static void mw_emit_error_at_21_ (void){
    {
        VAL d2 = pop_value();
        mw_location_trace_21_();
        push_value(d2);
    }
    push_ptr(": error: \0\0\0\0");
    mw_str_trace_21_();
    mw_str_trace_ln_21_();
    mw_num_errors();
    push_u64(0);
    push_fnptr(&mb_emit_error_at_21__2);
    mw_prim_pack_cons();
    mw_modify();
}

static void mw_emit_fatal_error_at_21_ (void){
    mw_emit_error_at_21_();
    push_i64(1LL);
    mw_posix_exit_21_();
}

static void mw_token_alloc_21_ (void){
    mw_Token_2E_alloc_21_();
    mw_TOKEN_NONE();
    mw_over();
    mw_token_value();
    mw__21_();
}

static void mw_token_is_int_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_str_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_name_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_comma_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 1LL:
            mw_prim_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_arrow_3F_ (void){
    mw_PRIM_SYNTAX_ARROW();
    mw_token_prim_3D__3F_();
}

static void mw_token_is_dashes_3F_ (void){
    mw_PRIM_SYNTAX_DASHES();
    mw_token_prim_3D__3F_();
}

static void mw_token_is_lparen_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_rparen_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_lsquare_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_rsquare_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_lcurly_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_rcurly_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_name_40_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            break;
        default:
            mw_drop();
            push_ptr("expected name\0\0\0\0");
            mw_emit_fatal_error_21_();
            break;
    
}}

static void mw_token_name_3F_ (void){
    mw_dup();
    mw_token_name_40_();
}

static void mw_token_str_40_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            break;
        default:
            mw_drop();
            push_ptr("expected string\0\0\0\0");
            mw_emit_fatal_error_21_();
            break;
    
}}

static void mw_token_str_3F_ (void){
    mw_dup();
    mw_token_str_40_();
}

static void mw_token_int_40_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            break;
        default:
            mw_drop();
            push_ptr("expected int\0\0\0\0");
            mw_emit_fatal_error_21_();
            break;
    
}}

static void mw_token_int_3F_ (void){
    mw_dup();
    mw_token_int_40_();
}

static void mw_token_is_arg_end_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 1LL:
            mw_prim_drop();
            mw_true();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_left_enclosure_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_right_enclosure_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_location (void){
    mw_dup();
    mw_token_module();
    mw__40_();
    mw_swap();
    mw_dup();
    mw_token_row();
    mw__40_();
    mw_swap();
    mw_token_col();
    mw__40_();
    mw_location_pack();
}

static void mw_token_location_3F_ (void){
    mw_dup();
    mw_token_location();
}

static void mw_token_succ (void){
    mw_Token_2E_succ();
}

static void mw_token_pred (void){
    mw_Token_2E_pred();
}

static void mw_token_next (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_token_succ();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_token_succ();
            break;
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_token_succ();
            break;
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_token_succ();
            mw_dup();
            mw_token_value();
            mw__40_();
            switch (get_top_data_tag()) {
                case 2LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_nip();
                    mw_token_succ();
                    break;
                default:
                    mw_drop();
                    break;
            
}            break;
        default:
            mw_drop();
            mw_token_succ();
            break;
    
}}

static void mw_token_prev (void){
    mw_token_pred();
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_dup();
            mw_token_pred();
            mw_token_is_name_3F_();
            if (pop_u64()) {
                mw_nip();
            } else {
                mw_drop();
            }
            break;
        default:
            mw_drop();
            break;
    
}}

static void mw_token_next_arg_end (void){
    while(1) {
        mw_token_is_arg_end_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_token_next();
    }
}

static void mw_token_has_args_3F_ (void){
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_dup();
        mw_token_succ();
        mw_token_is_lparen_3F_();
        mw_nip();
    } else {
        mw_token_is_lparen_3F_();
    }
}

static void mw_token_num_args (void){
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_token_has_args_3F_();
        if (pop_u64()) {
            mw_token_succ();
        } else {
            mw_id();
        }
    } else {
        mw_id();
    }
    mw_token_is_left_enclosure_3F_();
    if (pop_u64()) {
        {
            VAL d3 = pop_value();
            push_i64(0LL);
            push_value(d3);
        }
        while(1) {
            mw_token_is_right_enclosure_3F_();
            mw_not();
            if (!pop_u64()) break;
            {
                VAL d4 = pop_value();
                mw_1_2B_();
                push_value(d4);
            }
            mw_token_succ();
            mw_token_next_arg_end();
        }
        mw_drop();
    } else {
        mw_drop();
        push_i64(0LL);
    }
}

static void mw_token_num_args_3F_ (void){
    mw_dup();
    mw_token_num_args();
}

static void mw_token_args_0 (void){
    mw_token_num_args_3F_();
    push_i64(0LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
    } else {
        push_ptr("expected no args\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mw_token_args_1 (void){
    mw_token_num_args_3F_();
    push_i64(1LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_token_is_name_3F_();
        if (pop_u64()) {
            mw_token_succ();
        } else {
            mw_id();
        }
        mw_token_succ();
    } else {
        mw_token_num_args_3F_();
        push_i64(1LL);
        mw__3C_();
        if (pop_u64()) {
            push_ptr("expected 1 arg, got none\0\0\0\0");
            mw_emit_fatal_error_21_();
        } else {
            push_ptr("expected 1 arg, got too many\0\0\0\0");
            mw_emit_fatal_error_21_();
        }
    }
}

static void mw_token_args_2 (void){
    mw_token_num_args_3F_();
    push_i64(2LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_token_is_name_3F_();
        if (pop_u64()) {
            mw_token_succ();
        } else {
            mw_id();
        }
        mw_token_succ();
        mw_dup();
        mw_token_next_arg_end();
        mw_token_succ();
    } else {
        mw_token_num_args_3F_();
        push_i64(2LL);
        mw__3C_();
        if (pop_u64()) {
            push_ptr("expected 2 args, got too few\0\0\0\0");
            mw_emit_fatal_error_21_();
        } else {
            push_ptr("expected 2 args, got too many\0\0\0\0");
            mw_emit_fatal_error_21_();
        }
    }
}

static void mw_token_args_3 (void){
    mw_token_num_args_3F_();
    push_i64(3LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_token_is_name_3F_();
        if (pop_u64()) {
            mw_token_succ();
        } else {
            mw_id();
        }
        mw_token_succ();
        mw_dup();
        mw_token_next_arg_end();
        mw_token_succ();
        mw_dup();
        mw_token_next_arg_end();
        mw_token_succ();
    } else {
        mw_token_num_args_3F_();
        push_i64(3LL);
        mw__3C_();
        if (pop_u64()) {
            push_ptr("expected 3 args, got too few\0\0\0\0");
            mw_emit_fatal_error_21_();
        } else {
            push_ptr("expected 3 args, got too many\0\0\0\0");
            mw_emit_fatal_error_21_();
        }
    }
}

static void mw_token_args (void){
    mw_token_has_args_3F_();
    if (pop_u64()) {
        mw_token_is_name_3F_();
        if (pop_u64()) {
            mw_token_succ();
        } else {
            mw_id();
        }
        mw_L0();
        mw_swap();
        while(1) {
            mw_token_is_args_end_3F_();
            mw_not();
            if (!pop_u64()) break;
            mw_token_succ();
            push_u64(0);
            push_fnptr(&mb_token_args_6);
            mw_prim_pack_cons();
            mw_sip();
            mw_token_next_arg_end();
        }
        mw_drop();
    } else {
        mw_drop();
        mw_L0();
    }
}

static void mw_token_is_args_end_3F_ (void){
    mw_dup();
    mw_token_is_comma_3F_();
    if (pop_u64()) {
        mw_token_succ();
    } else {
        mw_id();
    }
    mw_token_is_right_enclosure_3F_();
    mw_nip();
}

static void mw_token_args_2B_ (void){
    mw_dup();
    mw_token_args();
    mw_List__3E_List_2B_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            push_ptr("expected args\0\0\0\0");
            mw_emit_fatal_error_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_token_args_2_2B_ (void){
    mw_dup();
    mw_token_args();
    mw_dup();
    mw_len();
    push_i64(2LL);
    mw__3E__3D_();
    if (pop_u64()) {
        mw_nip();
    } else {
        mw_drop();
        push_ptr("expected 2+ args\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_List__3E_List_2B_();
    mw_unwrap();
    mw_uncons();
    mw_List__3E_List_2B_();
    mw_unwrap();
}

static void mw_emit_warning_21_ (void){
    {
        VAL d2 = pop_value();
        mw_token_location();
        push_value(d2);
    }
    mw_emit_warning_at_21_();
}

static void mw_emit_error_21_ (void){
    {
        VAL d2 = pop_value();
        mw_token_location();
        push_value(d2);
    }
    mw_emit_error_at_21_();
}

static void mw_emit_fatal_error_21_ (void){
    {
        VAL d2 = pop_value();
        mw_token_location();
        push_value(d2);
    }
    mw_emit_fatal_error_at_21_();
}

static void mw_token_is_module_end_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_run_end_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_true();
            break;
        case 1LL:
            mw_prim_drop();
            mw_true();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_run (void){
    push_u64(0);
    push_fnptr(&mb_token_run_1);
    mw_prim_pack_cons();
    push_u64(0);
    push_fnptr(&mb_token_run_2);
    mw_prim_pack_cons();
    mw_collect_while();
    mw_nip();
}

static void mw_token_run_has_arrow (void){
    mw_token_run();
    push_u64(0);
    push_fnptr(&mb_token_run_has_arrow_1);
    mw_prim_pack_cons();
    mw_any();
}

static void mw_token_run_has_dashes (void){
    mw_token_run();
    push_u64(0);
    push_fnptr(&mb_token_run_has_dashes_1);
    mw_prim_pack_cons();
    mw_any();
}

static void mw_sig_is_stack_end_3F_ (void){
    mw_token_is_dashes_3F_();
    if (pop_u64()) {
        mw_true();
    } else {
        mw_token_run_end_3F_();
    }
}

static void mw_sig_is_stack_end2_3F_ (void){
    mw_sig_is_stack_end_3F_();
    if (pop_u64()) {
        mw_true();
    } else {
        mw_sig_token_is_effect_con_3F_();
    }
}

static void mw_sig_next_stack_end (void){
    while(1) {
        mw_sig_is_stack_end_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_token_next();
    }
}

static void mw_sig_has_dashes (void){
    mw_sig_next_stack_end();
    mw_token_is_dashes_3F_();
    mw_nip();
}

static void mw_sig_has_dashes_3F_ (void){
    mw_dup();
    mw_sig_has_dashes();
}

static void mw_sig_arity (void){
    mw_sig_has_dashes_3F_();
    if (pop_u64()) {
        mw_sig_count_types();
        mw_token_next();
        mw_sig_count_types();
        mw_drop();
    } else {
        mw_sig_count_types();
        mw_drop();
        push_i64(0LL);
        mw_swap();
    }
}

static void mw_sig_count_types (void){
    push_i64(0LL);
    mw_swap();
    while(1) {
        mw_sig_is_stack_end_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_sig_token_is_type_3F_();
        if (pop_u64()) {
            {
                VAL d4 = pop_value();
                mw_1_2B_();
                push_value(d4);
            }
        } else {
            mw_id();
        }
        mw_token_next();
    }
}

static void mw_sig_token_is_type_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_name_could_be_type();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_sig_token_is_type_con_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_name_could_be_type_con();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_sig_token_is_type_hole_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_name_is_type_hole();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_underscore_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_name_is_underscore();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_sig_token_is_type_var_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_name_could_be_type_var();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_token_is_param_name_3F_ (void){
    mw_sig_token_is_type_var_3F_();
}

static void mw_sig_token_is_stack_var_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_name_could_be_stack_var();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_sig_token_is_effect_con_3F_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_name_could_be_effect_con();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_sig_skip_dashes (void){
    mw_sig_has_dashes_3F_();
    if (pop_u64()) {
        mw_sig_next_stack_end();
        mw_token_next();
    } else {
        mw_id();
    }
}

static void mw_run_lexer_21_ (void){
    mw_module_new_21_();
    mw_lexer_module();
    mw__21_();
    mw_lexer_module();
    mw__40_();
    mw_module_source_path();
    mw_Path__3E_Str();
    mw_open_file_21_();
    mw_input_start_21_();
    push_i64(1LL);
    mw_Int__3E_Row();
    mw_lexer_row();
    mw__21_();
    push_i64(1LL);
    mw_Int__3E_Col();
    mw_lexer_col();
    mw__21_();
    mw_token_alloc_21_();
    while(1) {
        mw_lexer_done_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_lexer_next_21_();
    }
    mw_input_end_21_();
    mw_lexer_stack();
    mw_stack_pop_21_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_TOKEN_NONE();
            mw_lexer_emit_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_ptr("Mismatched left parenthesis.\0\0\0\0");
            mw_emit_fatal_error_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}    mw_token_alloc_21_();
    mw_lexer_module();
    mw__40_();
    mw_module_end();
    mw__21_();
    mw_token_succ();
    mw_lexer_module();
    mw__40_();
    mw_module_start();
    mw__21_();
    mw_lexer_module();
    mw__40_();
}

static void mw_lexer_done_3F_ (void){
    mw_input_done_3F_();
}

static void mw_lexer_make_21_ (void){
    mw_token_alloc_21_();
    mw_tuck();
    mw_token_value();
    mw__21_();
    mw_lexer_module();
    mw__40_();
    mw_over();
    mw_token_module();
    mw__21_();
    mw_lexer_row();
    mw__40_();
    mw_over();
    mw_token_row();
    mw__21_();
    mw_lexer_col();
    mw__40_();
    mw_over();
    mw_token_col();
    mw__21_();
}

static void mw_lexer_emit_21_ (void){
    mw_lexer_make_21_();
    mw_drop();
}

static void mw_lexer_next_21_ (void){
    mw_lexer_peek();
    mw_char_valid_3F_();
    mw_not();
    if (pop_u64()) {
        push_ptr("invalid character\0\0\0\0");
        mw_lexer_emit_fatal_error_21_();
    } else {
        mw_is_name_char_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_lexer_emit_name_21_();
        } else {
            mw_is_newline_3F_();
            if (pop_u64()) {
                mw_drop();
                mw_lexer_newline_21_();
            } else {
                mw_is_whitespace_3F_();
                if (pop_u64()) {
                    mw_drop();
                } else {
                    mw_is_pound_3F_();
                    if (pop_u64()) {
                        mw_drop();
                        mw_lexer_skip_comment_21_();
                    } else {
                        mw_is_comma_3F_();
                        if (pop_u64()) {
                            mw_drop();
                            mw_TOKEN_COMMA();
                            mw_lexer_emit_21_();
                        } else {
                            mw_is_lparen_3F_();
                            if (pop_u64()) {
                                mw_drop();
                                mw_lexer_emit_lparen_21_();
                            } else {
                                mw_is_rparen_3F_();
                                if (pop_u64()) {
                                    mw_drop();
                                    mw_lexer_emit_rparen_21_();
                                } else {
                                    mw_is_lsquare_3F_();
                                    if (pop_u64()) {
                                        mw_drop();
                                        mw_lexer_emit_lsquare_21_();
                                    } else {
                                        mw_is_rsquare_3F_();
                                        if (pop_u64()) {
                                            mw_drop();
                                            mw_lexer_emit_rsquare_21_();
                                        } else {
                                            mw_is_lcurly_3F_();
                                            if (pop_u64()) {
                                                mw_drop();
                                                mw_lexer_emit_lcurly_21_();
                                            } else {
                                                mw_is_rcurly_3F_();
                                                if (pop_u64()) {
                                                    mw_drop();
                                                    mw_lexer_emit_rcurly_21_();
                                                } else {
                                                    mw_is_quote_3F_();
                                                    if (pop_u64()) {
                                                        mw_drop();
                                                        mw_lexer_emit_string_21_();
                                                    } else {
                                                        push_ptr("unrecognized token\0\0\0\0");
                                                        mw_lexer_emit_fatal_error_21_();
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            mw_lexer_move_21_();
        }
    }
}

static void mw_lexer_newline_21_ (void){
    mw_lexer_row();
    push_u64(0);
    push_fnptr(&mb_lexer_newline_21__1);
    mw_prim_pack_cons();
    mw_modify();
    push_i64(0LL);
    mw_Int__3E_Col();
    mw_lexer_col();
    mw__21_();
}

static void mw_lexer_emit_lparen_21_ (void){
    mw_nil();
    mw_TOKEN_LPAREN();
    mw_lexer_make_21_();
    mw_lexer_stack();
    mw_stack_push_21_();
}

static void mw_lexer_emit_rparen_21_ (void){
    mw_lexer_stack();
    mw_stack_pop_21_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            push_ptr("Mismatched right parenthesis.\0\0\0\0");
            mw_lexer_emit_fatal_error_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_token_is_lparen_3F_();
            if (pop_u64()) {
                mw_dup();
                mw_TOKEN_RPAREN();
                mw_lexer_make_21_();
                mw_TOKEN_LPAREN();
                mw_swap();
                mw_token_value();
                mw__21_();
            } else {
                push_ptr("Mismatched right parenthesis.\0\0\0\0");
                mw_lexer_emit_fatal_error_21_();
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_lexer_emit_lsquare_21_ (void){
    mw_nil();
    mw_TOKEN_LSQUARE();
    mw_lexer_make_21_();
    mw_lexer_stack();
    mw_stack_push_21_();
}

static void mw_lexer_emit_rsquare_21_ (void){
    mw_lexer_stack();
    mw_stack_pop_21_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            push_ptr("Mismatched right bracket.\0\0\0\0");
            mw_lexer_emit_fatal_error_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_token_is_lsquare_3F_();
            if (pop_u64()) {
                mw_dup();
                mw_TOKEN_RSQUARE();
                mw_lexer_make_21_();
                mw_TOKEN_LSQUARE();
                mw_swap();
                mw_token_value();
                mw__21_();
            } else {
                push_ptr("Mismatched right bracket.\0\0\0\0");
                mw_lexer_emit_fatal_error_21_();
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_lexer_emit_lcurly_21_ (void){
    mw_nil();
    mw_TOKEN_LCURLY();
    mw_lexer_make_21_();
    mw_lexer_stack();
    mw_stack_push_21_();
}

static void mw_lexer_emit_rcurly_21_ (void){
    mw_lexer_stack();
    mw_stack_pop_21_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            push_ptr("Mismatched right brace.\0\0\0\0");
            mw_lexer_emit_fatal_error_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_token_is_lcurly_3F_();
            if (pop_u64()) {
                mw_dup();
                mw_TOKEN_RCURLY();
                mw_lexer_make_21_();
                mw_TOKEN_LCURLY();
                mw_swap();
                mw_token_value();
                mw__21_();
            } else {
                push_ptr("Mismatched right brace.\0\0\0\0");
                mw_lexer_emit_fatal_error_21_();
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_lexer_emit_name_21_ (void){
    mw_str_buf_clear_21_();
    mw_lexer_module();
    mw__40_();
    mw_lexer_row();
    mw__40_();
    mw_lexer_col();
    mw__40_();
    mw_lexer_peek();
    while(1) {
        mw_is_name_char_3F_();
        if (!pop_u64()) break;
        mw_char_valid_3F_();
        if (pop_u64()) {
            mw_str_buf_push_char_21_();
            mw_lexer_move_21_();
            mw_lexer_peek();
        } else {
            push_ptr("invalid character\0\0\0\0");
            mw_lexer_emit_fatal_error_21_();
        }
    }
    mw_drop();
    mw_str_buf_is_doc_start_3F_();
    if (pop_u64()) {
        mw_drop3();
        mw_lexer_skip_doc_21_();
    } else {
        mw_str_buf_is_int_3F_();
        if (pop_u64()) {
            mw_str_buf_int_3F_();
            mw_TOKEN_INT();
        } else {
            mw_str_buf_dup_21_();
            mw_name_new_21_();
            mw_TOKEN_NAME();
        }
        mw_token_alloc_21_();
        mw_tuck();
        mw_token_value();
        mw__21_();
        mw_tuck();
        mw_token_col();
        mw__21_();
        mw_tuck();
        mw_token_row();
        mw__21_();
        mw_token_module();
        mw__21_();
    }
}

static void mw_str_buf_is_doc_start_3F_ (void){
    mw_str_buf_length_3F_();
    push_i64(3LL);
    mw__3D__3D_();
    push_u64(0);
    push_fnptr(&mb_str_buf_is_doc_start_3F__1);
    mw_prim_pack_cons();
    mw_and();
}

static void mw_str_buf_is_arrow_3F_ (void){
    mw_str_buf_length_3F_();
    push_i64(2LL);
    mw__3D__3D_();
    push_u64(0);
    push_fnptr(&mb_str_buf_is_arrow_3F__1);
    mw_prim_pack_cons();
    mw_and();
}

static void mw_str_buf_is_dashes_3F_ (void){
    mw_str_buf_length_3F_();
    push_i64(2LL);
    mw__3D__3D_();
    if (pop_u64()) {
        push_i64(0LL);
        mw_str_buf_char_40_();
        mw_is_dash_3F_();
        mw_nip();
        push_i64(1LL);
        mw_str_buf_char_40_();
        mw_is_dash_3F_();
        mw_nip();
        mw__26__26_();
    } else {
        mw_false();
    }
}

static void mw_str_buf_is_equal_3F_ (void){
    mw_str_buf_length_3F_();
    push_i64(1LL);
    mw__3D__3D_();
    if (pop_u64()) {
        push_i64(0LL);
        mw_str_buf_char_40_();
        mw_is_eq_3F_();
        mw_nip();
    } else {
        mw_false();
    }
}

static void mw_str_buf_is_int_3F_ (void){
    mw_str_buf_is_dec_int_3F_();
    if (pop_u64()) {
        mw_true();
    } else {
        mw_str_buf_is_hex_int_3F_();
    }
}

static void mw_str_buf_is_dec_int_3F_ (void){
    push_i64(0LL);
    push_i64(0LL);
    mw_dup();
    mw_str_buf_char_40_();
    mw_is_sign_3F_();
    mw_nip();
    if (pop_u64()) {
        mw_1_2B_();
    } else {
        mw_id();
    }
    while(1) {
        mw_dup();
        mw_str_buf_char_40_();
        mw_is_digit_3F_();
        mw_nip();
        if (!pop_u64()) break;
        {
            VAL d3 = pop_value();
            mw_1_2B_();
            push_value(d3);
        }
        mw_1_2B_();
    }
    mw_swap();
    push_i64(1LL);
    mw__3E__3D_();
    if (pop_u64()) {
        mw_str_buf_length_3F_();
        mw__3D__3D_();
    } else {
        mw_drop();
        mw_false();
    }
}

static void mw_is_zero_char (void){
    mw_Char__3E_Int();
    push_i64(48LL);
    mw__3D__3D_();
}

static void mw_is_xX_char (void){
    mw_Char__3E_Int();
    mw_dup();
    push_i64(88LL);
    mw__3D__3D_();
    {
        VAL d2 = pop_value();
        push_i64(120LL);
        mw__3D__3D_();
        push_value(d2);
    }
    mw__7C__7C_();
}

static void mw_str_buf_is_hex_int_3F_ (void){
    push_i64(0LL);
    push_i64(0LL);
    mw_dup();
    mw_str_buf_char_40_();
    mw_is_sign_3F_();
    mw_nip();
    if (pop_u64()) {
        mw_1_2B_();
    } else {
        mw_id();
    }
    mw_dup();
    mw_str_buf_char_40_();
    mw_is_zero_char();
    if (pop_u64()) {
        mw_1_2B_();
        mw_dup();
        mw_str_buf_char_40_();
        mw_is_xX_char();
        if (pop_u64()) {
            mw_1_2B_();
            while(1) {
                mw_dup();
                mw_str_buf_char_40_();
                mw_is_hexdigit_3F_();
                mw_nip();
                if (!pop_u64()) break;
                {
                    VAL d5 = pop_value();
                    mw_1_2B_();
                    push_value(d5);
                }
                mw_1_2B_();
            }
            mw_swap();
            push_i64(1LL);
            mw__3E__3D_();
            if (pop_u64()) {
                mw_str_buf_length_3F_();
                mw__3D__3D_();
            } else {
                mw_drop();
                mw_false();
            }
        } else {
            mw_drop2();
            mw_false();
        }
    } else {
        mw_drop2();
        mw_false();
    }
}

static void mw_str_buf_int_3F_ (void){
    mw_str_buf_is_dec_int_3F_();
    if (pop_u64()) {
        mw_str_buf_dec_int_3F_();
    } else {
        mw_str_buf_hex_int_3F_();
    }
}

static void mw_str_buf_dec_int_3F_ (void){
    push_i64(1LL);
    push_i64(0LL);
    push_i64(0LL);
    mw_dup();
    mw_str_buf_char_40_();
    mw_is_sign_3F_();
    if (pop_u64()) {
        mw_is_dash_3F_();
        mw_nip();
        if (pop_u64()) {
            push_u64(0);
            push_fnptr(&mb_str_buf_dec_int_3F__3);
            mw_prim_pack_cons();
            mw_dip2();
        } else {
            mw_id();
        }
        mw_1_2B_();
    } else {
        mw_drop();
    }
    while(1) {
        mw_dup();
        mw_str_buf_length_3F_();
        mw__3C_();
        if (!pop_u64()) break;
        push_u64(0);
        push_fnptr(&mb_str_buf_dec_int_3F__8);
        mw_prim_pack_cons();
        mw_sip();
        mw_1_2B_();
    }
    mw_drop();
    mw__2A_();
}

static void mw_str_buf_hex_int_3F_ (void){
    push_i64(1LL);
    push_i64(0LL);
    push_i64(0LL);
    mw_dup();
    mw_str_buf_char_40_();
    mw_is_sign_3F_();
    if (pop_u64()) {
        mw_is_dash_3F_();
        mw_nip();
        if (pop_u64()) {
            push_u64(0);
            push_fnptr(&mb_str_buf_hex_int_3F__3);
            mw_prim_pack_cons();
            mw_dip2();
        } else {
            mw_id();
        }
        mw_1_2B_();
    } else {
        mw_drop();
    }
    push_i64(2LL);
    mw__2B_();
    while(1) {
        mw_dup();
        mw_str_buf_length_3F_();
        mw__3C_();
        if (!pop_u64()) break;
        push_u64(0);
        push_fnptr(&mb_str_buf_hex_int_3F__8);
        mw_prim_pack_cons();
        mw_sip();
        mw_1_2B_();
    }
    mw_drop();
    mw__2A_();
}

static void mw_hexdigit_value (void){
    mw_is_digit_3F_();
    if (pop_u64()) {
        mw_Char__3E_Int();
        push_i64(48LL);
        mw__();
    } else {
        mw_is_upper_hexdigit_3F_();
        if (pop_u64()) {
            mw_Char__3E_Int();
            push_i64(55LL);
            mw__();
        } else {
            mw_Char__3E_Int();
            push_i64(87LL);
            mw__();
        }
    }
}

static void mw_lexer_emit_string_21_ (void){
    mw_str_buf_clear_21_();
    mw_TOKEN_NONE();
    mw_lexer_make_21_();
    mw_lexer_move_21_();
    mw_lexer_peek();
    while(1) {
        mw_is_string_end_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_char_valid_3F_();
        if (pop_u64()) {
            mw_lexer_push_string_char_21_();
            mw_lexer_move_21_();
            mw_lexer_peek();
        } else {
            push_ptr("invalid character in string literal\0\0\0\0");
            mw_lexer_emit_fatal_error_21_();
        }
    }
    mw_drop();
    mw_str_buf_dup_21_();
    mw_TOKEN_STR();
    mw_swap();
    mw_token_value();
    mw__21_();
}

static void mw_lexer_push_string_char_21_ (void){
    mw_is_backslash_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_lexer_move_21_();
        mw_lexer_peek();
        mw_is_newline_3F_();
        if (pop_u64()) {
            mw_drop();
        } else {
            mw_is_n_3F_();
            if (pop_u64()) {
                mw_drop();
                push_i64(10LL);
                mw_Int__3E_Char();
                mw_str_buf_push_char_21_();
            } else {
                mw_is_r_3F_();
                if (pop_u64()) {
                    mw_drop();
                    push_i64(13LL);
                    mw_Int__3E_Char();
                    mw_str_buf_push_char_21_();
                } else {
                    mw_is_t_3F_();
                    if (pop_u64()) {
                        mw_drop();
                        push_i64(9LL);
                        mw_Int__3E_Char();
                        mw_str_buf_push_char_21_();
                    } else {
                        mw_is_quote_3F_();
                        if (pop_u64()) {
                            mw_str_buf_push_char_21_();
                        } else {
                            mw_is_backslash_3F_();
                            if (pop_u64()) {
                                mw_str_buf_push_char_21_();
                            } else {
                                mw_str_buf_push_char_21_();
                                push_ptr("Unknown character escape sequence.\0\0\0\0");
                                mw_lexer_emit_warning_21_();
                            }
                        }
                    }
                }
            }
        }
    } else {
        mw_str_buf_push_char_21_();
    }
}

static void mw_lexer_skip_comment_21_ (void){
    while(1) {
        mw_lexer_comment_end_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_lexer_move_21_();
    }
    mw_lexer_peek();
    mw_is_newline_3F_();
    if (pop_u64()) {
        mw_lexer_newline_21_();
    } else {
        mw_id();
    }
    mw_drop();
}

static void mw_lexer_skip_doc_21_ (void){
    while(1) {
        mw_lexer_comment_end_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_lexer_move_21_();
    }
}

static void mw_lexer_comment_end_3F_ (void){
    mw_lexer_done_3F_();
    if (pop_u64()) {
        mw_true();
    } else {
        mw_lexer_peek();
        mw_is_newline_3F_();
        mw_nip();
    }
}

static void mw_lexer_peek (void){
    mw_input_peek();
}

static void mw_lexer_move_21_ (void){
    mw_input_move_21_();
    mw_lexer_col();
    push_u64(0);
    push_fnptr(&mb_lexer_move_21__1);
    mw_prim_pack_cons();
    mw_modify();
}

static void mw_lexer_location (void){
    mw_lexer_module();
    mw__40_();
    mw_lexer_row();
    mw__40_();
    mw_lexer_col();
    mw__40_();
    mw_location_pack();
}

static void mw_lexer_emit_warning_21_ (void){
    {
        VAL d2 = pop_value();
        mw_lexer_location();
        push_value(d2);
    }
    mw_emit_warning_at_21_();
}

static void mw_lexer_emit_error_21_ (void){
    {
        VAL d2 = pop_value();
        mw_lexer_location();
        push_value(d2);
    }
    mw_emit_error_at_21_();
}

static void mw_lexer_emit_fatal_error_21_ (void){
    {
        VAL d2 = pop_value();
        mw_lexer_location();
        push_value(d2);
    }
    mw_emit_fatal_error_at_21_();
}

static void mw_Buffer_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Buffer_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Buffer_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Buffer_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Buffer_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Buffer_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Buffer_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Buffer_2E_alloc_21_ (void){
    mw_Buffer_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Buffer_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_buffer_new_21_ (void){
    mw_Buffer_2E_alloc_21_();
    mw_tuck();
    mw_buffer_size();
    mw__21_();
    mw_tuck();
    mw_dup2();
    mw_buffer_name();
    mw__21_();
    mw_DEF_BUFFER();
    mw_swap();
    mw_name_def();
    mw__21_();
}

static void mw_unBAG (void){
    mw_id();
}

static void mw_B0 (void){
    mw_nil();
}

static void mw_B1 (void){
    mw_L1();
    mw_BAG();
}

static void mw_B2 (void){
    mw_order2();
    mw_L2();
    mw_BAG();
}

static void mw_B3 (void){
    mw_order3();
    mw_L3();
    mw_BAG();
}

static void mw_unBAG_2B_ (void){
    mw_id();
}

static void mw_B1_2B_ (void){
    mw_L1_2B_();
    mw_BAG_2B_();
}

static void mw_B2_2B_ (void){
    mw_order2();
    mw_L2_2B_();
    mw_BAG_2B_();
}

static void mw_B3_2B_ (void){
    mw_order3();
    mw_L3_2B_();
    mw_BAG_2B_();
}

static void mw_Bag_2B___3E_Bag (void){
    mw_unBAG_2B_();
    mw_List_2B___3E_List();
    mw_BAG();
}

static void mw_Bag__3E_Bag_2B_ (void){
    mw_unBAG();
    mw_List__3E_List_2B_();
    push_u64(0);
    push_fnptr(&mb_Bag__3E_Bag_2B__1);
    mw_prim_pack_cons();
    mw_maybe_map();
}

static void mw_bag_empty (void){
    mw_nil();
}

static void mw_bag_is_empty (void){
    mw_is_nil();
}

static void mw_bag_singleton (void){
    mw_B1();
}

static void mw_bag_is_singleton (void){
    mw_bag_len();
    push_i64(1LL);
    mw__3D__3D_();
}

static void mw_bag_is_singleton_2B_ (void){
    mw_bag_len_2B_();
    push_i64(1LL);
    mw__3D__3D_();
}

static void mw_bag_len (void){
    mw_unBAG();
    mw_len();
}

static void mw_bag_len_2B_ (void){
    mw_unBAG_2B_();
    mw_len_2B_();
}

static void mw_bag_len_3F_ (void){
    mw_dup();
    mw_bag_len();
}

static void mw_bag_len_2B__3F_ (void){
    mw_dup();
    mw_bag_len_2B_();
}

static void mw_bag_first (void){
    mw_unBAG();
    mw_first();
}

static void mw_bag_last (void){
    mw_unBAG();
    mw_last();
}

static void mw_bag_middle (void){
    mw_unBAG();
    mw_middle();
}

static void mw_bag_first_2B_ (void){
    mw_unBAG_2B_();
    mw_first_2B_();
}

static void mw_bag_last_2B_ (void){
    mw_unBAG_2B_();
    mw_last_2B_();
}

static void mw_bag_middle_2B_ (void){
    mw_unBAG_2B_();
    mw_middle_2B_();
}

static void mw_bag_split_half_left (void){
    mw_unBAG_2B_();
    mw_split_half_left();
    {
        VAL d2 = pop_value();
        mw_BAG_2B_();
        push_value(d2);
    }
    mw_BAG();
}

static void mw_bag_split_half_right (void){
    mw_unBAG_2B_();
    mw_split_half_right();
    {
        VAL d2 = pop_value();
        mw_BAG();
        push_value(d2);
    }
    mw_BAG_2B_();
}

static void mw_bag_split_half (void){
    mw_unBAG();
    mw_split_half();
    {
        VAL d2 = pop_value();
        mw_BAG();
        push_value(d2);
    }
    mw_BAG();
}

static void mw_bag_uncons (void){
    mw_unBAG_2B_();
    mw_uncons();
    mw_BAG();
}

static void mw_bag_unsnoc (void){
    mw_unBAG_2B_();
    mw_unsnoc();
    {
        VAL d2 = pop_value();
        mw_BAG();
        push_value(d2);
    }
}

static void mw_bag_cons (void){
    mw_swap();
    mw_bag_insert();
}

static void mw_bag_snoc (void){
    mw_bag_insert();
}

static void mw_bag_cons_2B_ (void){
    mw_swap();
    mw_bag_insert_2B_();
}

static void mw_bag_snoc_2B_ (void){
    mw_bag_insert_2B_();
}

static void mw_bag_cons_2B__2B_ (void){
    mw_swap();
    mw_bag_insert_2B__2B_();
}

static void mw_bag_snoc_2B__2B_ (void){
    mw_bag_insert_2B__2B_();
}

static void mw_bag_has (void){
    mw_swap();
    mw_Bag__3E_Bag_2B_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_drop();
            mw_false();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_bag_has_2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_bag_has_2B_ (void){
    mw_over();
    mw_bag_is_singleton_2B_();
    if (pop_u64()) {
        {
            VAL d3 = pop_value();
            mw_bag_first_2B_();
            push_value(d3);
        }
        mw__3D__3D_();
    } else {
        {
            VAL d3 = pop_value();
            mw_bag_split_half_right();
            push_value(d3);
        }
        mw_over();
        mw_bag_first_2B_();
        mw_cmp_3F_();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_drop4();
                mw_true();
                break;
            case 1LL:
                mw_prim_drop();
                mw_drop();
                mw_nip();
                mw_bag_has();
                break;
            case 2LL:
                mw_prim_drop();
                mw_drop();
                {
                    VAL d5 = pop_value();
                    mw_nip();
                    push_value(d5);
                }
                mw_bag_has_2B_();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}    }
}

static void mw_bag_has_3F_ (void){
    mw_dup2();
    mw_bag_has();
}

static void mw_bag_insert (void){
    mw_bag_insert_2B_();
    mw_Bag_2B___3E_Bag();
}

static void mw_bag_insert_2B_ (void){
    mw_swap();
    mw_Bag__3E_Bag_2B_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_B1_2B_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_bag_insert_2B__2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_bag_insert_2B__2B_ (void){
    mw_over();
    mw_bag_is_singleton_2B_();
    if (pop_u64()) {
        {
            VAL d3 = pop_value();
            mw_bag_first_2B_();
            push_value(d3);
        }
        mw_B2_2B_();
    } else {
        {
            VAL d3 = pop_value();
            mw_bag_split_half_right();
            push_value(d3);
        }
        mw_over();
        mw_bag_first_2B_();
        mw_dup2();
        mw__3E__3D_();
        if (pop_u64()) {
            mw_drop();
            mw_bag_insert_2B__2B_();
            mw_bag_cat_unsafe__2B_();
        } else {
            mw_drop();
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_bag_insert_2B_();
                push_value(d4);
            }
            mw_bag_cat_unsafe_2B_();
        }
    }
}

static void mw_bag_replace (void){
    mw_bag_has_3F_();
    if (pop_u64()) {
        mw_drop();
    } else {
        mw_bag_insert();
    }
}

static void mw_bag_cat_unsafe (void){
    {
        VAL d2 = pop_value();
        mw_unBAG();
        push_value(d2);
    }
    mw_unBAG();
    mw_cat();
    mw_BAG();
}

static void mw_bag_cat_unsafe__2B_ (void){
    {
        VAL d2 = pop_value();
        mw_unBAG();
        push_value(d2);
    }
    mw_unBAG_2B_();
    mw_cat__2B_();
    mw_BAG_2B_();
}

static void mw_bag_cat_unsafe_2B_ (void){
    {
        VAL d2 = pop_value();
        mw_unBAG_2B_();
        push_value(d2);
    }
    mw_unBAG_2B_();
    mw_cat_2B_();
    mw_BAG_2B_();
}

static void mw_order2 (void){
    mw_dup2();
    mw__3C__3D_();
    if (pop_u64()) {
        mw_id();
    } else {
        mw_swap();
    }
}

static void mw_order3 (void){
    {
        VAL d2 = pop_value();
        mw_order2();
        push_value(d2);
    }
    mw_dup2();
    mw__3C__3D_();
    if (pop_u64()) {
        mw_id();
    } else {
        mw_swap();
        {
            VAL d3 = pop_value();
            mw_order2();
            push_value(d3);
        }
    }
}

static void mw_bag_lookup_key (void){
    mw_swap();
    mw_Bag__3E_Bag_2B_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_drop();
            mw_NONE();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_bag_lookup_key_2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_bag_lookup_key_2B_ (void){
    mw_over();
    mw_bag_is_singleton_2B_();
    if (pop_u64()) {
        {
            VAL d3 = pop_value();
            mw_bag_first_2B_();
            mw_unpack2();
            push_value(d3);
        }
        mw__3D__3D_();
        if (pop_u64()) {
            mw_SOME();
        } else {
            mw_drop();
            mw_NONE();
        }
    } else {
        {
            VAL d3 = pop_value();
            mw_bag_split_half_right();
            mw_dup();
            mw_bag_first_2B_();
            mw_unpack2();
            push_value(d3);
        }
        mw_cmp_3F_();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_drop2();
                mw_SOME();
                {
                    VAL d5 = pop_value();
                    mw_drop2();
                    push_value(d5);
                }
                break;
            case 1LL:
                mw_prim_drop();
                {
                    VAL d5 = pop_value();
                    mw_drop2();
                    mw_nip();
                    push_value(d5);
                }
                mw_bag_lookup_key_2B_();
                break;
            case 2LL:
                mw_prim_drop();
                {
                    VAL d5 = pop_value();
                    mw_drop3();
                    push_value(d5);
                }
                mw_bag_lookup_key();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}    }
}

static void mw_bag_lookup_key_3F_ (void){
    mw_dup2();
    mw_bag_lookup_key();
}

static void mw_bag_replace_key (void){
    mw_bag_replace_key_2B_();
    mw_Bag_2B___3E_Bag();
}

static void mw_bag_replace_key_2B_ (void){
    mw_swap();
    mw_Bag__3E_Bag_2B_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_B1_2B_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_bag_replace_key_2B__2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_bag_replace_key_2B__2B_ (void){
    mw_over();
    mw_bag_is_singleton_2B_();
    if (pop_u64()) {
        {
            VAL d3 = pop_value();
            mw_bag_first_2B_();
            push_value(d3);
        }
        mw_dup2();
        mw__3D__3D_key();
        if (pop_u64()) {
            mw_nip();
            mw_B1_2B_();
        } else {
            mw_B2_2B_();
        }
    } else {
        {
            VAL d3 = pop_value();
            mw_bag_split_half_right();
            mw_dup();
            mw_bag_first_2B_();
            push_value(d3);
        }
        mw_dup2();
        mw__3C__3D_key();
        if (pop_u64()) {
            mw_nip();
            mw_bag_replace_key_2B__2B_();
            mw_bag_cat_unsafe__2B_();
        } else {
            mw_nip();
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_bag_replace_key_2B_();
                push_value(d4);
            }
            mw_bag_cat_unsafe_2B_();
        }
    }
}

static void mw__3D__3D_key (void){
    {
        VAL d2 = pop_value();
        mw_unpack2();
        mw_nip();
        push_value(d2);
    }
    mw_unpack2();
    mw_nip();
    mw__3D__3D_();
}

static void mw__3C__3D_key (void){
    {
        VAL d2 = pop_value();
        mw_unpack2();
        mw_nip();
        push_value(d2);
    }
    mw_unpack2();
    mw_nip();
    mw__3C__3D_();
}

static void mw_bag_values (void){
    mw_unBAG();
}

static void mw_bag_values_2B_ (void){
    mw_unBAG_2B_();
}

static void mw_unMAP (void){
    mw_id();
}

static void mw_map_empty (void){
    mw_nil();
}

static void mw_map_is_empty (void){
    mw_is_nil();
}

static void mw_map_is_empty_3F_ (void){
    mw_is_nil_3F_();
}

static void mw_map_singleton (void){
    mw_B1();
    mw_MAP();
}

static void mw_map_is_singleton (void){
    mw_unMAP();
    mw_bag_is_singleton();
}

static void mw_map_has (void){
    mw_map_lookup();
    mw_is_some();
}

static void mw_map_has_3F_ (void){
    mw_dup2();
    mw_map_has();
}

static void mw_map_lookup (void){
    {
        VAL d2 = pop_value();
        mw_unMAP();
        push_value(d2);
    }
    mw_bag_lookup_key();
}

static void mw_map_lookup_3F_ (void){
    mw_dup2();
    mw_map_lookup();
}

static void mw_map_insert (void){
    {
        VAL d2 = pop_value();
        mw_unMAP();
        push_value(d2);
    }
    mw_bag_replace_key();
    mw_MAP();
}

static void mw_map_cons (void){
    mw_swap();
    mw_map_insert();
}

static void mw_map_snoc (void){
    mw_map_insert();
}

static void mw_map_pairs (void){
    mw_unMAP();
    mw_bag_values();
}

static void mw_map_keys (void){
    mw_map_pairs();
    push_u64(0);
    push_fnptr(&mb_map_keys_1);
    mw_prim_pack_cons();
    mw_map();
}

static void mw_map_values (void){
    mw_map_pairs();
    push_u64(0);
    push_fnptr(&mb_map_values_1);
    mw_prim_pack_cons();
    mw_map();
}

static void mw_prim_type_is_physical (void){
    switch (get_top_data_tag()) {
        case 1LL:
            mw_prim_drop();
            mw_false();
            break;
        case 2LL:
            mw_prim_drop();
            mw_false();
            break;
        case 3LL:
            mw_prim_drop();
            mw_false();
            break;
        default:
            mw_drop();
            mw_true();
            break;
    
}}

static void mw_MetaVar_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_MetaVar_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_MetaVar_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_MetaVar_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_MetaVar_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_MetaVar_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_MetaVar_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_MetaVar_2E_alloc_21_ (void){
    mw_MetaVar_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_MetaVar_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_Data_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Data_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Data_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Data_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Data_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Data_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Data_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Data_2E_alloc_21_ (void){
    mw_Data_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Data_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_Tag_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Tag_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Tag_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Tag_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Tag_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Tag_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Tag_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Tag_2E_alloc_21_ (void){
    mw_Tag_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Tag_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_def_type_21_ (void){
    {
        VAL d2 = pop_value();
        mw_DEF_TYPE();
        push_value(d2);
    }
    mw_name_new_21_();
    mw_name_def();
    mw__21_();
}

static void mw_init_types_21_ (void){
    mw_TYPE_INT();
    push_ptr("Int\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_PTR();
    push_ptr("Ptr\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_STR();
    push_ptr("Str\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_CHAR();
    push_ptr("Char\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_U8();
    push_ptr("U8\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_U16();
    push_ptr("U16\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_U32();
    push_ptr("U32\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_U64();
    push_ptr("U64\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_I8();
    push_ptr("I8\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_I16();
    push_ptr("I16\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_I32();
    push_ptr("I32\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_I64();
    push_ptr("I64\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_BOOL();
    push_ptr("Bool\0\0\0\0");
    mw_def_type_21_();
    mw_TYPE_MUT();
    push_ptr("Mut\0\0\0\0");
    mw_def_type_21_();
}

static void mw_T_2A_ (void){
    mw_TTensor();
}

static void mw_T__3E_ (void){
    mw_TMorphism();
}

static void mw_TMut (void){
    mw_TYPE_MUT();
    mw_swap();
    mw_TApp();
}

static void mw_T0 (void){
    mw_TYPE_UNIT();
}

static void mw_T1 (void){
    {
        VAL d2 = pop_value();
        mw_T0();
        push_value(d2);
    }
    mw_T_2A_();
}

static void mw_T2 (void){
    {
        VAL d2 = pop_value();
        mw_T1();
        push_value(d2);
    }
    mw_T_2A_();
}

static void mw_T3 (void){
    {
        VAL d2 = pop_value();
        mw_T2();
        push_value(d2);
    }
    mw_T_2A_();
}

static void mw_T4 (void){
    {
        VAL d2 = pop_value();
        mw_T3();
        push_value(d2);
    }
    mw_T_2A_();
}

static void mw_T5 (void){
    {
        VAL d2 = pop_value();
        mw_T4();
        push_value(d2);
    }
    mw_T_2A_();
}

static void mw_T6 (void){
    {
        VAL d2 = pop_value();
        mw_T5();
        push_value(d2);
    }
    mw_T_2A_();
}

static void mw_type_is_morphism (void){
    switch (get_top_data_tag()) {
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop2();
            mw_true();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_type_is_morphism_3F_ (void){
    mw_dup();
    mw_type_is_morphism();
}

static void mw_type_is_physical (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_is_physical_2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_is_physical_3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_type_is_physical();
            break;
        default:
            mw_drop();
            mw_true();
            break;
    
}}

static void mw_TYPE_TYPE (void){
    mw_PRIM_TYPE_TYPE();
    mw_TPrim();
}

static void mw_TYPE_STACK (void){
    mw_PRIM_TYPE_STACK();
    mw_TPrim();
}

static void mw_TYPE_EFFECT (void){
    mw_PRIM_TYPE_EFFECT();
    mw_TPrim();
}

static void mw_TYPE_UNIT (void){
    mw_PRIM_TYPE_UNIT();
    mw_TPrim();
}

static void mw_TYPE_BOOL (void){
    mw_PRIM_TYPE_BOOL();
    mw_TPrim();
}

static void mw_TYPE_MUT (void){
    mw_PRIM_TYPE_MUT();
    mw_TPrim();
}

static void mw_TYPE_INT (void){
    mw_PRIM_TYPE_INT();
    mw_TPrim();
}

static void mw_TYPE_PTR (void){
    mw_PRIM_TYPE_PTR();
    mw_TPrim();
}

static void mw_TYPE_STR (void){
    mw_PRIM_TYPE_STR();
    mw_TPrim();
}

static void mw_TYPE_CHAR (void){
    mw_PRIM_TYPE_CHAR();
    mw_TPrim();
}

static void mw_TYPE_U8 (void){
    mw_PRIM_TYPE_U8();
    mw_TPrim();
}

static void mw_TYPE_U16 (void){
    mw_PRIM_TYPE_U16();
    mw_TPrim();
}

static void mw_TYPE_U32 (void){
    mw_PRIM_TYPE_U32();
    mw_TPrim();
}

static void mw_TYPE_U64 (void){
    mw_PRIM_TYPE_U64();
    mw_TPrim();
}

static void mw_TYPE_I8 (void){
    mw_PRIM_TYPE_I8();
    mw_TPrim();
}

static void mw_TYPE_I16 (void){
    mw_PRIM_TYPE_I16();
    mw_TPrim();
}

static void mw_TYPE_I32 (void){
    mw_PRIM_TYPE_I32();
    mw_TPrim();
}

static void mw_TYPE_I64 (void){
    mw_PRIM_TYPE_I64();
    mw_TPrim();
}

static void mw_type_expand (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_meta_expand();
            break;
        default:
            mw_id();
            break;
    
}}

static void mw_gamma_token_40_ (void){
    mw_id();
}

static void mw_gamma_token_3F_ (void){
    mw_dup();
    mw_gamma_token_40_();
}

static void mw_type_unify_failed_21_ (void){
    push_u64(0);
    push_fnptr(&mb_type_unify_failed_21__1);
    mw_prim_pack_cons();
    mw_dip2();
    push_ptr(": error: Failed to unify \0\0\0\0");
    mw_str_trace_21_();
    {
        VAL d2 = pop_value();
        mw_type_trace_21_();
        push_value(d2);
    }
    push_ptr(" with \0\0\0\0");
    mw_str_trace_21_();
    mw_type_trace_21_();
    mw_trace_ln_21_();
    mw_TYPE_ERROR();
    mw_num_errors();
    push_u64(0);
    push_fnptr(&mb_type_unify_failed_21__3);
    mw_prim_pack_cons();
    mw_modify();
}

static void mw_type_unify_21_ (void){
    mw_swap();
    mw_type_expand();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_drop();
            mw_TYPE_ERROR();
            break;
        case 1LL:
            mw_prim_drop();
            mw_id();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_type_hole_unify_21_();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_type_expand();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_drop();
                    mw_drop();
                    mw_TYPE_ERROR();
                    break;
                case 1LL:
                    mw_prim_drop();
                    mw_TMeta();
                    break;
                case 4LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TMeta();
                        push_value(d6);
                    }
                    mw_type_hole_unify_21_();
                    break;
                default:
                    mw_swap();
                    mw_meta_unify_21_();
                    break;
            
}            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_type_expand();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_drop();
                    mw_drop();
                    mw_TYPE_ERROR();
                    break;
                case 1LL:
                    mw_prim_drop();
                    mw_TVar();
                    break;
                case 4LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TVar();
                        push_value(d6);
                    }
                    mw_type_hole_unify_21_();
                    break;
                case 3LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TVar();
                        push_value(d6);
                    }
                    mw_meta_unify_21_();
                    break;
                case 5LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_type_var_unify_21_();
                    break;
                default:
                    {
                        VAL d6 = pop_value();
                        mw_TVar();
                        push_value(d6);
                    }
                    mw_type_unify_failed_21_();
                    break;
            
}            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_type_expand();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_drop();
                    mw_drop();
                    mw_TYPE_ERROR();
                    break;
                case 1LL:
                    mw_prim_drop();
                    mw_TPrim();
                    break;
                case 4LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TPrim();
                        push_value(d6);
                    }
                    mw_type_hole_unify_21_();
                    break;
                case 3LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TPrim();
                        push_value(d6);
                    }
                    mw_meta_unify_21_();
                    break;
                case 11LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TPrim();
                        push_value(d6);
                    }
                    mw_type_value_unify_21_();
                    break;
                case 2LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_type_prim_unify_21_();
                    break;
                default:
                    {
                        VAL d6 = pop_value();
                        mw_TPrim();
                        push_value(d6);
                    }
                    mw_type_unify_failed_21_();
                    break;
            
}            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_type_expand();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_drop();
                    mw_drop();
                    mw_TYPE_ERROR();
                    break;
                case 1LL:
                    mw_prim_drop();
                    mw_TData();
                    break;
                case 4LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TData();
                        push_value(d6);
                    }
                    mw_type_hole_unify_21_();
                    break;
                case 3LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TData();
                        push_value(d6);
                    }
                    mw_meta_unify_21_();
                    break;
                case 11LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TData();
                        push_value(d6);
                    }
                    mw_type_value_unify_21_();
                    break;
                case 7LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_type_data_unify_21_();
                    break;
                default:
                    {
                        VAL d6 = pop_value();
                        mw_TData();
                        push_value(d6);
                    }
                    mw_type_unify_failed_21_();
                    break;
            
}            break;
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_type_expand();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_drop();
                    mw_drop();
                    mw_TYPE_ERROR();
                    break;
                case 1LL:
                    mw_prim_drop();
                    mw_TTable();
                    break;
                case 4LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TTable();
                        push_value(d6);
                    }
                    mw_type_hole_unify_21_();
                    break;
                case 3LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TTable();
                        push_value(d6);
                    }
                    mw_meta_unify_21_();
                    break;
                case 11LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TTable();
                        push_value(d6);
                    }
                    mw_type_value_unify_21_();
                    break;
                case 6LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_type_table_unify_21_();
                    break;
                default:
                    {
                        VAL d6 = pop_value();
                        mw_TTable();
                        push_value(d6);
                    }
                    mw_type_unify_failed_21_();
                    break;
            
}            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_rotl();
            mw_type_expand();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_drop();
                    mw_drop2();
                    mw_TYPE_ERROR();
                    break;
                case 1LL:
                    mw_prim_drop();
                    mw_TTensor();
                    break;
                case 4LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TTensor();
                        push_value(d6);
                    }
                    mw_type_hole_unify_21_();
                    break;
                case 3LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TTensor();
                        push_value(d6);
                    }
                    mw_meta_unify_21_();
                    break;
                case 11LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TTensor();
                        push_value(d6);
                    }
                    mw_type_value_unify_21_();
                    break;
                case 8LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_prim_pack_uncons(); mw_prim_swap();
                    mw_type_unify_pair_21_();
                    mw_TTensor();
                    break;
                default:
                    {
                        VAL d6 = pop_value();
                        mw_TTensor();
                        push_value(d6);
                    }
                    mw_type_unify_failed_21_();
                    break;
            
}            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_rotl();
            mw_type_expand();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_drop();
                    mw_drop2();
                    mw_TYPE_ERROR();
                    break;
                case 1LL:
                    mw_prim_drop();
                    mw_TMorphism();
                    break;
                case 4LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TMorphism();
                        push_value(d6);
                    }
                    mw_type_hole_unify_21_();
                    break;
                case 3LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TMorphism();
                        push_value(d6);
                    }
                    mw_meta_unify_21_();
                    break;
                case 11LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TMorphism();
                        push_value(d6);
                    }
                    mw_type_value_unify_21_();
                    break;
                case 9LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_prim_pack_uncons(); mw_prim_swap();
                    mw_type_unify_pair_21_();
                    mw_TMorphism();
                    break;
                default:
                    {
                        VAL d6 = pop_value();
                        mw_TMorphism();
                        push_value(d6);
                    }
                    mw_type_unify_failed_21_();
                    break;
            
}            break;
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_rotl();
            mw_type_expand();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_drop();
                    mw_drop2();
                    mw_TYPE_ERROR();
                    break;
                case 1LL:
                    mw_prim_drop();
                    mw_TApp();
                    break;
                case 4LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TApp();
                        push_value(d6);
                    }
                    mw_type_hole_unify_21_();
                    break;
                case 3LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TApp();
                        push_value(d6);
                    }
                    mw_meta_unify_21_();
                    break;
                case 11LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TApp();
                        push_value(d6);
                    }
                    mw_type_value_unify_21_();
                    break;
                case 10LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_prim_pack_uncons(); mw_prim_swap();
                    mw_type_unify_pair_21_();
                    mw_TApp();
                    break;
                default:
                    {
                        VAL d6 = pop_value();
                        mw_TApp();
                        push_value(d6);
                    }
                    mw_type_unify_failed_21_();
                    break;
            
}            break;
        case 11LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_type_expand();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_drop();
                    mw_drop();
                    mw_TYPE_ERROR();
                    break;
                case 1LL:
                    mw_prim_drop();
                    mw_TValue();
                    break;
                case 4LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TValue();
                        push_value(d6);
                    }
                    mw_type_hole_unify_21_();
                    break;
                case 3LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_TValue();
                        push_value(d6);
                    }
                    mw_meta_unify_21_();
                    break;
                case 11LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_value_unify_21_();
                    break;
                default:
                    mw_value_type_unify_21_();
                    break;
            
}            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_value_unify_21_ (void){
    mw_swap();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_dup2();
                    mw__3D__3D_();
                    if (pop_u64()) {
                        mw_drop();
                        mw_VALUE_INT();
                        mw_TValue();
                    } else {
                        mw_drop2();
                        mw_TYPE_INT();
                    }
                    break;
                case 1LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_drop2();
                    mw_gamma_token_3F_();
                    push_ptr("Can't unify int value with string value.\0\0\0\0");
                    mw_emit_error_21_();
                    mw_TYPE_ERROR();
                    break;
                case 2LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_drop2();
                    mw_gamma_token_3F_();
                    push_ptr("Can't unify int value with block.\0\0\0\0");
                    mw_emit_error_21_();
                    mw_TYPE_ERROR();
                    break;
                default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
            
}            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            switch (get_top_data_tag()) {
                case 1LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_dup2();
                    mw__3D__3D_();
                    if (pop_u64()) {
                        mw_drop();
                        mw_VALUE_STR();
                        mw_TValue();
                    } else {
                        mw_drop2();
                        mw_TYPE_STR();
                    }
                    break;
                case 0LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_drop2();
                    mw_gamma_token_3F_();
                    push_ptr("Can't unify string value with int value.\0\0\0\0");
                    mw_emit_error_21_();
                    mw_TYPE_ERROR();
                    break;
                case 2LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_drop2();
                    mw_gamma_token_3F_();
                    push_ptr("Can't unify string value with block.\0\0\0\0");
                    mw_emit_error_21_();
                    mw_TYPE_ERROR();
                    break;
                default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
            
}            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            switch (get_top_data_tag()) {
                case 2LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_dup2();
                    mw__3D__3D_();
                    if (pop_u64()) {
                        mw_drop();
                        mw_VALUE_BLOCK();
                        mw_TValue();
                    } else {
                        mw_block_infer_type_21_();
                        mw_block_unify_type_21_();
                    }
                    break;
                case 0LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_drop2();
                    mw_gamma_token_3F_();
                    push_ptr("Can't unify block with int value.\0\0\0\0");
                    mw_emit_error_21_();
                    mw_TYPE_ERROR();
                    break;
                case 1LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_drop2();
                    mw_gamma_token_3F_();
                    push_ptr("Can't unify block with string value.\0\0\0\0");
                    mw_emit_error_21_();
                    mw_TYPE_ERROR();
                    break;
                default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
            
}            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_type_value_unify_21_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_TYPE_INT();
            mw_type_unify_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_TYPE_STR();
            mw_type_unify_21_();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_block_unify_type_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_value_type_unify_21_ (void){
    mw_swap();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_TYPE_INT();
            mw_swap();
            mw_type_unify_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_TYPE_STR();
            mw_swap();
            mw_type_unify_21_();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_block_unify_type_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_arrow_type (void){
    mw_dup();
    mw_arrow_dom();
    mw__40_();
    mw_swap();
    mw_arrow_cod();
    mw__40_();
    mw_T__3E_();
}

static void mw_block_infer_type_21_ (void){
    mw_block_arrow();
    mw_force_21_();
    mw_arrow_type();
}

static void mw_type_unify_pair_21_ (void){
    {
        VAL d2 = pop_value();
        mw_swap();
        {
            VAL d3 = pop_value();
            mw_type_unify_21_();
            mw_swap();
            push_value(d3);
        }
        push_value(d2);
    }
    mw_type_unify_21_();
    {
        VAL d2 = pop_value();
        mw_swap();
        push_value(d2);
    }
}

static void mw_type_prim_unify_21_ (void){
    mw_dup2();
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_TPrim();
    } else {
        {
            VAL d3 = pop_value();
            mw_TPrim();
            push_value(d3);
        }
        mw_TPrim();
        mw_type_unify_failed_21_();
    }
}

static void mw_type_data_unify_21_ (void){
    mw_dup2();
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_TData();
    } else {
        {
            VAL d3 = pop_value();
            mw_TData();
            push_value(d3);
        }
        mw_TData();
        mw_type_unify_failed_21_();
    }
}

static void mw_type_table_unify_21_ (void){
    mw_dup2();
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_TTable();
    } else {
        {
            VAL d3 = pop_value();
            mw_TTable();
            push_value(d3);
        }
        mw_TTable();
        mw_type_unify_failed_21_();
    }
}

static void mw_type_var_unify_21_ (void){
    mw_dup2();
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_TVar();
    } else {
        {
            VAL d3 = pop_value();
            mw_TVar();
            push_value(d3);
        }
        mw_TVar();
        mw_type_unify_failed_21_();
    }
}

static void mw_type_has_meta_3F_ (void){
    mw_dup2();
    mw_type_has_meta();
}

static void mw_type_has_meta (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_meta_has_meta();
            break;
        case 0LL:
            mw_prim_drop();
            mw_drop();
            mw_false();
            break;
        case 1LL:
            mw_prim_drop();
            mw_drop();
            mw_false();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop2();
            mw_false();
            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop2();
            mw_false();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop2();
            mw_false();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_type2_has_meta();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_type2_has_meta();
            break;
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_type2_has_meta();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop2();
            mw_false();
            break;
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop2();
            mw_false();
            break;
        case 11LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_value_type_has_meta();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_type2_has_meta (void){
    mw_over2();
    mw_swap();
    mw_type_has_meta();
    if (pop_u64()) {
        mw_drop2();
        mw_true();
    } else {
        mw_type_has_meta();
    }
}

static void mw_meta_has_meta (void){
    mw_dup();
    mw_meta_type();
    mw__40_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw__3D__3D_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_type_has_meta();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_value_type_has_meta (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop2();
            mw_false();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop2();
            mw_false();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_value_type_has_meta_4);
            mw_prim_pack_cons();
            mw_sip();
            mw_block_cod();
            mw__40_();
            mw_type2_has_meta();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_type_trace_sig_21_ (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_trace_sig_21__2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_trace_sig_21__3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 0LL:
            mw_prim_drop();
            push_ptr("<ERROR>\0\0\0\0");
            mw_str_trace_21_();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_swap();
            mw_type_trace_stack_dom_21_();
            push_ptr("--\0\0\0\0");
            mw_str_trace_21_();
            mw_type_trace_stack_cod_21_();
            break;
        default:
            mw_type_trace_stack_21_();
            break;
    
}}

static void mw_type_trace_stack_dom_21_ (void){
    mw_type_expand();
    mw_dup();
    mw_TYPE_UNIT();
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
    } else {
        mw_type_trace_stack_21_();
        push_ptr(" \0\0\0\0");
        mw_str_trace_21_();
    }
}

static void mw_type_trace_stack_cod_21_ (void){
    mw_type_expand();
    mw_dup();
    mw_TYPE_UNIT();
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
    } else {
        push_ptr(" \0\0\0\0");
        mw_str_trace_21_();
        mw_type_trace_stack_21_();
    }
}

static void mw_type_trace_stack_21_ (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_trace_stack_21__2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_trace_stack_21__3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_swap();
            mw_type_trace_stack_dom_21_();
            mw_type_trace_21_();
            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_var_name();
            mw__40_();
            mw_dup();
            mw_name_trace_21_();
            mw_name_could_be_stack_var();
            if (pop_u64()) {
                mw_id();
            } else {
                push_ptr(" .\0\0\0\0");
                mw_str_trace_21_();
            }
            break;
        default:
            mw_type_trace_21_();
            break;
    
}}

static void mw_type_trace_21_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            push_ptr("<ERROR>\0\0\0\0");
            mw_str_trace_21_();
            break;
        case 1LL:
            mw_prim_drop();
            push_ptr("_\0\0\0\0");
            mw_str_trace_21_();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_type_trace_prim_21_();
            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_var_name();
            mw__40_();
            mw_name_trace_21_();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_meta_trace_21_();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            push_ptr("[\0\0\0\0");
            mw_str_trace_21_();
            mw_TTensor();
            mw_type_trace_stack_21_();
            push_ptr("]\0\0\0\0");
            mw_str_trace_21_();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            push_ptr("[\0\0\0\0");
            mw_str_trace_21_();
            mw_TMorphism();
            mw_type_trace_sig_21_();
            push_ptr("]\0\0\0\0");
            mw_str_trace_21_();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_data_name();
            mw__40_();
            mw_name_trace_21_();
            break;
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_table_name();
            mw__40_();
            mw_name_trace_21_();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_name_trace_21_();
            break;
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_app_type_trace_21_();
            break;
        case 11LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_value_as_type();
            mw_type_trace_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_value_as_type (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_PRIM_TYPE_INT();
            mw_TPrim();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_PRIM_TYPE_STR();
            mw_TPrim();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_value_as_type_4);
            mw_prim_pack_cons();
            mw_sip();
            mw_block_cod();
            mw__40_();
            mw_T__3E_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_type_trace_prim_21_ (void){
    switch (get_top_data_tag()) {
        case 1LL:
            mw_prim_drop();
            push_ptr("<TYPE>\0\0\0\0");
            break;
        case 2LL:
            mw_prim_drop();
            push_ptr("<STACK>\0\0\0\0");
            break;
        case 3LL:
            mw_prim_drop();
            push_ptr("<EFFECT>\0\0\0\0");
            break;
        case 0LL:
            mw_prim_drop();
            push_ptr("[]\0\0\0\0");
            break;
        case 8LL:
            mw_prim_drop();
            push_ptr("Bool\0\0\0\0");
            break;
        case 4LL:
            mw_prim_drop();
            push_ptr("Int\0\0\0\0");
            break;
        case 5LL:
            mw_prim_drop();
            push_ptr("Ptr\0\0\0\0");
            break;
        case 6LL:
            mw_prim_drop();
            push_ptr("Str\0\0\0\0");
            break;
        case 7LL:
            mw_prim_drop();
            push_ptr("Char\0\0\0\0");
            break;
        case 12LL:
            mw_prim_drop();
            push_ptr("U8\0\0\0\0");
            break;
        case 11LL:
            mw_prim_drop();
            push_ptr("U16\0\0\0\0");
            break;
        case 10LL:
            mw_prim_drop();
            push_ptr("U32\0\0\0\0");
            break;
        case 9LL:
            mw_prim_drop();
            push_ptr("U64\0\0\0\0");
            break;
        case 16LL:
            mw_prim_drop();
            push_ptr("I8\0\0\0\0");
            break;
        case 15LL:
            mw_prim_drop();
            push_ptr("I16\0\0\0\0");
            break;
        case 14LL:
            mw_prim_drop();
            push_ptr("I32\0\0\0\0");
            break;
        case 13LL:
            mw_prim_drop();
            push_ptr("I64\0\0\0\0");
            break;
        case 17LL:
            mw_prim_drop();
            push_ptr("Mut\0\0\0\0");
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}    mw_str_trace_21_();
}

static void mw_type_semifreshen_sig (void){
    mw_dup();
    mw_type_sig_needs_fresh_stack_rest();
    if (pop_u64()) {
        mw_type_semifreshen_sig_aux();
    } else {
        mw_id();
    }
}

static void mw_type_semifreshen_sig_aux (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_semifreshen_sig_aux_2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_semifreshen_sig_aux_3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_meta_alloc_21_();
            mw_TMeta();
            mw_rotr();
            {
                VAL d4 = pop_value();
                mw_type_semifreshen_sig_stack();
                push_value(d4);
            }
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_type_semifreshen_sig_stack();
                push_value(d4);
            }
            mw_swap();
            mw_TMorphism();
            mw_nip();
            break;
        default:
            mw_id();
            break;
    
}}

static void mw_type_semifreshen_sig_stack (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_semifreshen_sig_stack_2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_semifreshen_sig_stack_3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_type_semifreshen_sig_stack();
                push_value(d4);
            }
            mw_TTensor();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_drop();
                    mw_dup();
                    break;
                default:
                    mw_TPrim();
                    break;
            
}            break;
        default:
            mw_id();
            break;
    
}}

static void mw_type_freshen_sig (void){
    mw_dup();
    mw_type_sig_needs_fresh_stack_rest();
    if (pop_u64()) {
        mw_type_freshen_sig_aux();
    } else {
        mw_type_freshen();
    }
}

static void mw_type_stack_rest (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_stack_rest_2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_stack_rest_3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            mw_type_stack_rest();
            break;
        default:
            mw_id();
            break;
    
}}

static void mw_type_sig_needs_fresh_stack_rest (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_sig_needs_fresh_stack_rest_2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_sig_needs_fresh_stack_rest_3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_type_stack_rest();
            mw_TYPE_UNIT();
            mw__3D__3D_();
            if (pop_u64()) {
                mw_type_stack_rest();
                mw_TYPE_UNIT();
                mw__3D__3D_();
            } else {
                mw_drop();
                mw_false();
            }
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_type_freshen_sig_aux (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_freshen_sig_aux_2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_freshen_sig_aux_3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_meta_alloc_21_();
            mw_TMeta();
            mw_rot4r();
            {
                VAL d4 = pop_value();
                mw_type_freshen_sig_stack();
                push_value(d4);
            }
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_type_freshen_sig_stack();
                push_value(d4);
            }
            mw_swap();
            mw_TMorphism();
            {
                VAL d4 = pop_value();
                mw_nip();
                push_value(d4);
            }
            break;
        default:
            mw_type_freshen();
            break;
    
}}

static void mw_type_freshen_sig_stack (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_freshen_sig_stack_2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_freshen_sig_stack_3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_type_freshen_sig_stack();
                push_value(d4);
            }
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_type_freshen();
                push_value(d4);
            }
            mw_swap();
            mw_TTensor();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_drop();
                    mw_over();
                    break;
                default:
                    mw_TPrim();
                    break;
            
}            break;
        default:
            mw_type_freshen();
            break;
    
}}

static void mw_type_freshen (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_TYPE_ERROR();
            break;
        case 1LL:
            mw_prim_drop();
            mw_TYPE_DONT_CARE();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_TPrim();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_THole();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_TData();
            break;
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_TTable();
            break;
        case 11LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_TValue();
            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_type_var_freshen();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_meta_freshen();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_type_pair_freshen();
            mw_TTensor();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_type_pair_freshen();
            mw_TMorphism();
            break;
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_type_pair_freshen();
            mw_TApp();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_type_pair_freshen (void){
    {
        VAL d2 = pop_value();
        mw_type_freshen();
        mw_swap();
        push_value(d2);
    }
    mw_type_freshen();
    {
        VAL d2 = pop_value();
        mw_swap();
        push_value(d2);
    }
}

static void mw_meta_freshen (void){
    push_u64(0);
    push_fnptr(&mb_meta_freshen_1);
    mw_prim_pack_cons();
    push_u64(0);
    push_fnptr(&mb_meta_freshen_2);
    mw_prim_pack_cons();
    mw_meta_expand_if();
}

static void mw_type_var_freshen (void){
    mw_swap();
    mw_subst_has_var_3F_();
    if (pop_u64()) {
        mw_tuck();
        mw_subst_get_var();
    } else {
        mw_meta_alloc_21_();
        mw_TMeta();
        mw_dup();
        {
            VAL d3 = pop_value();
            mw_rotr();
            mw_subst_new_21_();
            push_value(d3);
        }
    }
}

static void mw_type_rigidify_sig_21_ (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_rigidify_sig_21__2);
            mw_prim_pack_cons();
            mw_meta_expand_or_update_21_();
            mw_type_rigidify_sig_21_();
            break;
        default:
            mw_type_rigidify_21_();
            break;
    
}}

static void mw_type_rigidify_stack_21_ (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_rigidify_stack_21__2);
            mw_prim_pack_cons();
            mw_meta_expand_or_update_21_();
            mw_type_rigidify_stack_21_();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_type_rigidify_stack_21_();
                push_value(d4);
            }
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_type_rigidify_21_();
                push_value(d4);
            }
            mw_swap();
            mw_TTensor();
            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_TVar();
            break;
        default:
            mw_type_rigidify_21_();
            break;
    
}}

static void mw_type_rigidify_21_ (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_rigidify_21__2);
            mw_prim_pack_cons();
            mw_meta_expand_or_update_21_();
            mw_type_rigidify_21_();
            break;
        case 0LL:
            mw_prim_drop();
            mw_TYPE_ERROR();
            break;
        case 1LL:
            mw_prim_drop();
            mw_TYPE_DONT_CARE();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_TPrim();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_THole();
            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_TVar();
            break;
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_TTable();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_TData();
            break;
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_type_rigidify_21_();
                push_value(d4);
            }
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_type_rigidify_21_();
                push_value(d4);
            }
            mw_swap();
            mw_TApp();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_type_rigidify_stack_21_();
                push_value(d4);
            }
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_type_rigidify_21_();
                push_value(d4);
            }
            mw_swap();
            mw_TTensor();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            {
                VAL d4 = pop_value();
                mw_type_rigidify_stack_21_();
                push_value(d4);
            }
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_type_rigidify_stack_21_();
                push_value(d4);
            }
            mw_swap();
            mw_TMorphism();
            break;
        case 11LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_type_rigidify_value_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_type_rigidify_value_21_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_VALUE_INT();
            mw_TValue();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_VALUE_STR();
            mw_TValue();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_block_arrow();
            mw_force_21_();
            mw_arrow_type();
            mw_type_rigidify_sig_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_prim_type_arity (void){
    switch (get_top_data_tag()) {
        case 17LL:
            mw_prim_drop();
            push_i64(1LL);
            break;
        default:
            mw_drop();
            push_i64(0LL);
            break;
    
}}

static void mw_type_arity (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_arity_2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_arity_3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_data_arity();
            mw__40_();
            break;
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            mw_type_arity();
            mw_1_();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_type_arity();
            break;
        default:
            mw_drop();
            push_i64(0LL);
            break;
    
}}

static void mw_type_head (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_head_2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_head_3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            mw_type_head();
            break;
        default:
            mw_id();
            break;
    
}}

static void mw_type_max_count (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_max_count_2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_max_count_3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            mw_type_max_count();
            break;
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_table_max_count();
            mw__40_();
            mw_1_2B_();
            mw_SOME();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_data_is_enum_3F_();
            if (pop_u64()) {
                mw_data_tags();
                mw__40_();
                mw_len();
                mw_SOME();
            } else {
                mw_drop();
                mw_NONE();
            }
            break;
        default:
            mw_drop();
            mw_NONE();
            break;
    
}}

static void mw_meta_trace_21_ (void){
    mw_dup();
    mw_meta_type();
    mw__40_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            push_ptr("?\0\0\0\0");
            mw_str_trace_21_();
            mw_MetaVar_2E_id();
            mw_int_trace_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_type_trace_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_meta_alloc_21_ (void){
    mw_MetaVar_2E_alloc_21_();
}

static void mw_meta_expand_if (void){
    {
        VAL var_g = pop_value();
        VAL var_f = pop_value();
        mw_dup();
        mw_meta_type();
        mw__40_();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                push_value(var_g);
                incref(var_g);
                mw_prim_run();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_type_expand();
                mw_tuck();
                mw_SOME();
                mw_swap();
                mw_meta_type();
                mw__21_();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_g);
        decref(var_f);
    }
}

static void mw_meta_expand (void){
    push_u64(0);
    push_fnptr(&mb_meta_expand_1);
    mw_prim_pack_cons();
    push_u64(0);
    push_fnptr(&mb_meta_expand_2);
    mw_prim_pack_cons();
    mw_meta_expand_if();
}

static void mw_meta_unify_21_ (void){
    mw_dup();
    mw_meta_type();
    mw__40_();
    switch (get_top_data_tag()) {
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_type_unify_21_();
            break;
        case 0LL:
            mw_prim_drop();
            mw_dup2();
            mw_TMeta();
            mw__3D__3D_();
            if (pop_u64()) {
                mw_drop();
            } else {
                mw_swap();
                mw_type_has_meta_3F_();
                if (pop_u64()) {
                    mw_swap();
                    mw_TMeta();
                    mw_type_unify_failed_21_();
                } else {
                    mw_tuck();
                    mw_SOME();
                    mw_swap();
                    mw_meta_type();
                    mw__21_();
                }
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_meta_expand_or_update_21_ (void){
    {
        VAL var_f = pop_value();
        mw_dup();
        mw_meta_type();
        mw__40_();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                {
                    VAL d5 = pop_value();
                    push_value(var_f);
                    incref(var_f);
                    mw_prim_run();
                    mw_dup();
                    mw_SOME();
                    push_value(d5);
                }
                mw_meta_type();
                mw__21_();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_nip();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        decref(var_f);
    }
}

static void mw_type_hole_unify_21_ (void){
    mw_is_nil_3F_();
    if (pop_u64()) {
        mw_drop();
    } else {
        mw_THole();
        mw_type_trace_21_();
        push_ptr(" ~ \0\0\0\0");
        mw_str_trace_21_();
        mw_dup();
        mw_type_trace_21_();
        mw_trace_ln_21_();
    }
}

static void mw_type_max_num_params (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_max_num_params_2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_max_num_params_3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            mw_type_num_morphisms_on_top();
            break;
        default:
            mw_drop();
            push_i64(0LL);
            break;
    
}}

static void mw_type_num_morphisms_on_top (void){
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_type_num_morphisms_on_top_2);
            mw_prim_pack_cons();
            push_u64(0);
            push_fnptr(&mb_type_num_morphisms_on_top_3);
            mw_prim_pack_cons();
            mw_meta_expand_if();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_type_is_morphism();
            if (pop_u64()) {
                mw_type_num_morphisms_on_top();
                mw_1_2B_();
            } else {
                mw_drop();
                push_i64(0LL);
            }
            break;
        default:
            mw_drop();
            push_i64(0LL);
            break;
    
}}

static void mw_app_type_trace_21_ (void){
    mw_app_type_trace_open_21_();
    push_ptr(")\0\0\0\0");
    mw_str_trace_21_();
}

static void mw_app_type_trace_open_21_ (void){
    mw_swap();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_app_type_trace_open_21_();
            push_ptr(", \0\0\0\0");
            mw_str_trace_21_();
            mw_type_trace_21_();
            break;
        default:
            mw_type_trace_21_();
            push_ptr("(\0\0\0\0");
            mw_str_trace_21_();
            mw_type_trace_21_();
            break;
    
}}

static void mw_unSUBST (void){
    mw_id();
}

static void mw_subst_nil (void){
    mw_nil();
}

static void mw_subst_is_nil (void){
    mw_is_nil();
}

static void mw_subst_is_nil_3F_ (void){
    mw_is_nil_3F_();
}

static void mw_subst_new_21_ (void){
    mw_unSUBST();
    mw_rotr();
    mw_pack2();
    mw_map_insert();
    mw_SUBST();
}

static void mw_subst_has_var (void){
    mw_unSUBST();
    mw_swap();
    mw_map_has();
}

static void mw_subst_has_var_3F_ (void){
    mw_dup2();
    mw_subst_has_var();
}

static void mw_subst_get_var (void){
    mw_unSUBST();
    mw_swap();
    mw_map_lookup();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_TYPE_ERROR();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_id();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_subst_get_var_3F_ (void){
    mw_dup2();
    mw_subst_get_var();
}

static void mw_subst_match_var (void){
    mw_subst_has_var_3F_();
    if (pop_u64()) {
        push_u64(0);
        push_fnptr(&mb_subst_match_var_2);
        mw_prim_pack_cons();
        mw_sip();
    } else {
        mw_subst_new_21_();
    }
}

static void mw_tag_num_inputs_3F_ (void){
    mw_dup();
    mw_tag_sig();
    mw__40_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            push_i64(0LL);
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_i64(0LL);
            mw_swap();
            while(1) {
                mw_token_run_end_3F_();
                mw_not();
                if (!pop_u64()) break;
                mw_token_next();
                {
                    VAL d5 = pop_value();
                    mw_1_2B_();
                    push_value(d5);
                }
            }
            mw_drop();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_tag_is_transparent_3F_ (void){
    mw_dup();
    mw_tag_data();
    mw__40_();
    mw_data_is_transparent();
}

static void mw_data_num_tags (void){
    mw_data_tags();
    mw__40_();
    mw_len();
}

static void mw_data_add_tag_21_ (void){
    mw_dup2();
    mw_data_num_tags();
    mw_swap();
    mw_tag_value();
    mw__21_();
    mw_dup();
    mw_data_tags();
    mw__40_();
    mw_rotr();
    {
        VAL d2 = pop_value();
        mw_snoc();
        push_value(d2);
    }
    mw_data_tags();
    mw__21_();
}

static void mw_data_is_enum_3F_ (void){
    mw_dup();
    mw_data_tags();
    mw__40_();
    push_u64(0);
    push_fnptr(&mb_data_is_enum_3F__1);
    mw_prim_pack_cons();
    mw_all_3F_();
    mw_nip();
}

static void mw_data_is_transparent (void){
    mw_data_tags();
    mw__40_();
    switch (get_top_data_tag()) {
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_tag_num_inputs_3F_();
            push_i64(1LL);
            mw__3D__3D_();
            mw_nip();
            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_Atom_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Atom_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Atom_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Atom_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Atom_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Atom_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Atom_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Atom_2E_alloc_21_ (void){
    mw_Atom_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Atom_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_Arrow_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Arrow_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Arrow_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Arrow_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Arrow_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Arrow_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Arrow_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Arrow_2E_alloc_21_ (void){
    mw_Arrow_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Arrow_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_Lambda_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Lambda_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Lambda_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Lambda_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Lambda_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Lambda_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Lambda_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Lambda_2E_alloc_21_ (void){
    mw_Lambda_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Lambda_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_Block_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Block_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Block_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Block_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Block_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Block_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Block_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Block_2E_alloc_21_ (void){
    mw_Block_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Block_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_unPARAM (void){
    mw_id();
}

static void mw_Var__3E_Param (void){
    mw_PARAM();
}

static void mw_Param__3E_Var (void){
    mw_id();
}

static void mw_atom_arg_add_left_21_ (void){
    mw_over();
    mw_atom_args();
    push_u64(0);
    push_fnptr(&mb_atom_arg_add_left_21__1);
    mw_prim_pack_cons();
    mw_modify();
}

static void mw_arrow_atom_add_21_ (void){
    mw_over();
    mw_arrow_atoms();
    push_u64(0);
    push_fnptr(&mb_arrow_atom_add_21__1);
    mw_prim_pack_cons();
    mw_modify();
}

static void mw_block_new_21_ (void){
    mw_Block_2E_alloc_21_();
    mw_over();
    mw_arrow_ctx();
    mw__40_();
    mw_over();
    mw_block_ctx();
    mw__21_();
    mw_over();
    mw_arrow_token_start();
    mw__40_();
    mw_over();
    mw_block_token();
    mw__21_();
    mw_over();
    mw_arrow_dom();
    mw__40_();
    mw_over();
    mw_block_dom();
    mw__21_();
    mw_over();
    mw_arrow_cod();
    mw__40_();
    mw_over();
    mw_block_cod();
    mw__21_();
    mw_swap();
    mw_ready();
    mw_over();
    mw_block_arrow();
    mw__21_();
}

static void mw_block_new_deferred_21_ (void){
    mw_Block_2E_alloc_21_();
    mw_tuck();
    mw_block_token();
    mw__21_();
    mw_tuck();
    mw_block_ctx();
    mw__21_();
    mw_meta_alloc_21_();
    mw_TMeta();
    mw_over();
    mw_block_dom();
    mw__21_();
    mw_meta_alloc_21_();
    mw_TMeta();
    mw_over();
    mw_block_cod();
    mw__21_();
    mw_dup();
    push_u64(0);
    push_fnptr(&mb_block_new_deferred_21__1);
    mw_prim_pack_cons();
    mw_delay();
    mw_over();
    mw_block_arrow();
    mw__21_();
}

static void mw_block_force_21_ (void){
    mw_block_arrow();
    mw_force_21_();
    mw_drop();
}

static void mw_block_unify_type_21_ (void){
    mw_over2();
    mw_gamma_token_40_();
    mw_elab_expand_morphism_21_();
    mw_drop();
    {
        VAL d2 = pop_value();
        mw_swap();
        push_u64(0);
        push_fnptr(&mb_block_unify_type_21__2);
        mw_prim_pack_cons();
        mw_sip();
        push_value(d2);
    }
    mw_swap();
    push_u64(0);
    push_fnptr(&mb_block_unify_type_21__3);
    mw_prim_pack_cons();
    mw_sip();
    mw_block_arrow();
    mw_force_21_();
    mw_arrow_type();
}

static void mw_Match_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Match_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Match_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Match_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Match_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Match_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Match_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Match_2E_alloc_21_ (void){
    mw_Match_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Match_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_Case_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Case_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Case_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Case_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Case_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Case_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Case_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Case_2E_alloc_21_ (void){
    mw_Case_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Case_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_match_add_case_21_ (void){
    {
        VAL d2 = pop_value();
        mw_dup();
        mw_match_cases();
        mw__40_();
        push_value(d2);
    }
    mw_cases_cover_case_3F_();
    if (pop_u64()) {
        mw_case_token();
        mw__40_();
        push_ptr("Case is unreachable.\0\0\0\0");
        mw_emit_error_21_();
        mw_drop();
    } else {
        mw_snoc();
        mw_over();
        mw_match_cases();
        mw__21_();
    }
}

static void mw_match_is_exhaustive_3F_ (void){
    mw_match_has_default_case_3F_();
    if (pop_u64()) {
        mw_true();
    } else {
        mw_match_scrutinee_data_3F_();
        switch (get_top_data_tag()) {
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_data_num_tags();
                mw_over();
                mw_match_cases();
                mw__40_();
                mw_len();
                mw__3D__3D_();
                break;
            case 0LL:
                mw_prim_drop();
                mw_true();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}    }
}

static void mw_match_has_default_case_3F_ (void){
    mw_dup();
    mw_match_cases();
    mw__40_();
    mw_cases_have_default_case();
}

static void mw_match_scrutinee_type_3F_ (void){
    mw_dup();
    mw_match_dom();
    mw__40_();
    mw_type_expand();
    switch (get_top_data_tag()) {
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_nip();
            mw_SOME();
            break;
        default:
            mw_drop();
            mw_NONE();
            break;
    
}}

static void mw_match_scrutinee_data_3F_ (void){
    mw_match_scrutinee_type_3F_();
    push_u64(0);
    push_fnptr(&mb_match_scrutinee_data_3F__1);
    mw_prim_pack_cons();
    mw_maybe_bind();
}

static void mw_match_is_transparent_3F_ (void){
    mw_match_scrutinee_data_3F_();
    switch (get_top_data_tag()) {
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_data_is_transparent();
            break;
        case 0LL:
            mw_prim_drop();
            mw_false();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_cases_cover_case_3F_ (void){
    mw_dup2();
    mw_cases_cover_case();
}

static void mw_cases_cover_case (void){
    mw_swap();
    push_u64(0);
    push_fnptr(&mb_cases_cover_case_1);
    mw_prim_pack_cons();
    mw_any();
    mw_nip();
}

static void mw_case_is_covered (void){
    {
        VAL d2 = pop_value();
        mw_case_pattern();
        mw__40_();
        push_value(d2);
    }
    mw_case_pattern();
    mw__40_();
    mw_pattern_is_covered();
}

static void mw_cases_have_default_case (void){
    push_u64(0);
    push_fnptr(&mb_cases_have_default_case_1);
    mw_prim_pack_cons();
    mw_any();
}

static void mw_case_is_default_case (void){
    mw_case_pattern();
    mw__40_();
    mw_PATTERN_UNDERSCORE();
    mw__3D__3D_();
}

static void mw_pattern_is_covered (void){
    mw_dup();
    mw_PATTERN_UNDERSCORE();
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop2();
        mw_true();
    } else {
        mw__3D__3D_();
    }
}

static void mw_ready (void){
    mw_LAZY_READY();
}

static void mw_ready2 (void){
    mw_pack2();
    mw_LAZY_READY();
}

static void mw_delay (void){
    mw_LAZY_DELAY();
}

static void mw_delay0 (void){
    push_u64(0);
    push_fnptr(&mb_delay0_1);
    mw_prim_pack_cons();
    mw_delay();
}

static void mw_delay2 (void){
    mw_pack3();
    push_u64(0);
    push_fnptr(&mb_delay2_1);
    mw_prim_pack_cons();
    mw_delay();
}

static void mw_delay3 (void){
    mw_pack4();
    push_u64(0);
    push_fnptr(&mb_delay3_1);
    mw_prim_pack_cons();
    mw_delay();
}

static void mw_delay4 (void){
    mw_pack5();
    push_u64(0);
    push_fnptr(&mb_delay4_1);
    mw_prim_pack_cons();
    mw_delay();
}

static void mw_force_21_ (void){
    mw_dup();
    mw__40_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_rotl();
            mw_LAZY_WAIT();
            mw_over();
            mw__21_();
            {
                VAL d4 = pop_value();
                mw_run();
                mw_dup();
                mw_LAZY_READY();
                push_value(d4);
            }
            mw__21_();
            break;
        case 2LL:
            mw_prim_drop();
            push_ptr("attempted to force already running thunk\0\0\0\0");
            mw_panic_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_force_or_21_ (void){
    {
        VAL var_f = pop_value();
        mw_dup();
        mw__40_();
        switch (get_top_data_tag()) {
            case 2LL:
                mw_prim_drop();
                mw_drop();
                push_value(var_f);
                incref(var_f);
                mw_prim_run();
                break;
            default:
                mw_drop();
                mw_force_21_();
                break;
        
}        decref(var_f);
    }
}

static void mw_force2_21_ (void){
    mw_force_21_();
    mw_unpack2();
}

static void mw_force_or2_21_ (void){
    {
        VAL var_f = pop_value();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_force_or2_21__2);
        mw_prim_pack_cons();
        mw_force_or_21_();
        mw_unpack2();
        decref(var_f);
    }
}

static void mw_Var_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Var_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Var_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Var_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Var_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Var_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Var_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Var_2E_alloc_21_ (void){
    mw_Var_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Var_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_var_new_21_ (void){
    mw_Var_2E_alloc_21_();
    mw_tuck();
    mw_var_name();
    mw__21_();
}

static void mw_var_new_implicit_21_ (void){
    mw_var_new_21_();
    mw_true();
    mw_over();
    mw_var_is_implicit();
    mw__21_();
}

static void mw_var_is_physical (void){
    mw_var_type();
    mw__40_();
    mw_type_is_physical();
}

static void mw_Word_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Word_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Word_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Word_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Word_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Word_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Word_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Word_2E_alloc_21_ (void){
    mw_Word_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Word_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_Table_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Table_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Table_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Table_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Table_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Table_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Table_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Table_2E_alloc_21_ (void){
    mw_Table_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Table_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_Field_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Field_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Field_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Field_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Field_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Field_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Field_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Field_2E_alloc_21_ (void){
    mw_Field_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Field_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_CODEGEN_BUF_SIZE (void){
    push_i64(256LL);
}

static void mw_codegen_u8_40_ (void){
    mw_CODEGEN_BUF();
    mw_u8_40__40_();
}

static void mw_codegen_u8_21_ (void){
    mw_CODEGEN_BUF();
    mw_u8_21__21_();
}

static void mw_codegen_full_3F_ (void){
    mw_codegen_length();
    mw__40_();
    push_i64(4LL);
    mw__2B_();
    mw_CODEGEN_BUF_SIZE();
    mw__3E__3D_();
}

static void mw_codegen_flush_21_ (void){
    mw_codegen_length();
    mw__40_();
    push_i64(0LL);
    mw__3E_();
    if (pop_u64()) {
        mw_codegen_file();
        mw__40_();
        mw_File__3E_Int();
        mw_CODEGEN_BUF();
        mw_codegen_length();
        mw__40_();
        mw_posix_write_21_();
        mw_dup();
        push_i64(0LL);
        mw__3C_();
        if (pop_u64()) {
            push_ptr("error: codegen write failed\0\0\0\0");
            mw_panic_21_();
        } else {
            mw_codegen_length();
            mw__40_();
            mw__3C_();
            if (pop_u64()) {
                push_ptr("error: codegen write wrote fewer bytes than expected\0\0\0\0");
                mw_panic_21_();
            } else {
                push_i64(0LL);
                mw_codegen_length();
                mw__21_();
            }
        }
    } else {
        mw_id();
    }
}

static void mw__2E_b (void){
    mw_codegen_full_3F_();
    if (pop_u64()) {
        mw_codegen_flush_21_();
    } else {
        mw_id();
    }
    mw_codegen_length();
    push_u64(0);
    push_fnptr(&mb__2E_b_3);
    mw_prim_pack_cons();
    mw_modify();
    mw_codegen_u8_21_();
}

static void mw__2E_c (void){
    mw_codegen_full_3F_();
    if (pop_u64()) {
        mw_codegen_flush_21_();
    } else {
        mw_id();
    }
    mw_dup();
    mw_codegen_length();
    mw__40_();
    mw_CODEGEN_BUF();
    push_u64(0);
    push_fnptr(&mb__2E_c_3);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
    mw_char_width();
    mw_codegen_length();
    push_u64(0);
    push_fnptr(&mb__2E_c_4);
    mw_prim_pack_cons();
    mw_modify();
}

static void mw__2E_ (void){
    mw_dup();
    mw_Str__3E_Ptr();
    mw_swap();
    mw_str_size();
    mw_dup();
    mw_codegen_length();
    mw__40_();
    mw__2B_();
    mw_CODEGEN_BUF_SIZE();
    mw__3E_();
    if (pop_u64()) {
        mw_codegen_flush_21_();
        while(1) {
            mw_dup();
            mw_CODEGEN_BUF_SIZE();
            mw__3E_();
            if (!pop_u64()) break;
            mw_over();
            mw_CODEGEN_BUF_SIZE();
            mw_CODEGEN_BUF();
            mw_prim_ptr_copy();
            mw_CODEGEN_BUF_SIZE();
            mw_codegen_length();
            mw__21_();
            mw_codegen_flush_21_();
            {
                VAL d4 = pop_value();
                mw_CODEGEN_BUF_SIZE();
                mw_swap();
                mw_ptr_2B_();
                push_value(d4);
            }
            mw_CODEGEN_BUF_SIZE();
            mw__();
        }
        mw_dup();
        mw_codegen_length();
        mw__21_();
        mw_CODEGEN_BUF();
        mw_prim_ptr_copy();
    } else {
        push_u64(0);
        push_fnptr(&mb__2E__6);
        mw_prim_pack_cons();
        mw_sip();
        mw_codegen_length();
        push_u64(0);
        push_fnptr(&mb__2E__7);
        mw_prim_pack_cons();
        mw_modify();
    }
}

static void mw_codegen_start_21_ (void){
    mw_codegen_file();
    mw__21_();
    push_i64(0LL);
    mw_codegen_length();
    mw__21_();
}

static void mw_codegen_end_21_ (void){
    mw_codegen_flush_21_();
    mw_codegen_file();
    mw__40_();
    mw_close_file_21_();
    mw_stdout();
    mw_codegen_file();
    mw__21_();
    push_i64(0LL);
    mw_codegen_length();
    mw__21_();
}

static void mw_run_output_c99_21_ (void){
    mw_num_errors();
    mw__40_();
    push_i64(0LL);
    mw__3E_();
    if (pop_u64()) {
        mw_drop2();
    } else {
        mw_make_output_path();
        mw_Path__3E_Str();
        mw_create_file_21_();
        mw_codegen_start_21_();
        mw_c99_header_21_();
        mw_c99_tags_21_();
        mw_c99_buffers_21_();
        mw_c99_variables_21_();
        mw_c99_externals_21_();
        mw_c99_word_sigs_21_();
        mw_c99_block_sigs_21_();
        mw_c99_field_sigs_21_();
        mw_c99_main_21_();
        mw_c99_word_defs_21_();
        mw_c99_block_defs_21_();
        mw_c99_field_defs_21_();
        mw_codegen_end_21_();
    }
}

static void mw__2E_lf (void){
    push_i64(10LL);
    mw_Int__3E_U8();
    mw__2E_b();
}

static void mw__3B_ (void){
    mw__2E_();
    mw__2E_lf();
}

static void mw__3B__3B_ (void){
    mw__2E_();
    mw__2E_lf();
    mw__2E_lf();
}

static void mw__2E_n (void){
    mw_int_show();
    mw__2E_();
}

static void mw__2E_name (void){
    mw_name_mangle_21_();
    mw__2E_();
}

static void mw__2E_w (void){
    push_ptr("static void mw_\0\0\0\0");
    mw__2E_();
    mw__2E_name();
    push_ptr(" (void)\0\0\0\0");
    mw__2E_();
}

static void mw__2E_p (void){
    mw_prim_name();
    mw__40_();
    mw__2E_w();
}

static void mw__2E_pm (void){
    push_ptr("#define mw_\0\0\0\0");
    mw__2E_();
    mw_prim_name();
    mw__40_();
    mw__2E_name();
    push_ptr("() \0\0\0\0");
    mw__2E_();
}

static void mw_c99_header_21_ (void){
    {
        static uint8_t b[20528] = {
            47,42,32,77,73,82,84,72,32,72,69,65,68,69,82,32,42,47,10,
            10,
            35,105,102,32,100,101,102,105,110,101,100,40,87,73,78,51,50,41,32,124,124,32,100,101,102,105,110,101,100,40,95,87,73,78,51,50,41,32,124,124,32,100,101,102,105,110,101,100,40,95,95,87,73,78,51,50,95,95,41,32,124,124,32,100,101,102,105,110,101,100,40,95,95,78,84,95,95,41,10,
            35,100,101,102,105,110,101,32,77,73,82,84,72,95,87,73,78,68,79,87,83,32,49,10,
            35,101,108,105,102,32,100,101,102,105,110,101,100,40,95,95,108,105,110,117,120,95,95,41,10,
            35,100,101,102,105,110,101,32,77,73,82,84,72,95,76,73,78,85,88,32,49,10,
            35,101,108,105,102,32,100,101,102,105,110,101,100,40,95,95,65,80,80,76,69,95,95,41,10,
            35,100,101,102,105,110,101,32,77,73,82,84,72,95,77,65,67,79,83,32,49,10,
            35,101,108,115,101,10,
            35,101,114,114,111,114,32,34,80,108,97,116,102,111,114,109,32,110,111,116,32,115,117,112,112,111,114,116,101,100,46,34,10,
            35,101,110,100,105,102,10,
            10,
            35,105,110,99,108,117,100,101,32,60,115,116,100,105,110,116,46,104,62,10,
            35,105,110,99,108,117,100,101,32,60,115,116,100,98,111,111,108,46,104,62,10,
            35,105,110,99,108,117,100,101,32,60,115,116,100,100,101,102,46,104,62,10,
            10,
            101,120,116,101,114,110,32,118,111,105,100,42,32,109,97,108,108,111,99,40,115,105,122,101,95,116,41,59,10,
            101,120,116,101,114,110,32,118,111,105,100,42,32,99,97,108,108,111,99,40,115,105,122,101,95,116,44,32,115,105,122,101,95,116,41,59,10,
            101,120,116,101,114,110,32,118,111,105,100,42,32,114,101,97,108,108,111,99,40,118,111,105,100,42,44,32,115,105,122,101,95,116,41,59,10,
            101,120,116,101,114,110,32,118,111,105,100,42,32,109,101,109,115,101,116,40,118,111,105,100,42,44,32,105,110,116,44,32,115,105,122,101,95,116,41,59,10,
            101,120,116,101,114,110,32,118,111,105,100,42,32,109,101,109,99,112,121,40,118,111,105,100,42,44,32,99,111,110,115,116,32,118,111,105,100,42,44,32,115,105,122,101,95,116,41,59,10,
            101,120,116,101,114,110,32,118,111,105,100,32,102,114,101,101,40,118,111,105,100,42,41,59,10,
            101,120,116,101,114,110,32,115,105,122,101,95,116,32,115,116,114,108,101,110,40,99,111,110,115,116,32,99,104,97,114,42,41,59,10,
            101,120,116,101,114,110,32,105,110,116,32,114,101,97,100,40,105,110,116,44,32,118,111,105,100,42,44,32,115,105,122,101,95,116,41,59,10,
            101,120,116,101,114,110,32,105,110,116,32,119,114,105,116,101,40,105,110,116,44,32,118,111,105,100,42,44,32,115,105,122,101,95,116,41,59,10,
            101,120,116,101,114,110,32,105,110,116,32,99,108,111,115,101,40,105,110,116,41,59,10,
            101,120,116,101,114,110,32,105,110,116,32,111,112,101,110,40,118,111,105,100,42,44,32,105,110,116,44,32,105,110,116,41,59,10,
            101,120,116,101,114,110,32,105,110,116,32,115,116,114,99,109,112,40,99,111,110,115,116,32,99,104,97,114,42,44,32,99,111,110,115,116,32,99,104,97,114,42,41,59,10,
            101,120,116,101,114,110,32,118,111,105,100,32,101,120,105,116,40,105,110,116,41,59,10,
            10,
            35,100,101,102,105,110,101,32,69,88,80,69,67,84,40,116,101,115,116,44,109,115,103,41,32,92,10,
            32,32,32,32,100,111,32,123,32,92,10,
            32,32,32,32,32,32,32,32,105,102,32,40,33,40,116,101,115,116,41,41,32,123,32,92,10,
            32,32,32,32,32,32,32,32,32,32,32,32,119,114,105,116,101,40,50,44,32,109,115,103,32,34,92,110,34,44,32,115,116,114,108,101,110,40,109,115,103,32,34,92,110,34,41,41,59,32,92,10,
            32,32,32,32,32,32,32,32,32,32,32,32,101,120,105,116,40,49,41,59,32,92,10,
            32,32,32,32,32,32,32,32,125,32,92,10,
            32,32,32,32,125,32,119,104,105,108,101,40,48,41,10,
            10,
            35,100,101,102,105,110,101,32,65,83,83,69,82,84,40,116,101,115,116,41,32,92,10,
            32,32,32,32,69,88,80,69,67,84,40,116,101,115,116,44,32,34,97,115,115,101,114,116,105,111,110,32,102,97,105,108,101,100,32,40,34,32,35,116,101,115,116,32,34,41,34,41,10,
            10,
            116,121,112,101,100,101,102,32,101,110,117,109,32,84,65,71,32,123,10,
            32,32,32,32,84,65,71,95,73,78,84,32,61,32,48,44,10,
            32,32,32,32,84,65,71,95,67,79,78,83,32,61,32,49,44,10,
            125,32,84,65,71,59,10,
            10,
            116,121,112,101,100,101,102,32,118,111,105,100,32,40,42,102,110,112,116,114,41,40,118,111,105,100,41,59,10,
            10,
            116,121,112,101,100,101,102,32,117,110,105,111,110,32,68,65,84,65,32,123,10,
            32,32,32,32,115,105,122,101,95,116,32,117,115,105,122,101,59,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,117,54,52,59,10,
            32,32,32,32,117,105,110,116,51,50,95,116,32,117,51,50,59,10,
            32,32,32,32,117,105,110,116,49,54,95,116,32,117,49,54,59,10,
            32,32,32,32,117,105,110,116,56,95,116,32,117,56,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,105,54,52,59,10,
            32,32,32,32,105,110,116,51,50,95,116,32,105,51,50,59,10,
            32,32,32,32,105,110,116,49,54,95,116,32,105,49,54,59,10,
            32,32,32,32,105,110,116,56,95,116,32,105,56,59,10,
            32,32,32,32,118,111,105,100,42,32,112,116,114,59,10,
            32,32,32,32,118,111,105,100,32,40,42,102,110,112,116,114,41,40,118,111,105,100,41,59,10,
            32,32,32,32,115,116,114,117,99,116,32,86,65,76,42,32,118,97,108,112,116,114,59,10,
            32,32,32,32,99,104,97,114,42,32,99,104,97,114,112,116,114,59,10,
            125,32,68,65,84,65,59,10,
            10,
            116,121,112,101,100,101,102,32,115,116,114,117,99,116,32,86,65,76,32,123,10,
            32,32,32,32,68,65,84,65,32,100,97,116,97,59,10,
            32,32,32,32,84,65,71,32,116,97,103,59,10,
            125,32,86,65,76,59,10,
            10,
            116,121,112,101,100,101,102,32,115,116,114,117,99,116,32,67,79,78,83,32,123,10,
            32,32,32,32,117,105,110,116,51,50,95,116,32,114,101,102,115,59,10,
            32,32,32,32,98,111,111,108,32,102,114,101,101,99,100,114,59,10,
            32,32,32,32,86,65,76,32,99,97,114,59,10,
            32,32,32,32,86,65,76,32,99,100,114,59,10,
            125,32,67,79,78,83,59,10,
            10,
            35,100,101,102,105,110,101,32,83,84,65,67,75,95,77,65,88,32,48,120,56,48,48,48,10,
            115,116,97,116,105,99,32,115,105,122,101,95,116,32,115,116,97,99,107,95,99,111,117,110,116,101,114,32,61,32,83,84,65,67,75,95,77,65,88,59,10,
            115,116,97,116,105,99,32,86,65,76,32,115,116,97,99,107,32,91,83,84,65,67,75,95,77,65,88,93,32,61,32,123,48,125,59,10,
            10,
            35,100,101,102,105,110,101,32,72,69,65,80,95,83,73,90,69,32,48,120,56,48,48,48,48,10,
            35,100,101,102,105,110,101,32,72,69,65,80,95,77,65,83,75,32,48,120,55,70,70,70,70,10,
            115,116,97,116,105,99,32,115,105,122,101,95,116,32,104,101,97,112,95,110,101,120,116,32,61,32,49,59,10,
            115,116,97,116,105,99,32,115,105,122,101,95,116,32,104,101,97,112,95,99,111,117,110,116,32,61,32,48,59,10,
            115,116,97,116,105,99,32,67,79,78,83,32,104,101,97,112,32,91,72,69,65,80,95,83,73,90,69,93,32,61,32,123,48,125,59,10,
            10,
            115,116,97,116,105,99,32,105,110,116,32,103,108,111,98,97,108,95,97,114,103,99,59,10,
            115,116,97,116,105,99,32,99,104,97,114,42,42,32,103,108,111,98,97,108,95,97,114,103,118,59,10,
            10,
            115,116,97,116,105,99,32,115,105,122,101,95,116,32,103,101,116,95,99,101,108,108,95,105,110,100,101,120,40,86,65,76,32,118,41,32,123,10,
            32,32,32,32,105,102,32,40,118,46,116,97,103,32,61,61,32,84,65,71,95,67,79,78,83,41,10,
            32,32,32,32,32,32,32,32,114,101,116,117,114,110,32,118,46,100,97,116,97,46,117,115,105,122,101,59,10,
            32,32,32,32,101,108,115,101,10,
            32,32,32,32,32,32,32,32,114,101,116,117,114,110,32,48,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,105,110,99,114,101,102,40,86,65,76,32,118,41,32,123,10,
            32,32,32,32,115,105,122,101,95,116,32,105,32,61,32,103,101,116,95,99,101,108,108,95,105,110,100,101,120,40,118,41,59,10,
            32,32,32,32,105,102,32,40,105,41,32,123,10,
            32,32,32,32,32,32,32,32,104,101,97,112,91,105,93,46,114,101,102,115,43,43,59,10,
            32,32,32,32,125,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,100,101,99,114,101,102,40,86,65,76,32,118,41,32,123,10,
            32,32,32,32,115,105,122,101,95,116,32,105,32,61,32,103,101,116,95,99,101,108,108,95,105,110,100,101,120,40,118,41,59,10,
            32,32,32,32,105,102,32,40,105,41,32,123,10,
            32,32,32,32,32,32,32,32,65,83,83,69,82,84,40,104,101,97,112,91,105,93,46,114,101,102,115,41,59,10,
            32,32,32,32,32,32,32,32,104,101,97,112,91,105,93,46,114,101,102,115,45,45,59,10,
            32,32,32,32,32,32,32,32,105,102,32,40,104,101,97,112,91,105,93,46,114,101,102,115,32,61,61,32,48,41,32,123,10,
            32,32,32,32,32,32,32,32,32,32,32,32,67,79,78,83,32,99,111,110,115,32,61,32,104,101,97,112,91,105,93,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,109,101,109,115,101,116,40,104,101,97,112,43,105,44,32,48,44,32,115,105,122,101,111,102,40,67,79,78,83,41,41,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,104,101,97,112,91,105,93,46,99,100,114,46,100,97,116,97,46,117,115,105,122,101,32,61,32,104,101,97,112,95,110,101,120,116,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,104,101,97,112,95,110,101,120,116,32,61,32,105,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,104,101,97,112,95,99,111,117,110,116,45,45,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,105,102,32,40,99,111,110,115,46,102,114,101,101,99,100,114,41,32,123,10,
            32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,102,114,101,101,40,99,111,110,115,46,99,100,114,46,100,97,116,97,46,112,116,114,41,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,125,32,101,108,115,101,32,123,10,
            32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,100,101,99,114,101,102,40,99,111,110,115,46,99,100,114,41,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,125,10,
            32,32,32,32,32,32,32,32,32,32,32,32,100,101,99,114,101,102,40,99,111,110,115,46,99,97,114,41,59,10,
            32,32,32,32,32,32,32,32,125,10,
            32,32,32,32,125,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,118,97,108,117,101,95,117,110,99,111,110,115,40,86,65,76,32,118,97,108,44,32,86,65,76,42,32,99,97,114,44,32,86,65,76,42,32,99,100,114,41,32,123,10,
            32,32,32,32,115,105,122,101,95,116,32,105,32,61,32,103,101,116,95,99,101,108,108,95,105,110,100,101,120,40,118,97,108,41,59,10,
            32,32,32,32,105,102,32,40,105,41,32,123,10,
            32,32,32,32,32,32,32,32,42,99,97,114,32,61,32,104,101,97,112,91,105,93,46,99,97,114,59,10,
            32,32,32,32,32,32,32,32,42,99,100,114,32,61,32,104,101,97,112,91,105,93,46,99,100,114,59,10,
            32,32,32,32,125,32,101,108,115,101,32,123,10,
            32,32,32,32,32,32,32,32,42,99,97,114,32,61,32,40,86,65,76,41,123,48,125,59,10,
            32,32,32,32,32,32,32,32,42,99,100,114,32,61,32,118,97,108,59,10,
            32,32,32,32,125,10,
            125,10,
            10,
            115,116,97,116,105,99,32,115,105,122,101,95,116,32,118,97,108,117,101,95,112,116,114,95,115,105,122,101,32,40,86,65,76,32,118,41,32,123,10,
            32,32,32,32,86,65,76,32,118,99,97,114,44,32,118,99,100,114,59,10,
            32,32,32,32,118,97,108,117,101,95,117,110,99,111,110,115,40,118,44,32,38,118,99,97,114,44,32,38,118,99,100,114,41,59,10,
            32,32,32,32,114,101,116,117,114,110,32,118,99,97,114,46,100,97,116,97,46,117,115,105,122,101,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,42,32,118,97,108,117,101,95,112,116,114,32,40,86,65,76,32,118,41,32,123,10,
            32,32,32,32,86,65,76,32,118,99,97,114,44,32,118,99,100,114,59,10,
            32,32,32,32,118,97,108,117,101,95,117,110,99,111,110,115,40,118,44,32,38,118,99,97,114,44,32,38,118,99,100,114,41,59,10,
            32,32,32,32,114,101,116,117,114,110,32,118,99,100,114,46,100,97,116,97,46,112,116,114,59,10,
            125,10,
            10,
            35,100,101,102,105,110,101,32,112,111,112,95,102,110,112,116,114,40,41,32,40,112,111,112,95,118,97,108,117,101,40,41,46,100,97,116,97,46,102,110,112,116,114,41,10,
            35,100,101,102,105,110,101,32,112,111,112,95,117,56,40,41,32,40,112,111,112,95,118,97,108,117,101,40,41,46,100,97,116,97,46,117,56,41,10,
            35,100,101,102,105,110,101,32,112,111,112,95,117,49,54,40,41,32,40,112,111,112,95,118,97,108,117,101,40,41,46,100,97,116,97,46,117,49,54,41,10,
            35,100,101,102,105,110,101,32,112,111,112,95,117,51,50,40,41,32,40,112,111,112,95,118,97,108,117,101,40,41,46,100,97,116,97,46,117,51,50,41,10,
            35,100,101,102,105,110,101,32,112,111,112,95,117,54,52,40,41,32,40,112,111,112,95,118,97,108,117,101,40,41,46,100,97,116,97,46,117,54,52,41,10,
            35,100,101,102,105,110,101,32,112,111,112,95,105,56,40,41,32,40,112,111,112,95,118,97,108,117,101,40,41,46,100,97,116,97,46,105,56,41,10,
            35,100,101,102,105,110,101,32,112,111,112,95,105,49,54,40,41,32,40,112,111,112,95,118,97,108,117,101,40,41,46,100,97,116,97,46,105,49,54,41,10,
            35,100,101,102,105,110,101,32,112,111,112,95,105,51,50,40,41,32,40,112,111,112,95,118,97,108,117,101,40,41,46,100,97,116,97,46,105,51,50,41,10,
            35,100,101,102,105,110,101,32,112,111,112,95,105,54,52,40,41,32,40,112,111,112,95,118,97,108,117,101,40,41,46,100,97,116,97,46,105,54,52,41,10,
            35,100,101,102,105,110,101,32,112,111,112,95,117,115,105,122,101,40,41,32,40,112,111,112,95,118,97,108,117,101,40,41,46,100,97,116,97,46,117,115,105,122,101,41,10,
            35,100,101,102,105,110,101,32,112,111,112,95,98,111,111,108,40,41,32,40,40,98,111,111,108,41,112,111,112,95,117,54,52,40,41,41,10,
            35,100,101,102,105,110,101,32,112,111,112,95,112,116,114,40,41,32,40,112,111,112,95,118,97,108,117,101,40,41,46,100,97,116,97,46,112,116,114,41,10,
            10,
            35,100,101,102,105,110,101,32,112,117,115,104,95,117,54,52,40,118,41,32,112,117,115,104,95,118,97,108,117,101,40,109,107,117,54,52,40,118,41,41,10,
            35,100,101,102,105,110,101,32,112,117,115,104,95,105,54,52,40,118,41,32,112,117,115,104,95,118,97,108,117,101,40,109,107,105,54,52,40,118,41,41,10,
            35,100,101,102,105,110,101,32,112,117,115,104,95,117,115,105,122,101,40,118,41,32,112,117,115,104,95,118,97,108,117,101,40,40,117,105,110,116,54,52,95,116,41,40,118,41,41,10,
            35,100,101,102,105,110,101,32,112,117,115,104,95,102,110,112,116,114,40,118,41,32,112,117,115,104,95,117,54,52,40,40,117,105,110,116,54,52,95,116,41,40,118,41,41,10,
            35,100,101,102,105,110,101,32,112,117,115,104,95,98,111,111,108,40,98,41,32,112,117,115,104,95,117,54,52,40,40,117,105,110,116,54,52,95,116,41,40,40,98,111,111,108,41,40,98,41,41,41,10,
            35,100,101,102,105,110,101,32,112,117,115,104,95,117,56,40,98,41,32,112,117,115,104,95,117,54,52,40,40,117,105,110,116,54,52,95,116,41,40,98,41,41,10,
            35,100,101,102,105,110,101,32,112,117,115,104,95,117,49,54,40,98,41,32,112,117,115,104,95,117,54,52,40,40,117,105,110,116,54,52,95,116,41,40,98,41,41,10,
            35,100,101,102,105,110,101,32,112,117,115,104,95,117,51,50,40,98,41,32,112,117,115,104,95,117,54,52,40,40,117,105,110,116,54,52,95,116,41,40,98,41,41,10,
            35,100,101,102,105,110,101,32,112,117,115,104,95,105,56,40,98,41,32,112,117,115,104,95,105,54,52,40,40,105,110,116,54,52,95,116,41,40,98,41,41,10,
            35,100,101,102,105,110,101,32,112,117,115,104,95,105,49,54,40,98,41,32,112,117,115,104,95,105,54,52,40,40,105,110,116,54,52,95,116,41,40,98,41,41,10,
            35,100,101,102,105,110,101,32,112,117,115,104,95,105,51,50,40,98,41,32,112,117,115,104,95,105,54,52,40,40,105,110,116,54,52,95,116,41,40,98,41,41,10,
            35,100,101,102,105,110,101,32,112,117,115,104,95,112,116,114,40,118,41,32,112,117,115,104,95,117,54,52,40,40,117,105,110,116,54,52,95,116,41,40,118,111,105,100,42,41,40,118,41,41,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,112,117,115,104,95,118,97,108,117,101,40,86,65,76,32,120,41,32,123,10,
            32,32,32,32,65,83,83,69,82,84,40,115,116,97,99,107,95,99,111,117,110,116,101,114,32,62,32,48,41,59,10,
            32,32,32,32,115,116,97,99,107,91,45,45,115,116,97,99,107,95,99,111,117,110,116,101,114,93,32,61,32,120,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,86,65,76,32,116,111,112,95,118,97,108,117,101,40,118,111,105,100,41,32,123,10,
            32,32,32,32,65,83,83,69,82,84,40,115,116,97,99,107,95,99,111,117,110,116,101,114,32,60,32,83,84,65,67,75,95,77,65,88,41,59,10,
            32,32,32,32,114,101,116,117,114,110,32,115,116,97,99,107,91,115,116,97,99,107,95,99,111,117,110,116,101,114,93,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,86,65,76,32,112,111,112,95,118,97,108,117,101,40,118,111,105,100,41,32,123,10,
            32,32,32,32,65,83,83,69,82,84,40,115,116,97,99,107,95,99,111,117,110,116,101,114,32,60,32,83,84,65,67,75,95,77,65,88,41,59,10,
            32,32,32,32,114,101,116,117,114,110,32,115,116,97,99,107,91,115,116,97,99,107,95,99,111,117,110,116,101,114,43,43,93,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,86,65,76,32,109,107,117,54,52,32,40,117,105,110,116,54,52,95,116,32,120,41,32,123,10,
            32,32,32,32,114,101,116,117,114,110,32,40,86,65,76,41,123,46,116,97,103,61,84,65,71,95,73,78,84,44,32,46,100,97,116,97,61,123,46,117,54,52,61,120,125,125,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,86,65,76,32,109,107,105,54,52,32,40,105,110,116,54,52,95,116,32,120,41,32,123,10,
            32,32,32,32,114,101,116,117,114,110,32,40,86,65,76,41,123,46,116,97,103,61,84,65,71,95,73,78,84,44,32,46,100,97,116,97,61,123,46,105,54,52,61,120,125,125,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,86,65,76,32,109,107,99,101,108,108,95,114,97,119,32,40,86,65,76,32,99,97,114,44,32,86,65,76,32,99,100,114,44,32,98,111,111,108,32,102,114,101,101,99,100,114,41,32,123,10,
            32,32,32,32,69,88,80,69,67,84,40,104,101,97,112,95,99,111,117,110,116,32,60,32,72,69,65,80,95,83,73,90,69,45,49,44,32,34,104,101,97,112,32,111,118,101,114,102,108,111,119,34,41,59,10,
            32,32,32,32,115,105,122,101,95,116,32,105,32,61,32,104,101,97,112,95,110,101,120,116,59,10,
            32,32,32,32,67,79,78,83,32,42,99,101,108,108,32,61,32,104,101,97,112,43,105,59,10,
            32,32,32,32,65,83,83,69,82,84,40,99,101,108,108,45,62,114,101,102,115,32,61,61,32,48,41,59,10,
            32,32,32,32,104,101,97,112,95,110,101,120,116,32,61,32,99,101,108,108,45,62,99,100,114,46,100,97,116,97,46,117,115,105,122,101,32,63,32,99,101,108,108,45,62,99,100,114,46,100,97,116,97,46,117,115,105,122,101,32,58,32,105,43,49,59,10,
            32,32,32,32,104,101,97,112,95,99,111,117,110,116,43,43,59,10,
            32,32,32,32,99,101,108,108,45,62,114,101,102,115,32,61,32,49,59,10,
            32,32,32,32,99,101,108,108,45,62,102,114,101,101,99,100,114,32,61,32,102,114,101,101,99,100,114,59,10,
            32,32,32,32,99,101,108,108,45,62,99,97,114,32,61,32,99,97,114,59,10,
            32,32,32,32,99,101,108,108,45,62,99,100,114,32,61,32,99,100,114,59,10,
            32,32,32,32,114,101,116,117,114,110,32,40,86,65,76,41,123,32,46,116,97,103,61,84,65,71,95,67,79,78,83,44,32,46,100,97,116,97,61,123,46,117,115,105,122,101,61,105,125,32,125,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,86,65,76,32,109,107,99,101,108,108,40,86,65,76,32,99,97,114,44,32,86,65,76,32,99,100,114,41,32,123,10,
            32,32,32,32,105,102,32,40,40,99,97,114,46,100,97,116,97,46,117,115,105,122,101,32,61,61,32,48,41,32,38,38,32,40,99,100,114,46,116,97,103,32,61,61,32,84,65,71,95,73,78,84,41,41,10,
            32,32,32,32,32,32,32,32,114,101,116,117,114,110,32,99,100,114,59,10,
            32,32,32,32,114,101,116,117,114,110,32,109,107,99,101,108,108,95,114,97,119,40,99,97,114,44,32,99,100,114,44,32,102,97,108,115,101,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,86,65,76,32,109,107,112,116,114,95,111,119,110,101,100,32,40,118,111,105,100,42,32,112,116,114,44,32,115,105,122,101,95,116,32,115,105,122,101,41,32,123,10,
            32,32,32,32,86,65,76,32,99,100,114,32,61,32,123,32,46,116,97,103,61,84,65,71,95,73,78,84,44,32,46,100,97,116,97,61,123,46,112,116,114,61,112,116,114,125,32,125,59,32,47,47,32,84,79,68,79,32,84,65,71,95,82,65,87,80,84,82,32,109,97,121,98,101,63,10,
            32,32,32,32,86,65,76,32,99,97,114,32,61,32,123,32,46,116,97,103,61,84,65,71,95,73,78,84,44,32,46,100,97,116,97,61,123,46,117,115,105,122,101,61,115,105,122,101,125,32,125,59,10,
            32,32,32,32,114,101,116,117,114,110,32,109,107,99,101,108,108,95,114,97,119,40,99,97,114,44,32,99,100,114,44,32,116,114,117,101,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,86,65,76,32,109,107,112,116,114,95,115,104,97,114,101,100,32,40,118,111,105,100,42,32,112,116,114,44,32,115,105,122,101,95,116,32,115,105,122,101,41,32,123,10,
            32,32,32,32,86,65,76,32,99,100,114,32,61,32,123,32,46,116,97,103,61,84,65,71,95,73,78,84,44,32,46,100,97,116,97,61,123,46,112,116,114,61,112,116,114,125,32,125,59,32,47,47,32,84,79,68,79,32,84,65,71,95,82,65,87,80,84,82,32,109,97,121,98,101,63,10,
            32,32,32,32,86,65,76,32,99,97,114,32,61,32,123,32,46,116,97,103,61,84,65,71,95,73,78,84,44,32,46,100,97,116,97,61,123,46,117,115,105,122,101,61,115,105,122,101,125,32,125,59,10,
            32,32,32,32,114,101,116,117,114,110,32,109,107,99,101,108,108,40,99,97,114,44,32,99,100,114,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,98,111,111,108,32,118,97,108,95,104,97,115,95,102,114,101,101,99,100,114,32,40,86,65,76,32,118,41,32,123,10,
            32,32,32,32,115,105,122,101,95,116,32,105,32,61,32,103,101,116,95,99,101,108,108,95,105,110,100,101,120,40,118,41,59,10,
            32,32,32,32,114,101,116,117,114,110,32,105,32,38,38,32,104,101,97,112,91,105,93,46,102,114,101,101,99,100,114,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,100,111,95,117,110,99,111,110,115,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,97,108,44,32,99,97,114,44,32,99,100,114,59,10,
            32,32,32,32,118,97,108,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,65,83,83,69,82,84,40,33,118,97,108,95,104,97,115,95,102,114,101,101,99,100,114,40,118,97,108,41,41,59,10,
            32,32,32,32,118,97,108,117,101,95,117,110,99,111,110,115,40,118,97,108,44,32,38,99,97,114,44,32,38,99,100,114,41,59,10,
            32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,99,97,114,41,59,32,112,117,115,104,95,118,97,108,117,101,40,99,100,114,41,59,10,
            32,32,32,32,105,110,99,114,101,102,40,99,97,114,41,59,10,
            32,32,32,32,105,110,99,114,101,102,40,99,100,114,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,97,108,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,115,105,122,101,95,116,32,103,101,116,95,100,97,116,97,95,116,97,103,40,86,65,76,32,118,41,32,123,10,
            32,32,32,32,86,65,76,32,99,97,114,44,32,99,100,114,59,10,
            32,32,32,32,118,97,108,117,101,95,117,110,99,111,110,115,40,118,44,32,38,99,97,114,44,32,38,99,100,114,41,59,10,
            32,32,32,32,114,101,116,117,114,110,32,99,100,114,46,100,97,116,97,46,117,115,105,122,101,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,115,105,122,101,95,116,32,103,101,116,95,116,111,112,95,100,97,116,97,95,116,97,103,40,118,111,105,100,41,32,123,10,
            32,32,32,32,114,101,116,117,114,110,32,103,101,116,95,100,97,116,97,95,116,97,103,40,116,111,112,95,118,97,108,117,101,40,41,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,105,110,116,54,52,95,116,32,105,110,116,54,52,95,99,109,112,40,105,110,116,54,52,95,116,32,105,49,44,32,105,110,116,54,52,95,116,32,105,50,41,32,123,10,
            32,32,32,32,105,102,32,40,105,49,32,60,32,105,50,41,32,114,101,116,117,114,110,32,45,49,59,10,
            32,32,32,32,105,102,32,40,105,49,32,62,32,105,50,41,32,114,101,116,117,114,110,32,49,59,10,
            32,32,32,32,114,101,116,117,114,110,32,48,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,105,110,116,54,52,95,116,32,118,97,108,117,101,95,99,109,112,40,86,65,76,32,118,49,44,32,86,65,76,32,118,50,41,32,123,10,
            32,32,32,32,119,104,105,108,101,32,40,49,41,32,123,10,
            32,32,32,32,32,32,32,32,47,47,32,84,79,68,79,32,102,105,120,32,115,116,114,105,110,103,32,99,111,109,112,97,114,105,115,111,110,44,32,109,97,121,98,101,32,119,105,116,104,32,97,32,84,65,71,95,83,84,82,10,
            32,32,32,32,32,32,32,32,105,102,32,40,118,49,46,116,97,103,32,61,61,32,118,50,46,116,97,103,41,32,123,10,
            32,32,32,32,32,32,32,32,32,32,32,32,105,102,32,40,118,49,46,100,97,116,97,46,117,54,52,32,61,61,32,118,50,46,100,97,116,97,46,117,54,52,41,32,114,101,116,117,114,110,32,48,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,105,102,32,40,118,49,46,116,97,103,32,61,61,32,84,65,71,95,73,78,84,41,32,123,10,
            32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,105,102,32,40,118,49,46,100,97,116,97,46,105,54,52,32,60,32,118,50,46,100,97,116,97,46,105,54,52,41,32,114,101,116,117,114,110,32,45,49,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,101,108,115,101,32,114,101,116,117,114,110,32,49,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,125,10,
            32,32,32,32,32,32,32,32,125,10,
            32,32,32,32,32,32,32,32,86,65,76,32,118,49,99,97,114,44,32,118,49,99,100,114,44,32,118,50,99,97,114,44,32,118,50,99,100,114,59,10,
            32,32,32,32,32,32,32,32,118,97,108,117,101,95,117,110,99,111,110,115,40,118,49,44,32,38,118,49,99,97,114,44,32,38,118,49,99,100,114,41,59,10,
            32,32,32,32,32,32,32,32,118,97,108,117,101,95,117,110,99,111,110,115,40,118,50,44,32,38,118,50,99,97,114,44,32,38,118,50,99,100,114,41,59,10,
            32,32,32,32,32,32,32,32,105,110,116,54,52,95,116,32,99,100,114,99,109,112,32,61,32,118,97,108,117,101,95,99,109,112,40,118,49,99,100,114,44,32,118,50,99,100,114,41,59,10,
            32,32,32,32,32,32,32,32,105,102,32,40,99,100,114,99,109,112,41,32,114,101,116,117,114,110,32,99,100,114,99,109,112,59,10,
            32,32,32,32,32,32,32,32,118,49,32,61,32,118,49,99,97,114,59,32,118,50,32,61,32,118,50,99,97,114,59,10,
            32,32,32,32,125,10,
            125,10,
            10,
            115,116,97,116,105,99,32,98,111,111,108,32,118,97,108,117,101,95,101,113,40,86,65,76,32,118,49,44,32,86,65,76,32,118,50,41,32,123,10,
            32,32,32,32,114,101,116,117,114,110,32,118,97,108,117,101,95,99,109,112,40,118,49,44,118,50,41,32,61,61,32,48,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,114,117,110,95,118,97,108,117,101,40,86,65,76,32,118,41,32,123,10,
            32,32,32,32,86,65,76,32,99,97,114,44,32,99,100,114,59,10,
            32,32,32,32,118,97,108,117,101,95,117,110,99,111,110,115,40,118,44,32,38,99,97,114,44,32,38,99,100,114,41,59,10,
            32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,99,97,114,41,59,10,
            32,32,32,32,105,110,99,114,101,102,40,99,97,114,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,41,59,10,
            32,32,32,32,99,100,114,46,100,97,116,97,46,102,110,112,116,114,40,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,100,32,40,118,111,105,100,41,32,123,125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,100,117,112,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,32,61,32,116,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,118,41,59,10,
            32,32,32,32,105,110,99,114,101,102,40,118,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,100,114,111,112,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,115,119,97,112,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,97,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,98,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,97,41,59,10,
            32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,98,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,100,105,112,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,102,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,120,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,114,117,110,95,118,97,108,117,101,40,102,41,59,10,
            32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,120,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,102,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,101,108,115,101,95,98,114,97,110,99,104,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,116,104,101,110,95,98,114,97,110,99,104,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,98,111,111,108,32,98,32,61,32,112,111,112,95,98,111,111,108,40,41,59,10,
            32,32,32,32,105,102,32,40,98,41,32,123,10,
            32,32,32,32,32,32,32,32,100,101,99,114,101,102,40,101,108,115,101,95,98,114,97,110,99,104,41,59,10,
            32,32,32,32,32,32,32,32,114,117,110,95,118,97,108,117,101,40,116,104,101,110,95,98,114,97,110,99,104,41,59,10,
            32,32,32,32,125,32,101,108,115,101,32,123,10,
            32,32,32,32,32,32,32,32,100,101,99,114,101,102,40,116,104,101,110,95,98,114,97,110,99,104,41,59,10,
            32,32,32,32,32,32,32,32,114,117,110,95,118,97,108,117,101,40,101,108,115,101,95,98,114,97,110,99,104,41,59,10,
            32,32,32,32,125,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,119,104,105,108,101,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,98,111,100,121,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,99,111,110,100,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,119,104,105,108,101,40,49,41,32,123,10,
            32,32,32,32,32,32,32,32,105,110,99,114,101,102,40,99,111,110,100,41,59,32,114,117,110,95,118,97,108,117,101,40,99,111,110,100,41,59,10,
            32,32,32,32,32,32,32,32,98,111,111,108,32,98,32,61,32,112,111,112,95,98,111,111,108,40,41,59,10,
            32,32,32,32,32,32,32,32,105,102,32,40,33,98,41,32,98,114,101,97,107,59,10,
            32,32,32,32,32,32,32,32,105,110,99,114,101,102,40,98,111,100,121,41,59,32,114,117,110,95,118,97,108,117,101,40,98,111,100,121,41,59,10,
            32,32,32,32,125,10,
            32,32,32,32,100,101,99,114,101,102,40,99,111,110,100,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,98,111,100,121,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,110,116,95,97,100,100,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,47,47,32,84,79,68,79,32,112,114,111,109,111,116,101,32,116,111,32,98,105,103,105,110,116,32,111,110,32,111,118,101,114,102,108,111,119,46,10,
            32,32,32,32,105,110,116,54,52,95,116,32,98,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,97,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,105,102,32,40,98,32,62,61,32,48,41,32,123,10,
            32,32,32,32,32,32,32,32,69,88,80,69,67,84,40,97,32,60,61,32,73,78,84,54,52,95,77,65,88,32,45,32,98,44,32,34,105,110,116,101,103,101,114,32,111,118,101,114,102,108,111,119,32,100,117,114,105,110,103,32,97,100,100,105,116,105,111,110,32,40,116,111,111,32,112,111,115,105,116,105,118,101,41,34,41,59,10,
            32,32,32,32,125,32,101,108,115,101,32,123,10,
            32,32,32,32,32,32,32,32,69,88,80,69,67,84,40,97,32,62,61,32,73,78,84,54,52,95,77,73,78,32,45,32,98,44,32,34,105,110,116,101,103,101,114,32,111,118,101,114,102,108,111,119,32,100,117,114,105,110,103,32,97,100,100,105,116,105,111,110,32,40,116,111,111,32,110,101,103,97,116,105,118,101,41,34,41,59,10,
            32,32,32,32,125,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,97,32,43,32,98,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,110,116,95,115,117,98,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,47,47,32,84,79,68,79,32,112,114,111,109,111,116,101,32,116,111,32,98,105,103,105,110,116,32,111,110,32,111,118,101,114,102,108,111,119,10,
            32,32,32,32,105,110,116,54,52,95,116,32,98,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,97,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,105,102,32,40,98,32,62,61,32,48,41,32,123,10,
            32,32,32,32,32,32,32,32,69,88,80,69,67,84,40,97,32,62,61,32,73,78,84,54,52,95,77,73,78,32,43,32,98,44,32,34,105,110,116,101,103,101,114,32,111,118,101,114,102,108,111,119,32,100,117,114,105,110,103,32,115,117,98,116,114,97,99,116,105,111,110,32,40,116,111,111,32,110,101,103,97,116,105,118,101,41,34,41,59,10,
            32,32,32,32,125,32,101,108,115,101,32,123,10,
            32,32,32,32,32,32,32,32,69,88,80,69,67,84,40,97,32,60,61,32,73,78,84,54,52,95,77,65,88,32,43,32,98,44,32,34,105,110,116,101,103,101,114,32,111,118,101,114,102,108,111,119,32,100,117,114,105,110,103,32,115,117,98,116,114,97,99,116,105,111,110,32,40,116,111,111,32,112,111,115,105,116,105,118,101,41,34,41,59,10,
            32,32,32,32,125,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,97,32,45,32,98,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,110,116,95,109,117,108,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,47,47,32,84,79,68,79,32,112,114,111,109,111,116,101,32,116,111,32,98,105,103,105,110,116,32,111,110,32,111,118,101,114,102,108,111,119,10,
            32,32,32,32,105,110,116,54,52,95,116,32,98,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,97,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,47,47,32,111,118,101,114,102,108,111,119,32,99,104,101,99,107,115,32,102,111,114,32,109,117,108,116,105,112,108,105,99,97,116,105,111,110,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,97,32,42,32,98,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,110,116,95,100,105,118,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,47,47,32,84,79,68,79,32,112,114,111,109,111,116,101,32,116,111,32,98,105,103,105,110,116,32,111,110,32,111,118,101,114,102,108,111,119,10,
            32,32,32,32,105,110,116,54,52,95,116,32,98,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,97,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,69,88,80,69,67,84,40,98,32,33,61,32,48,44,32,34,100,105,118,105,100,101,32,98,121,32,122,101,114,111,34,41,59,10,
            32,32,32,32,69,88,80,69,67,84,40,33,40,40,98,32,61,61,32,45,49,41,32,38,38,32,40,97,32,61,61,32,73,78,84,54,52,95,77,73,78,41,41,44,32,34,111,118,101,114,102,108,111,119,32,100,117,114,105,110,103,32,100,105,118,105,115,105,111,110,34,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,114,32,61,32,97,32,37,32,98,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,113,32,61,32,97,32,47,32,98,59,10,
            32,32,32,32,105,102,32,40,40,40,97,32,60,32,48,41,32,94,32,40,98,32,60,32,48,41,41,32,38,38,32,114,41,32,113,45,45,59,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,113,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,110,116,95,109,111,100,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,105,110,116,54,52,95,116,32,98,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,97,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,69,88,80,69,67,84,40,98,32,33,61,32,48,44,32,34,100,105,118,105,100,101,32,98,121,32,122,101,114,111,34,41,59,10,
            32,32,32,32,105,102,32,40,98,32,61,61,32,45,49,41,32,123,32,112,117,115,104,95,105,54,52,40,48,41,59,32,114,101,116,117,114,110,59,32,125,10,
            32,32,32,32,105,110,116,54,52,95,116,32,114,32,61,32,97,32,37,32,98,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,113,32,61,32,97,32,47,32,98,59,10,
            32,32,32,32,105,102,32,40,40,40,97,32,60,32,48,41,32,94,32,40,98,32,60,32,48,41,41,32,38,38,32,114,41,32,114,32,43,61,32,98,59,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,114,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,110,116,95,97,110,100,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,98,32,61,32,112,111,112,95,117,54,52,40,41,59,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,97,32,61,32,112,111,112,95,117,54,52,40,41,59,10,
            32,32,32,32,112,117,115,104,95,117,54,52,40,97,32,38,32,98,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,110,116,95,111,114,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,98,32,61,32,112,111,112,95,117,54,52,40,41,59,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,97,32,61,32,112,111,112,95,117,54,52,40,41,59,10,
            32,32,32,32,112,117,115,104,95,117,54,52,40,97,32,124,32,98,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,110,116,95,120,111,114,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,98,32,61,32,112,111,112,95,117,54,52,40,41,59,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,97,32,61,32,112,111,112,95,117,54,52,40,41,59,10,
            32,32,32,32,112,117,115,104,95,117,54,52,40,97,32,94,32,98,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,110,116,95,115,104,108,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,98,32,61,32,112,111,112,95,117,54,52,40,41,59,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,97,32,61,32,112,111,112,95,117,54,52,40,41,59,10,
            32,32,32,32,112,117,115,104,95,117,54,52,40,40,98,32,62,61,32,54,52,41,32,63,32,48,32,58,32,40,97,32,60,60,32,98,41,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,110,116,95,115,104,114,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,98,32,61,32,112,111,112,95,117,54,52,40,41,59,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,97,32,61,32,112,111,112,95,117,54,52,40,41,59,10,
            32,32,32,32,112,117,115,104,95,117,54,52,40,40,98,32,62,61,32,54,52,41,32,63,32,48,32,58,32,40,97,32,62,62,32,98,41,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,98,111,111,108,95,116,114,117,101,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,112,117,115,104,95,98,111,111,108,40,116,114,117,101,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,98,111,111,108,95,102,97,108,115,101,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,112,117,115,104,95,98,111,111,108,40,102,97,108,115,101,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,98,111,111,108,95,97,110,100,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,98,32,61,32,112,111,112,95,98,111,111,108,40,41,59,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,97,32,61,32,112,111,112,95,98,111,111,108,40,41,59,10,
            32,32,32,32,112,117,115,104,95,98,111,111,108,40,97,32,38,38,32,98,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,98,111,111,108,95,111,114,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,98,32,61,32,112,111,112,95,98,111,111,108,40,41,59,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,97,32,61,32,112,111,112,95,98,111,111,108,40,41,59,10,
            32,32,32,32,112,117,115,104,95,98,111,111,108,40,97,32,124,124,32,98,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,118,97,108,117,101,95,101,113,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,98,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,97,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,32,99,109,112,32,61,32,118,97,108,117,101,95,99,109,112,40,97,44,98,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,97,41,59,32,100,101,99,114,101,102,40,98,41,59,10,
            32,32,32,32,112,117,115,104,95,117,54,52,40,99,109,112,32,61,61,32,48,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,118,97,108,117,101,95,108,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,98,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,97,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,32,99,109,112,32,61,32,118,97,108,117,101,95,99,109,112,40,97,44,98,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,97,41,59,32,100,101,99,114,101,102,40,98,41,59,10,
            32,32,32,32,112,117,115,104,95,117,54,52,40,99,109,112,32,60,32,48,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,118,97,108,117,101,95,108,101,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,98,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,97,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,32,99,109,112,32,61,32,118,97,108,117,101,95,99,109,112,40,97,44,98,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,97,41,59,32,100,101,99,114,101,102,40,98,41,59,10,
            32,32,32,32,112,117,115,104,95,117,54,52,40,99,109,112,32,60,61,32,48,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,115,121,115,95,97,114,103,99,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,103,108,111,98,97,108,95,97,114,103,99,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,115,121,115,95,97,114,103,118,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,112,117,115,104,95,112,116,114,40,103,108,111,98,97,108,95,97,114,103,118,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,111,115,105,120,95,119,114,105,116,101,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,115,105,122,101,95,116,32,110,32,61,32,112,111,112,95,117,115,105,122,101,40,41,59,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,118,111,105,100,42,32,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,105,110,116,32,102,100,32,61,32,40,105,110,116,41,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,47,47,32,84,79,68,79,32,99,111,109,112,97,114,101,32,103,105,118,101,110,32,115,105,122,101,32,97,103,97,105,110,115,116,32,112,111,105,110,116,101,114,32,115,105,122,101,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,40,105,110,116,54,52,95,116,41,119,114,105,116,101,40,102,100,44,32,112,44,32,110,41,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,111,115,105,120,95,114,101,97,100,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,115,105,122,101,95,116,32,110,32,61,32,112,111,112,95,117,115,105,122,101,40,41,59,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,118,111,105,100,42,32,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,105,110,116,32,102,100,32,61,32,40,105,110,116,41,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,47,47,32,84,79,68,79,32,99,111,109,112,97,114,101,32,103,105,118,101,110,32,115,105,122,101,32,97,103,97,105,110,115,116,32,112,111,105,110,116,101,114,32,115,105,122,101,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,40,105,110,116,54,52,95,116,41,114,101,97,100,40,102,100,44,112,44,110,41,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,111,115,105,120,95,111,112,101,110,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,105,110,116,32,109,32,61,32,40,105,110,116,41,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,105,110,116,32,102,32,61,32,40,105,110,116,41,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,118,111,105,100,42,32,112,97,116,104,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,40,105,110,116,54,52,95,116,41,111,112,101,110,40,112,97,116,104,44,102,44,109,41,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,111,115,105,120,95,99,108,111,115,101,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,105,110,116,32,102,100,32,61,32,40,105,110,116,41,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,40,105,110,116,54,52,95,116,41,99,108,111,115,101,40,102,100,41,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,111,115,105,120,95,101,120,105,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,105,110,116,32,120,32,61,32,40,105,110,116,41,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,101,120,105,116,40,120,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,100,101,98,117,103,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,119,114,105,116,101,40,50,44,32,34,63,63,34,44,32,50,41,59,10,
            32,32,32,32,99,104,97,114,32,99,91,51,50,93,32,61,32,123,48,125,59,10,
            32,32,32,32,99,104,97,114,42,32,99,112,59,10,
            32,32,32,32,115,105,122,101,95,116,32,110,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,120,44,32,121,59,10,
            32,32,32,32,102,111,114,32,40,108,111,110,103,32,105,32,61,32,83,84,65,67,75,95,77,65,88,45,49,59,32,105,32,62,61,32,40,108,111,110,103,41,115,116,97,99,107,95,99,111,117,110,116,101,114,59,32,105,45,45,41,32,123,10,
            32,32,32,32,32,32,32,32,99,112,32,61,32,99,43,51,48,59,10,
            32,32,32,32,32,32,32,32,120,32,61,32,115,116,97,99,107,91,105,93,46,100,97,116,97,46,105,54,52,59,32,47,47,32,84,79,68,79,32,108,111,111,107,32,97,116,32,116,97,103,44,32,98,101,32,98,101,116,116,101,114,32,97,116,32,116,104,105,115,10,
            32,32,32,32,32,32,32,32,110,32,61,32,49,59,10,
            32,32,32,32,32,32,32,32,121,32,61,32,120,59,32,105,102,32,40,120,32,60,32,48,41,32,123,32,120,32,61,32,45,120,59,32,125,10,
            32,32,32,32,32,32,32,32,100,111,32,123,32,42,99,112,45,45,32,61,32,39,48,39,32,43,32,40,120,32,37,32,49,48,41,59,32,120,32,47,61,32,49,48,59,32,110,43,43,59,32,125,32,119,104,105,108,101,40,120,41,59,10,
            32,32,32,32,32,32,32,32,105,102,32,40,121,32,60,32,48,41,32,123,32,42,99,112,45,45,32,61,32,39,45,39,59,32,110,43,43,59,32,125,10,
            32,32,32,32,32,32,32,32,42,99,112,32,61,32,39,32,39,59,10,
            32,32,32,32,32,32,32,32,119,114,105,116,101,40,50,44,32,99,112,44,32,110,41,59,10,
            32,32,32,32,125,10,
            32,32,32,32,119,114,105,116,101,40,50,44,32,34,92,110,34,44,32,49,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,118,97,108,117,101,95,103,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,42,112,41,59,10,
            32,32,32,32,105,110,99,114,101,102,40,42,112,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,110,116,95,103,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,42,112,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,116,114,95,103,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,118,111,105,100,32,42,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,112,117,115,104,95,112,116,114,40,42,112,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,117,56,95,103,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,117,105,110,116,56,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,112,117,115,104,95,117,56,40,42,112,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,117,49,54,95,103,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,117,105,110,116,49,54,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,112,117,115,104,95,117,49,54,40,42,112,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,117,51,50,95,103,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,117,105,110,116,51,50,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,112,117,115,104,95,117,51,50,40,42,112,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,117,54,52,95,103,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,112,117,115,104,95,117,54,52,40,42,112,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,56,95,103,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,56,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,112,117,115,104,95,105,56,40,42,112,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,49,54,95,103,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,49,54,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,112,117,115,104,95,105,49,54,40,42,112,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,51,50,95,103,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,51,50,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,112,117,115,104,95,105,51,50,40,42,112,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,54,52,95,103,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,42,112,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,118,97,108,117,101,95,115,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,42,112,41,59,10,
            32,32,32,32,42,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,110,116,95,115,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,42,112,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,116,114,95,115,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,118,111,105,100,32,42,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,42,112,32,61,32,112,111,112,95,112,116,114,40,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,117,56,95,115,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,117,105,110,116,56,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,42,112,32,61,32,112,111,112,95,117,56,40,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,117,49,54,95,115,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,117,105,110,116,49,54,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,42,112,32,61,32,112,111,112,95,117,49,54,40,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,117,51,50,95,115,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,117,105,110,116,51,50,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,42,112,32,61,32,112,111,112,95,117,51,50,40,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,117,54,52,95,115,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,42,112,32,61,32,112,111,112,95,117,54,52,40,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,56,95,115,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,56,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,42,112,32,61,32,112,111,112,95,105,56,40,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,49,54,95,115,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,49,54,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,42,112,32,61,32,112,111,112,95,105,49,54,40,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,51,50,95,115,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,51,50,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,42,112,32,61,32,112,111,112,95,105,51,50,40,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,105,54,52,95,115,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,42,112,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,41,59,10,
            32,32,32,32,42,112,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,41,59,10,
            125,10,
            10,
            10,
            35,105,102,32,100,101,102,105,110,101,100,40,77,73,82,84,72,95,87,73,78,68,79,87,83,41,10,
            35,100,101,102,105,110,101,32,109,119,95,112,114,105,109,95,115,121,115,95,111,115,40,41,32,112,117,115,104,95,117,54,52,40,49,41,10,
            35,101,108,105,102,32,100,101,102,105,110,101,100,40,77,73,82,84,72,95,76,73,78,85,88,41,10,
            35,100,101,102,105,110,101,32,109,119,95,112,114,105,109,95,115,121,115,95,111,115,40,41,32,112,117,115,104,95,117,54,52,40,50,41,10,
            35,101,108,105,102,32,100,101,102,105,110,101,100,40,77,73,82,84,72,95,77,65,67,79,83,41,10,
            35,100,101,102,105,110,101,32,109,119,95,112,114,105,109,95,115,121,115,95,111,115,40,41,32,112,117,115,104,95,117,54,52,40,51,41,10,
            35,101,108,115,101,10,
            35,100,101,102,105,110,101,32,109,119,95,112,114,105,109,95,115,121,115,95,111,115,40,41,32,112,117,115,104,95,117,54,52,40,48,41,10,
            35,101,110,100,105,102,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,117,110,115,97,102,101,95,99,97,115,116,32,40,118,111,105,100,41,32,123,32,125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,114,117,110,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,102,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,114,117,110,95,118,97,108,117,101,40,102,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,116,114,95,97,100,100,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,117,105,110,116,54,52,95,116,32,121,32,61,32,112,111,112,95,117,54,52,40,41,59,10,
            32,32,32,32,115,105,122,101,95,116,32,105,32,61,32,103,101,116,95,99,101,108,108,95,105,110,100,101,120,40,118,112,41,59,10,
            32,32,32,32,105,102,32,40,105,41,32,123,10,
            32,32,32,32,32,32,32,32,67,79,78,83,32,42,99,101,108,108,32,61,32,104,101,97,112,43,105,59,10,
            32,32,32,32,32,32,32,32,65,83,83,69,82,84,40,33,99,101,108,108,45,62,102,114,101,101,99,100,114,32,124,124,32,40,99,101,108,108,45,62,114,101,102,115,32,62,32,49,41,41,59,10,
            32,32,32,32,32,32,32,32,99,101,108,108,45,62,114,101,102,115,45,45,59,10,
            32,32,32,32,32,32,32,32,118,111,105,100,42,32,112,116,114,32,61,32,40,118,111,105,100,42,41,40,99,101,108,108,45,62,99,100,114,46,100,97,116,97,46,99,104,97,114,112,116,114,32,43,32,121,41,59,10,
            32,32,32,32,32,32,32,32,115,105,122,101,95,116,32,115,105,122,101,32,61,32,40,99,101,108,108,45,62,99,97,114,46,100,97,116,97,46,117,115,105,122,101,32,62,32,121,32,63,32,99,101,108,108,45,62,99,97,114,46,100,97,116,97,46,117,115,105,122,101,32,45,32,121,32,58,32,48,41,59,10,
            32,32,32,32,32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,109,107,112,116,114,95,115,104,97,114,101,100,40,112,116,114,44,115,105,122,101,41,41,59,10,
            32,32,32,32,125,32,101,108,115,101,32,123,10,
            32,32,32,32,32,32,32,32,112,117,115,104,95,117,54,52,40,121,32,43,32,118,112,46,100,97,116,97,46,117,54,52,41,59,10,
            32,32,32,32,125,10,
            125,10,
            35,100,101,102,105,110,101,32,109,119,95,112,114,105,109,95,112,116,114,95,115,105,122,101,40,41,32,112,117,115,104,95,117,54,52,40,40,117,105,110,116,54,52,95,116,41,115,105,122,101,111,102,40,118,111,105,100,42,41,41,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,116,114,95,97,108,108,111,99,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,105,110,116,54,52,95,116,32,112,115,105,122,101,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,105,102,32,40,112,115,105,122,101,32,62,32,48,41,32,123,10,
            32,32,32,32,32,32,32,32,115,105,122,101,95,116,32,115,105,122,101,32,61,32,40,115,105,122,101,95,116,41,112,115,105,122,101,59,10,
            32,32,32,32,32,32,32,32,118,111,105,100,42,32,112,116,114,32,61,32,99,97,108,108,111,99,40,49,44,115,105,122,101,41,59,10,
            32,32,32,32,32,32,32,32,86,65,76,32,118,32,61,32,109,107,112,116,114,95,111,119,110,101,100,40,112,116,114,44,32,115,105,122,101,41,59,10,
            32,32,32,32,32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,118,41,59,10,
            32,32,32,32,125,32,101,108,115,101,32,123,10,
            32,32,32,32,32,32,32,32,112,117,115,104,95,117,54,52,40,48,41,59,10,
            32,32,32,32,125,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,42,32,97,108,108,111,99,95,98,117,116,95,99,111,112,121,32,40,115,105,122,101,95,116,32,100,115,116,110,44,32,118,111,105,100,42,32,115,114,99,44,32,115,105,122,101,95,116,32,115,114,99,110,41,32,123,10,
            32,32,32,32,118,111,105,100,42,32,100,115,116,32,61,32,99,97,108,108,111,99,40,49,44,100,115,116,110,41,59,10,
            32,32,32,32,105,102,32,40,115,114,99,32,38,38,32,115,114,99,110,41,32,123,10,
            32,32,32,32,32,32,32,32,115,105,122,101,95,116,32,99,112,121,110,32,61,32,40,100,115,116,110,32,62,32,115,114,99,110,41,32,63,32,115,114,99,110,32,58,32,100,115,116,110,59,10,
            32,32,32,32,32,32,32,32,109,101,109,99,112,121,40,100,115,116,44,32,115,114,99,44,32,99,112,121,110,41,59,10,
            32,32,32,32,125,10,
            32,32,32,32,114,101,116,117,114,110,32,100,115,116,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,116,114,95,114,101,97,108,108,111,99,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,105,110,116,54,52,95,116,32,112,115,105,122,101,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,86,65,76,32,118,112,116,114,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,102,32,40,112,115,105,122,101,32,60,61,32,48,41,32,123,10,
            32,32,32,32,32,32,32,32,100,101,99,114,101,102,40,118,112,116,114,41,59,10,
            32,32,32,32,32,32,32,32,112,117,115,104,95,117,54,52,40,48,41,59,10,
            32,32,32,32,32,32,32,32,114,101,116,117,114,110,59,10,
            32,32,32,32,125,32,101,108,115,101,32,123,10,
            32,32,32,32,32,32,32,32,115,105,122,101,95,116,32,110,101,119,95,115,105,122,101,32,61,32,40,115,105,122,101,95,116,41,112,115,105,122,101,59,10,
            32,32,32,32,32,32,32,32,118,111,105,100,42,32,111,108,100,95,112,116,114,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,116,114,41,59,10,
            32,32,32,32,32,32,32,32,115,105,122,101,95,116,32,111,108,100,95,115,105,122,101,32,61,32,118,97,108,117,101,95,112,116,114,95,115,105,122,101,40,118,112,116,114,41,59,10,
            32,32,32,32,32,32,32,32,118,111,105,100,42,32,110,101,119,95,112,116,114,32,61,32,97,108,108,111,99,95,98,117,116,95,99,111,112,121,40,110,101,119,95,115,105,122,101,44,32,111,108,100,95,112,116,114,44,32,111,108,100,95,115,105,122,101,41,59,10,
            32,32,32,32,32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,109,107,112,116,114,95,111,119,110,101,100,40,110,101,119,95,112,116,114,44,32,110,101,119,95,115,105,122,101,41,41,59,10,
            32,32,32,32,32,32,32,32,100,101,99,114,101,102,40,118,112,116,114,41,59,10,
            32,32,32,32,125,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,116,114,95,99,111,112,121,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,100,115,116,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,105,108,101,110,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,86,65,76,32,118,115,114,99,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,118,111,105,100,42,32,115,114,99,32,61,32,118,97,108,117,101,95,112,116,114,40,118,115,114,99,41,59,10,
            32,32,32,32,118,111,105,100,42,32,100,115,116,32,61,32,118,97,108,117,101,95,112,116,114,40,118,100,115,116,41,59,10,
            32,32,32,32,105,102,32,40,115,114,99,32,38,38,32,100,115,116,32,38,38,32,40,105,108,101,110,32,62,32,48,41,41,32,123,10,
            32,32,32,32,32,32,32,32,109,101,109,99,112,121,40,100,115,116,44,32,115,114,99,44,32,40,115,105,122,101,95,116,41,105,108,101,110,41,59,10,
            32,32,32,32,125,10,
            32,32,32,32,100,101,99,114,101,102,40,118,115,114,99,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,100,115,116,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,116,114,95,102,105,108,108,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,100,115,116,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,105,108,101,110,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,105,110,116,54,52,95,116,32,118,97,108,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,118,111,105,100,42,32,100,115,116,32,61,32,118,97,108,117,101,95,112,116,114,40,118,100,115,116,41,59,10,
            32,32,32,32,105,102,32,40,100,115,116,32,38,38,32,40,105,108,101,110,32,62,32,48,41,41,32,123,10,
            32,32,32,32,32,32,32,32,109,101,109,115,101,116,40,100,115,116,44,32,40,105,110,116,41,118,97,108,44,32,40,115,105,122,101,95,116,41,105,108,101,110,41,59,10,
            32,32,32,32,125,10,
            32,32,32,32,100,101,99,114,101,102,40,118,100,115,116,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,116,114,95,114,97,119,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,116,114,32,61,32,116,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,118,111,105,100,32,42,112,116,114,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,116,114,41,59,10,
            32,32,32,32,112,117,115,104,95,112,116,114,40,112,116,114,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,115,116,114,95,101,113,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,112,116,114,49,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,118,112,116,114,50,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,99,111,110,115,116,32,99,104,97,114,42,32,112,116,114,49,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,116,114,49,41,59,10,
            32,32,32,32,99,111,110,115,116,32,99,104,97,114,42,32,112,116,114,50,32,61,32,118,97,108,117,101,95,112,116,114,40,118,112,116,114,50,41,59,10,
            32,32,32,32,98,111,111,108,32,114,101,115,117,108,116,32,61,32,40,33,112,116,114,49,32,124,124,32,33,112,116,114,50,41,32,63,32,40,112,116,114,49,32,61,61,32,112,116,114,50,41,32,58,32,115,116,114,99,109,112,40,112,116,114,49,44,112,116,114,50,41,32,61,61,32,48,59,10,
            32,32,32,32,112,117,115,104,95,98,111,111,108,40,114,101,115,117,108,116,41,59,10,
            32,32,32,32,100,101,99,114,101,102,40,118,112,116,114,49,41,59,32,100,101,99,114,101,102,40,118,112,116,114,50,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,115,116,114,95,97,108,108,111,99,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,47,47,32,115,116,114,105,110,103,115,32,104,97,118,101,32,97,32,52,32,98,121,116,101,32,115,101,110,116,105,110,101,108,32,111,102,32,122,101,114,111,115,10,
            32,32,32,32,105,110,116,54,52,95,116,32,112,115,105,122,101,32,61,32,112,111,112,95,105,54,52,40,41,59,10,
            32,32,32,32,112,117,115,104,95,105,54,52,40,112,115,105,122,101,32,43,40,112,115,105,122,101,32,62,61,32,48,32,63,32,52,32,58,32,48,41,41,59,10,
            32,32,32,32,109,119,95,112,114,105,109,95,112,116,114,95,97,108,108,111,99,40,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,115,116,114,95,98,97,115,101,32,40,118,111,105,100,41,32,123,32,125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,115,116,114,95,115,105,122,101,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,32,61,32,115,116,97,99,107,91,115,116,97,99,107,95,99,111,117,110,116,101,114,93,59,10,
            32,32,32,32,105,102,32,40,33,118,46,100,97,116,97,46,117,54,52,41,32,123,10,
            32,32,32,32,32,32,32,32,112,117,115,104,95,117,54,52,40,48,41,59,10,
            32,32,32,32,125,32,101,108,115,101,32,105,102,32,40,118,46,116,97,103,32,61,61,32,84,65,71,95,73,78,84,41,32,123,10,
            32,32,32,32,32,32,32,32,112,117,115,104,95,117,54,52,40,40,117,105,110,116,54,52,95,116,41,115,116,114,108,101,110,40,118,46,100,97,116,97,46,112,116,114,41,41,59,10,
            32,32,32,32,125,32,101,108,115,101,32,123,10,
            32,32,32,32,32,32,32,32,112,117,115,104,95,105,54,52,40,118,97,108,117,101,95,112,116,114,95,115,105,122,101,40,118,41,45,52,41,59,10,
            32,32,32,32,125,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,97,99,107,95,110,105,108,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,112,117,115,104,95,117,54,52,40,48,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,97,99,107,95,99,111,110,115,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,99,100,114,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,99,97,114,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,109,107,99,101,108,108,40,99,97,114,44,99,100,114,41,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,112,97,99,107,95,117,110,99,111,110,115,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,118,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,99,97,114,44,99,100,114,59,10,
            32,32,32,32,118,97,108,117,101,95,117,110,99,111,110,115,40,118,44,32,38,99,97,114,44,32,38,99,100,114,41,59,10,
            32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,99,97,114,41,59,10,
            32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,99,100,114,41,59,10,
            32,32,32,32,105,110,99,114,101,102,40,99,97,114,41,59,32,105,110,99,114,101,102,40,99,100,114,41,59,32,100,101,99,114,101,102,40,118,41,59,10,
            125,10,
            10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,109,117,116,95,110,101,119,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,99,97,114,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,99,100,114,32,61,32,123,48,125,59,10,
            32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,109,107,99,101,108,108,95,114,97,119,40,99,97,114,44,99,100,114,44,102,97,108,115,101,41,41,59,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,109,117,116,95,103,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,109,117,116,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,105,102,32,40,40,109,117,116,46,116,97,103,32,61,61,32,84,65,71,95,73,78,84,41,32,38,38,32,109,117,116,46,100,97,116,97,46,112,116,114,41,32,123,10,
            32,32,32,32,32,32,32,32,86,65,76,32,118,32,61,32,42,109,117,116,46,100,97,116,97,46,118,97,108,112,116,114,59,10,
            32,32,32,32,32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,118,41,59,10,
            32,32,32,32,32,32,32,32,105,110,99,114,101,102,40,118,41,59,10,
            32,32,32,32,125,32,101,108,115,101,32,123,10,
            32,32,32,32,32,32,32,32,86,65,76,32,99,97,114,44,99,100,114,59,10,
            32,32,32,32,32,32,32,32,118,97,108,117,101,95,117,110,99,111,110,115,40,109,117,116,44,32,38,99,97,114,44,32,38,99,100,114,41,59,10,
            32,32,32,32,32,32,32,32,65,83,83,69,82,84,40,40,99,100,114,46,116,97,103,32,61,61,32,48,41,32,38,38,32,40,99,100,114,46,100,97,116,97,46,117,54,52,32,61,61,32,48,41,41,59,10,
            32,32,32,32,32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,99,97,114,41,59,10,
            32,32,32,32,32,32,32,32,105,110,99,114,101,102,40,99,97,114,41,59,10,
            32,32,32,32,32,32,32,32,100,101,99,114,101,102,40,109,117,116,41,59,10,
            32,32,32,32,125,10,
            125,10,
            115,116,97,116,105,99,32,118,111,105,100,32,109,119,95,112,114,105,109,95,109,117,116,95,115,101,116,32,40,118,111,105,100,41,32,123,10,
            32,32,32,32,86,65,76,32,109,117,116,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,86,65,76,32,110,101,119,118,97,108,32,61,32,112,111,112,95,118,97,108,117,101,40,41,59,10,
            32,32,32,32,112,117,115,104,95,118,97,108,117,101,40,109,117,116,41,59,10,
            32,32,32,32,105,102,32,40,40,109,117,116,46,116,97,103,32,61,61,32,84,65,71,95,73,78,84,41,32,38,38,32,109,117,116,46,100,97,116,97,46,112,116,114,41,32,123,10,
            32,32,32,32,32,32,32,32,86,65,76,32,111,108,100,118,97,108,32,61,32,42,109,117,116,46,100,97,116,97,46,118,97,108,112,116,114,59,10,
            32,32,32,32,32,32,32,32,42,109,117,116,46,100,97,116,97,46,118,97,108,112,116,114,32,61,32,110,101,119,118,97,108,59,10,
            32,32,32,32,32,32,32,32,100,101,99,114,101,102,40,111,108,100,118,97,108,41,59,10,
            32,32,32,32,125,32,101,108,115,101,32,123,10,
            32,32,32,32,32,32,32,32,115,105,122,101,95,116,32,105,32,61,32,103,101,116,95,99,101,108,108,95,105,110,100,101,120,40,109,117,116,41,59,10,
            32,32,32,32,32,32,32,32,105,102,32,40,105,41,32,123,10,
            32,32,32,32,32,32,32,32,32,32,32,32,67,79,78,83,32,42,99,101,108,108,32,61,32,104,101,97,112,43,105,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,86,65,76,32,111,108,100,118,97,108,32,61,32,99,101,108,108,45,62,99,97,114,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,99,101,108,108,45,62,99,97,114,32,61,32,110,101,119,118,97,108,59,10,
            32,32,32,32,32,32,32,32,32,32,32,32,100,101,99,114,101,102,40,111,108,100,118,97,108,41,59,10,
            32,32,32,32,32,32,32,32,125,32,101,108,115,101,32,123,10,
            32,32,32,32,32,32,32,32,32,32,32,32,100,101,99,114,101,102,40,110,101,119,118,97,108,41,59,10,
            32,32,32,32,32,32,32,32,125,10,
            32,32,32,32,125,10,
            125,10,
            10,
            47,42,32,71,69,78,69,82,65,84,69,68,32,67,57,57,32,42,47,10,
            
            0,0,0,0,
        };
        push_ptr((void*)b);
    }
    mw__2E_();
    mw__2E_lf();
}

static void mw_c99_buffers_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_buffers_21__1);
    mw_prim_pack_cons();
    mw_Buffer_2E_for();
    mw__2E_lf();
}

static void mw_c99_buffer_21_ (void){
    mw_dup();
    mw_buffer_name();
    mw__40_();
    mw__2E_w();
    push_ptr(" {\0\0\0\0");
    mw__3B_();
    push_ptr("    static uint8_t b[\0\0\0\0");
    mw__2E_();
    mw_dup();
    mw_buffer_size();
    mw__40_();
    mw__2E_n();
    push_ptr("] = {0};\0\0\0\0");
    mw__3B_();
    push_ptr("    push_ptr(&b);\0\0\0\0");
    mw__3B_();
    push_ptr("}\0\0\0\0");
    mw__3B_();
    mw_drop();
}

static void mw_c99_variables_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_variables_21__1);
    mw_prim_pack_cons();
    mw_Variable_2E_for();
    mw__2E_lf();
}

static void mw_c99_variable_21_ (void){
    push_ptr("void mw_\0\0\0\0");
    mw__2E_();
    mw_variable_name();
    mw__40_();
    mw__2E_name();
    push_ptr("() {\0\0\0\0");
    mw__3B_();
    push_ptr("    static VAL v = {0};\0\0\0\0");
    mw__3B_();
    push_ptr("    push_ptr(&v);\0\0\0\0");
    mw__3B_();
    push_ptr("}\0\0\0\0");
    mw__3B_();
}

static void mw_c99_tags_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_tags_21__1);
    mw_prim_pack_cons();
    mw_Tag_2E_for();
    mw__2E_lf();
}

static void mw_c99_tag_21_ (void){
    mw_dup();
    mw_tag_name();
    mw__40_();
    mw__2E_w();
    push_ptr(" {\0\0\0\0");
    mw__3B_();
    mw_tag_is_transparent_3F_();
    if (pop_u64()) {
        mw_drop();
    } else {
        mw_tag_num_inputs_3F_();
        push_i64(0LL);
        mw__3D__3D_();
        if (pop_u64()) {
            push_ptr("    push_u64(\0\0\0\0");
            mw__2E_();
            mw_tag_value();
            mw__40_();
            mw__2E_n();
            push_ptr("LL);\0\0\0\0");
            mw__3B_();
        } else {
            push_ptr("    VAL car = pop_value();\0\0\0\0");
            mw__3B_();
            mw_tag_num_inputs_3F_();
            mw_1_();
            push_u64(0);
            push_fnptr(&mb_c99_tag_21__5);
            mw_prim_pack_cons();
            mw_repeat();
            push_ptr("    VAL tag = mku64(\0\0\0\0");
            mw__2E_();
            mw_tag_value();
            mw__40_();
            mw__2E_n();
            push_ptr("LL);\0\0\0\0");
            mw__3B_();
            push_ptr("    car = mkcell(car, tag);\0\0\0\0");
            mw__3B_();
            push_ptr("    push_value(car);\0\0\0\0");
            mw__3B_();
        }
    }
    push_ptr("}\0\0\0\0");
    mw__3B_();
}

static void mw_c99_externals_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_externals_21__1);
    mw_prim_pack_cons();
    mw_External_2E_for();
    mw__2E_lf();
}

static void mw_c99_external_21_ (void){
    mw_dup();
    mw_external_sig();
    mw__40_();
    mw_sig_arity();
    mw_dup();
    push_i64(2LL);
    mw__3E__3D_();
    if (pop_u64()) {
        push_ptr("can't declare external with multiple return values\0\0\0\0");
        mw_panic_21_();
    } else {
        mw_dup();
        push_i64(1LL);
        mw__3E__3D_();
        if (pop_u64()) {
            push_ptr("int64_t \0\0\0\0");
            mw__2E_();
        } else {
            push_ptr("void \0\0\0\0");
            mw__2E_();
        }
    }
    push_u64(0);
    push_fnptr(&mb_c99_external_21__5);
    mw_prim_pack_cons();
    mw_dip2();
    push_ptr(" (\0\0\0\0");
    mw__2E_();
    mw_over();
    mw_dup();
    mw_0_3E_();
    if (pop_u64()) {
        push_ptr("int64_t\0\0\0\0");
        mw__2E_();
        mw_1_();
        push_u64(0);
        push_fnptr(&mb_c99_external_21__7);
        mw_prim_pack_cons();
        mw_repeat();
    } else {
        mw_drop();
        push_ptr("void\0\0\0\0");
        mw__2E_();
    }
    push_ptr(");\0\0\0\0");
    mw__3B_();
    push_ptr("static void mw_\0\0\0\0");
    mw__2E_();
    push_u64(0);
    push_fnptr(&mb_c99_external_21__9);
    mw_prim_pack_cons();
    mw_dip2();
    push_ptr(" (void) {\0\0\0\0");
    mw__3B_();
    mw_over();
    push_u64(0);
    push_fnptr(&mb_c99_external_21__10);
    mw_prim_pack_cons();
    mw_countdown();
    mw_dup();
    mw_0_3E_();
    if (pop_u64()) {
        push_ptr("    push_i64(\0\0\0\0");
    } else {
        push_ptr("    \0\0\0\0");
    }
    mw__2E_();
    push_u64(0);
    push_fnptr(&mb_c99_external_21__13);
    mw_prim_pack_cons();
    mw_dip2();
    push_ptr("(\0\0\0\0");
    mw__2E_();
    {
        VAL d2 = pop_value();
        mw_dup();
        mw_0_3E_();
        if (pop_u64()) {
            mw_dup();
            mw_1_();
            mw_dup();
            push_u64(0);
            push_fnptr(&mb_c99_external_21__16);
            mw_prim_pack_cons();
            mw_count();
            push_ptr("x\0\0\0\0");
            mw__2E_();
            mw__2E_n();
        } else {
            mw_id();
        }
        push_value(d2);
    }
    push_ptr(")\0\0\0\0");
    mw__2E_();
    mw_dup();
    mw_0_3E_();
    if (pop_u64()) {
        push_ptr(");\0\0\0\0");
    } else {
        push_ptr(";\0\0\0\0");
    }
    mw__3B_();
    push_ptr("}\0\0\0\0");
    mw__3B_();
    mw_drop3();
}

static void mw_c99_nest (void){
    {
        VAL var_f = pop_value();
        mw_c99_depth();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_c99_nest_2);
        mw_prim_pack_cons();
        mw_modify();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        mw_c99_depth();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_c99_nest_3);
        mw_prim_pack_cons();
        mw_modify();
        decref(var_f);
    }
}

static void mw_c99_indent (void){
    mw_c99_depth();
    mw__40_();
    push_u64(0);
    push_fnptr(&mb_c99_indent_1);
    mw_prim_pack_cons();
    mw_repeat();
}

static void mw_c99_line (void){
    {
        VAL var_f = pop_value();
        mw_c99_indent();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        mw__2E_lf();
        decref(var_f);
    }
}

static void mw_c99_call_21_ (void){
    {
        VAL d2 = pop_value();
        mw_c99_args_push_21_();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_c99_call_21__2);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mw_c99_arrow_21_ (void){
    mw_arrow_atoms();
    mw__40_();
    push_u64(0);
    push_fnptr(&mb_c99_arrow_21__1);
    mw_prim_pack_cons();
    mw_for();
}

static void mw_c99_atom_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_atom_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_atom_op();
    mw__40_();
    mw_c99_args_op_21_();
}

static void mw_c99_args_op_21_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_drop();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_c99_int_21_();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_c99_str_21_();
            break;
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_c99_constant_21_();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_word_name();
            mw__40_();
            mw_c99_call_21_();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_external_name();
            mw__40_();
            mw_c99_call_21_();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_buffer_name();
            mw__40_();
            mw_c99_call_21_();
            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_variable_name();
            mw__40_();
            mw_c99_call_21_();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_field_name();
            mw__40_();
            mw_c99_call_21_();
            break;
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_tag_name();
            mw__40_();
            mw_c99_call_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_c99_prim_21_();
            break;
        case 11LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_c99_match_21_();
            break;
        case 12LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_c99_lambda_21_();
            break;
        case 13LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_c99_var_21_();
            break;
        case 14LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_c99_block_push_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_c99_int_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_int_21__1);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mw_c99_str_21_ (void){
    mw_dup();
    mw_str_size();
    push_i64(4090LL);
    mw__3E_();
    if (pop_u64()) {
        push_u64(0);
        push_fnptr(&mb_c99_str_21__2);
        mw_prim_pack_cons();
        mw_c99_line();
        push_u64(0);
        push_fnptr(&mb_c99_str_21__3);
        mw_prim_pack_cons();
        mw_c99_nest();
        push_u64(0);
        push_fnptr(&mb_c99_str_21__13);
        mw_prim_pack_cons();
        mw_c99_line();
    } else {
        push_u64(0);
        push_fnptr(&mb_c99_str_21__15);
        mw_prim_pack_cons();
        mw_c99_line();
    }
}

static void mw_c99_constant_21_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_c99_int_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_c99_str_21_();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_c99_block_push_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_c99_string_char_21_ (void){
    mw_is_backslash_3F_();
    if (pop_u64()) {
        mw_drop();
        push_ptr("\\\\\0\0\0\0");
        mw__2E_();
    } else {
        mw_is_quote_3F_();
        if (pop_u64()) {
            mw_drop();
            push_ptr("\\\"\0\0\0\0");
            mw__2E_();
        } else {
            mw_dup();
            mw_Char__3E_Int();
            push_i64(32LL);
            push_i64(127LL);
            mw_in_range();
            if (pop_u64()) {
                mw__2E_c();
            } else {
                mw_dup();
                mw_Char__3E_Int();
                push_i64(9LL);
                mw__3D__3D_();
                if (pop_u64()) {
                    mw_drop();
                    push_ptr("\\t\0\0\0\0");
                    mw__2E_();
                } else {
                    mw_dup();
                    mw_Char__3E_Int();
                    push_i64(10LL);
                    mw__3D__3D_();
                    if (pop_u64()) {
                        mw_drop();
                        push_ptr("\\n\0\0\0\0");
                        mw__2E_();
                    } else {
                        mw_dup();
                        mw_Char__3E_Int();
                        push_i64(13LL);
                        mw__3D__3D_();
                        if (pop_u64()) {
                            mw_drop();
                            push_ptr("\\r\0\0\0\0");
                            mw__2E_();
                        } else {
                            mw_char_bytes();
                            push_u64(0);
                            push_fnptr(&mb_c99_string_char_21__13);
                            mw_prim_pack_cons();
                            mw_for();
                        }
                    }
                }
            }
        }
    }
}

static void mw_c99_prim_21_ (void){
    switch (get_top_data_tag()) {
        case 4LL:
            mw_prim_drop();
            switch (get_top_data_tag()) {
                case 1LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    push_u64(0);
                    push_fnptr(&mb_c99_prim_21__3);
                    mw_prim_pack_cons();
                    mw_c99_line();
                    push_u64(0);
                    push_fnptr(&mb_c99_prim_21__4);
                    mw_prim_pack_cons();
                    mw_c99_nest();
                    push_u64(0);
                    push_fnptr(&mb_c99_prim_21__7);
                    mw_prim_pack_cons();
                    mw_c99_line();
                    break;
                default:
                    mw_PRIM_CORE_DIP();
                    mw_c99_prim_default_21_();
                    break;
            
}            break;
        case 5LL:
            mw_prim_drop();
            switch (get_top_data_tag()) {
                case 2LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_prim_pack_uncons(); mw_prim_swap();
                    push_u64(0);
                    push_fnptr(&mb_c99_prim_21__11);
                    mw_prim_pack_cons();
                    mw_c99_line();
                    push_u64(0);
                    push_fnptr(&mb_c99_prim_21__12);
                    mw_prim_pack_cons();
                    mw_c99_nest();
                    push_u64(0);
                    push_fnptr(&mb_c99_prim_21__13);
                    mw_prim_pack_cons();
                    mw_c99_line();
                    push_u64(0);
                    push_fnptr(&mb_c99_prim_21__14);
                    mw_prim_pack_cons();
                    mw_c99_nest();
                    push_u64(0);
                    push_fnptr(&mb_c99_prim_21__15);
                    mw_prim_pack_cons();
                    mw_c99_line();
                    break;
                default:
                    mw_PRIM_CORE_IF();
                    mw_c99_prim_default_21_();
                    break;
            
}            break;
        case 6LL:
            mw_prim_drop();
            switch (get_top_data_tag()) {
                case 2LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_prim_pack_uncons(); mw_prim_swap();
                    push_u64(0);
                    push_fnptr(&mb_c99_prim_21__19);
                    mw_prim_pack_cons();
                    mw_c99_line();
                    push_u64(0);
                    push_fnptr(&mb_c99_prim_21__20);
                    mw_prim_pack_cons();
                    mw_c99_nest();
                    push_u64(0);
                    push_fnptr(&mb_c99_prim_21__22);
                    mw_prim_pack_cons();
                    mw_c99_line();
                    break;
                default:
                    mw_PRIM_CORE_WHILE();
                    mw_c99_prim_default_21_();
                    break;
            
}            break;
        default:
            mw_c99_prim_default_21_();
            break;
    
}}

static void mw_c99_prim_default_21_ (void){
    mw_prim_name();
    mw__40_();
    mw_c99_call_21_();
}

static void mw_c99_args_push_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_args_push_21__1);
    mw_prim_pack_cons();
    mw_for();
}

static void mw_c99_arg_push_21_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_c99_block_push_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_c99_var_push_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_c99_arg_run_21_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_block_arrow();
            mw_force_21_();
            mw_c99_arrow_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_c99_var_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw__2E_var (void){
    push_ptr("var_\0\0\0\0");
    mw__2E_();
    mw_var_name();
    mw__40_();
    mw__2E_name();
}

static void mw__2E_param (void){
    mw_Param__3E_Var();
    mw__2E_var();
}

static void mw_c99_pack_ctx_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_pack_ctx_21__1);
    mw_prim_pack_cons();
    mw_c99_line();
    mw_ctx_physical_vars();
    push_u64(0);
    push_fnptr(&mb_c99_pack_ctx_21__2);
    mw_prim_pack_cons();
    mw_for();
}

static void mw_c99_unpack_ctx_21_ (void){
    mw_ctx_physical_vars();
    push_u64(0);
    push_fnptr(&mb_c99_unpack_ctx_21__1);
    mw_prim_pack_cons();
    mw_reverse_for();
    push_u64(0);
    push_fnptr(&mb_c99_unpack_ctx_21__4);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mw_c99_decref_ctx_21_ (void){
    mw_ctx_physical_vars();
    push_u64(0);
    push_fnptr(&mb_c99_decref_ctx_21__1);
    mw_prim_pack_cons();
    mw_reverse_for();
}

static void mw_c99_block_push_21_ (void){
    mw_dup();
    mw_block_arrow();
    mw_force_21_();
    mw_arrow_ctx();
    mw__40_();
    mw_c99_pack_ctx_21_();
    push_u64(0);
    push_fnptr(&mb_c99_block_push_21__1);
    mw_prim_pack_cons();
    mw_c99_line();
    push_u64(0);
    push_fnptr(&mb_c99_block_push_21__2);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mw_c99_var_21_ (void){
    mw_dup();
    mw_var_auto_run();
    mw__40_();
    {
        VAL d2 = pop_value();
        mw_c99_var_push_21_();
        push_value(d2);
    }
    if (pop_u64()) {
        push_u64(0);
        push_fnptr(&mb_c99_var_21__3);
        mw_prim_pack_cons();
        mw_c99_line();
    } else {
        mw_id();
    }
}

static void mw_c99_var_push_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_var_push_21__1);
    mw_prim_pack_cons();
    mw_c99_line();
    push_u64(0);
    push_fnptr(&mb_c99_var_push_21__2);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mw_c99_lambda_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_lambda_21__1);
    mw_prim_pack_cons();
    mw_c99_line();
    push_u64(0);
    push_fnptr(&mb_c99_lambda_21__2);
    mw_prim_pack_cons();
    mw_c99_nest();
    push_u64(0);
    push_fnptr(&mb_c99_lambda_21__7);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mw_c99_match_21_ (void){
    mw_match_is_transparent_3F_();
    if (pop_u64()) {
        mw_dup();
        mw_match_cases();
        mw__40_();
        mw_first();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_match_token();
                mw__40_();
                push_ptr("codegen: unexpected number of cases in transparent match\0\0\0\0");
                mw_emit_fatal_error_21_();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_case_body();
                mw__40_();
                mw_c99_arrow_21_();
                mw_drop();
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}    } else {
        push_u64(0);
        push_fnptr(&mb_c99_match_21__5);
        mw_prim_pack_cons();
        mw_c99_line();
        push_u64(0);
        push_fnptr(&mb_c99_match_21__6);
        mw_prim_pack_cons();
        mw_c99_nest();
        push_u64(0);
        push_fnptr(&mb_c99_match_21__11);
        mw_prim_pack_cons();
        mw_c99_line();
        mw__2E_();
    }
}

static void mw_c99_case_21_ (void){
    mw_dup();
    mw_case_pattern();
    mw__40_();
    mw_c99_pattern_21_();
    push_u64(0);
    push_fnptr(&mb_c99_case_21__1);
    mw_prim_pack_cons();
    mw_c99_nest();
}

static void mw_c99_pattern_21_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_c99_pattern_21__2);
            mw_prim_pack_cons();
            mw_c99_line();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            push_u64(0);
            push_fnptr(&mb_c99_pattern_21__4);
            mw_prim_pack_cons();
            mw_c99_line();
            push_u64(0);
            push_fnptr(&mb_c99_pattern_21__5);
            mw_prim_pack_cons();
            mw_c99_nest();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_c99_word_sigs_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_word_sigs_21__1);
    mw_prim_pack_cons();
    mw_Word_2E_for();
    mw__2E_lf();
}

static void mw_c99_word_sig_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_word_sig_21__1);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mw_c99_block_sigs_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_block_sigs_21__1);
    mw_prim_pack_cons();
    mw_Block_2E_for();
    mw__2E_lf();
}

static void mw_c99_block_sig_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_block_sig_21__1);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mw_c99_field_sigs_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_field_sigs_21__1);
    mw_prim_pack_cons();
    mw_Field_2E_for();
    mw__2E_lf();
}

static void mw_c99_field_sig_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_field_sig_21__1);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mw_c99_block_defs_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_block_defs_21__1);
    mw_prim_pack_cons();
    mw_Block_2E_for();
    mw__2E_lf();
}

static void mw_c99_block_def_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_block_def_21__1);
    mw_prim_pack_cons();
    mw_c99_line();
    push_u64(0);
    push_fnptr(&mb_c99_block_def_21__2);
    mw_prim_pack_cons();
    mw_c99_nest();
    push_u64(0);
    push_fnptr(&mb_c99_block_def_21__3);
    mw_prim_pack_cons();
    mw_c99_line();
    mw__2E_lf();
}

static void mw__2E_block (void){
    push_ptr("mb_\0\0\0\0");
    mw__2E_();
    mw_dup();
    mw_block_arrow();
    mw_force_21_();
    mw_dup();
    mw_arrow_home();
    mw__40_();
    mw_is_nil_3F_();
    if (pop_u64()) {
        mw_drop2();
        mw_Block_2E_id();
        mw__2E_n();
    } else {
        mw_word_name();
        mw__40_();
        mw__2E_name();
        push_ptr("_\0\0\0\0");
        mw__2E_();
        mw_arrow_homeidx();
        mw__40_();
        mw__2E_n();
        mw_drop();
    }
}

static void mw_c99_word_defs_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_word_defs_21__1);
    mw_prim_pack_cons();
    mw_Word_2E_for();
    mw__2E_lf();
}

static void mw_c99_word_def_21_ (void){
    mw_dup();
    mw_word_name();
    mw__40_();
    mw__2E_w();
    push_ptr("{\0\0\0\0");
    mw__3B_();
    mw_word_arrow();
    mw_force_21_();
    push_u64(0);
    push_fnptr(&mb_c99_word_def_21__1);
    mw_prim_pack_cons();
    mw_c99_nest();
    push_ptr("}\0\0\0\0");
    mw__3B__3B_();
}

static void mw_c99_field_defs_21_ (void){
    push_u64(0);
    push_fnptr(&mb_c99_field_defs_21__1);
    mw_prim_pack_cons();
    mw_Field_2E_for();
    mw__2E_lf();
}

static void mw_c99_field_def_21_ (void){
    push_ptr("static VAL* fieldptr_\0\0\0\0");
    mw__2E_();
    mw_dup();
    mw_field_name();
    mw__40_();
    mw__2E_name();
    push_ptr(" (size_t i) {\0\0\0\0");
    mw__3B_();
    push_ptr("    static struct VAL * p = 0;\0\0\0\0");
    mw__3B_();
    push_ptr("    size_t m = \0\0\0\0");
    mw__2E_();
    mw_TABLE_MAX_SIZE();
    mw__2E_n();
    push_ptr(";\0\0\0\0");
    mw__3B_();
    push_ptr("    if (!p) { p = calloc(m, sizeof *p); }\0\0\0\0");
    mw__3B_();
    push_ptr("    if (i>=m) { write(2,\"table too big\\n\",14); exit(123); }\0\0\0\0");
    mw__3B_();
    push_ptr("    return p+i;\0\0\0\0");
    mw__3B_();
    push_ptr("}\0\0\0\0");
    mw__3B__3B_();
    mw_dup();
    mw_field_name();
    mw__40_();
    mw__2E_w();
    push_ptr("{\0\0\0\0");
    mw__3B_();
    push_ptr("    size_t index = (size_t)pop_u64();\0\0\0\0");
    mw__3B_();
    push_ptr("    VAL *v = fieldptr_\0\0\0\0");
    mw__2E_();
    mw_dup();
    mw_field_name();
    mw__40_();
    mw__2E_name();
    push_ptr("(index);\0\0\0\0");
    mw__3B_();
    push_ptr("    push_ptr(v);\0\0\0\0");
    mw__3B_();
    push_ptr("}\0\0\0\0");
    mw__3B__3B_();
    mw_drop();
}

static void mw_c99_main_21_ (void){
    push_ptr("int main (int argc, char** argv) {\0\0\0\0");
    mw__3B_();
    push_ptr("    global_argc = argc;\0\0\0\0");
    mw__3B_();
    push_ptr("    global_argv = argv;\0\0\0\0");
    mw__3B_();
    push_u64(0);
    push_fnptr(&mb_c99_main_21__1);
    mw_prim_pack_cons();
    mw_c99_nest();
    push_ptr("    return 0;\0\0\0\0");
    mw__3B_();
    push_ptr("}\0\0\0\0");
    mw__3B__3B_();
}

static void mw_unCTX (void){
    mw_id();
}

static void mw_ctx_empty (void){
    mw_nil();
}

static void mw_ctx_is_physically_empty (void){
    mw_ctx_physical_vars();
    mw_is_nil();
}

static void mw_ctx_new_21_ (void){
    {
        VAL d2 = pop_value();
        mw_unCTX();
        push_value(d2);
    }
    mw_snoc();
    mw_CTX();
}

static void mw_ctx_vars (void){
    mw_unCTX();
}

static void mw_ctx_physical_vars (void){
    mw_ctx_vars();
    push_u64(0);
    push_fnptr(&mb_ctx_physical_vars_1);
    mw_prim_pack_cons();
    mw_filter();
}

static void mw_ctx_lookup (void){
    mw_unCTX();
    push_u64(0);
    push_fnptr(&mb_ctx_lookup_1);
    mw_prim_pack_cons();
    mw_reverse_find();
    mw_nip();
}

static void mw_ctx_len (void){
    mw_len();
}

static void mw_ctx_fresh_name_21_ (void){
    push_ptr("_x\0\0\0\0");
    mw_over();
    mw_ctx_len();
    mw_int_show();
    mw_str_cat();
    mw_name_new_21_();
}

static void mw_ctx_make_fresh_stack_type_var_21_ (void){
    mw_PRIM_TYPE_STACK();
    mw_TPrim();
    mw_ctx_make_fresh_var_21_();
}

static void mw_ctx_make_fresh_type_var_21_ (void){
    mw_PRIM_TYPE_TYPE();
    mw_TPrim();
    mw_ctx_make_fresh_var_21_();
}

static void mw_ctx_make_fresh_var_21_ (void){
    {
        VAL d2 = pop_value();
        mw_ctx_fresh_name_21_();
        mw_var_new_21_();
        push_value(d2);
    }
    mw_over();
    mw_var_type();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_ctx_make_fresh_var_21__2);
    mw_prim_pack_cons();
    mw_sip();
}

static void mw_External_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_External_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_External_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_External_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_External_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_External_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_External_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_External_2E_alloc_21_ (void){
    mw_External_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_External_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_Variable_2E_MAX (void){
    push_i64(65536LL);
}

static void mw_Variable_2E_id (void){
    mw_prim_unsafe_cast();
}

static void mw_Variable_2E_succ (void){
    mw_prim_unsafe_cast();
    push_i64(1LL);
    mw_prim_int_add();
    mw_Variable_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_int_mod();
    mw_prim_unsafe_cast();
}

static void mw_Variable_2E_pred (void){
    mw_prim_unsafe_cast();
    mw_prim_dup();
    push_i64(0LL);
    mw_prim_value_eq();
    if (pop_u64()) {
    } else {
        push_i64(1LL);
        mw_prim_int_sub();
    }
    mw_prim_unsafe_cast();
}

static void mw_Variable_2E_for (void){
    {
        VAL var_x = pop_value();
        push_i64(1LL);
        while(1) {
            mw_prim_dup();
            mw_Variable_2E_NUM();
            mw_prim_int_get();
            mw_prim_value_le();
            if (!pop_u64()) break;
            mw_prim_dup();
            {
                VAL d4 = pop_value();
                mw_prim_unsafe_cast();
                push_value(var_x);
                incref(var_x);
                mw_prim_run();
                push_value(d4);
            }
            push_i64(1LL);
            mw_prim_int_add();
        }
        mw_prim_drop();
        decref(var_x);
    }
}

static void mw_Variable_2E_alloc_21_ (void){
    mw_Variable_2E_NUM();
    mw_prim_int_get();
    push_i64(1LL);
    mw_prim_int_add();
    mw_prim_dup();
    mw_Variable_2E_NUM();
    mw_prim_int_set();
    mw_prim_unsafe_cast();
}

static void mw_variable_new_21_ (void){
    mw_Variable_2E_alloc_21_();
    mw_tuck();
    mw_variable_type();
    mw__21_();
    mw_tuck();
    mw_dup2();
    mw_variable_name();
    mw__21_();
    mw_DEF_VARIABLE();
    mw_swap();
    mw_name_def();
    mw__21_();
}

static void mw_type_elab_default (void){
    mw_false();
    mw_ctx_empty();
    mw_TYPE_ELAB();
}

static void mw_type_elab_stack_assertion (void){
    {
        VAL d2 = pop_value();
        mw_true();
        push_value(d2);
    }
    mw_TYPE_ELAB();
}

static void mw_type_elab_holes_allowed (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_type_elab_ctx (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_nip();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_type_elab_ctx_3F_ (void){
    mw_dup();
    mw_type_elab_ctx();
}

static void mw_type_elab_ctx_replace (void){
    mw_swap();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_drop();
            mw_swap();
            mw_TYPE_ELAB();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_elab_type_sig_21_ (void){
    mw_token_run_end_3F_();
    if (pop_u64()) {
        mw_dup();
        push_ptr("expected type signature\0\0\0\0");
        mw_emit_error_21_();
    } else {
        mw_id();
    }
    mw_elab_type_sig_params_21_();
    {
        VAL d2 = pop_value();
        mw_swap();
        push_value(d2);
    }
    mw_elab_type_stack_21_();
    {
        VAL d2 = pop_value();
        mw_swap();
        push_value(d2);
    }
    mw_token_is_dashes_3F_();
    if (pop_u64()) {
        mw_token_next();
        mw_elab_type_stack_21_();
        {
            VAL d3 = pop_value();
            mw_swap();
            push_value(d3);
        }
    } else {
        {
            VAL d3 = pop_value();
            mw_T0();
            mw_rotr();
            push_value(d3);
        }
    }
    mw_token_run_end_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        mw_dup();
        push_ptr("expected right paren or comma\0\0\0\0");
        mw_emit_error_21_();
    }
    {
        VAL d2 = pop_value();
        mw_rot4r();
        {
            VAL d3 = pop_value();
            mw_swap();
            push_u64(0);
            push_fnptr(&mb_elab_type_sig_21__13);
            mw_prim_pack_cons();
            mw_for();
            push_value(d3);
        }
        mw_T__3E_();
        push_value(d2);
    }
}

static void mw_elab_type_sig_params_21_ (void){
    mw_token_is_lparen_3F_();
    if (pop_u64()) {
        mw_dup();
        mw_token_next();
        {
            VAL d3 = pop_value();
            mw_L0();
            mw_rotr();
            mw_token_args();
            push_u64(0);
            push_fnptr(&mb_elab_type_sig_params_21__3);
            mw_prim_pack_cons();
            mw_for();
            mw_swap();
            push_value(d3);
        }
    } else {
        mw_L0();
        mw_swap();
    }
}

static void mw_elab_type_stack_21_ (void){
    mw_sig_token_is_stack_var_3F_();
    if (pop_u64()) {
        mw_elab_stack_var_21_();
        {
            VAL d3 = pop_value();
            mw_TVar();
            push_value(d3);
        }
    } else {
        {
            VAL d3 = pop_value();
            mw_TYPE_UNIT();
            push_value(d3);
        }
    }
    mw_elab_type_stack_rest_21_();
}

static void mw_elab_type_stack_rest_21_ (void){
    while(1) {
        mw_sig_is_stack_end2_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_swap();
        {
            VAL d3 = pop_value();
            mw_elab_type_atom_21_();
            push_value(d3);
        }
        mw_swap();
        {
            VAL d3 = pop_value();
            mw_swap();
            mw_TTensor();
            push_value(d3);
        }
    }
}

static void mw_elab_type_arg_21_ (void){
    mw_elab_type_atom_21_();
    mw_token_is_arg_end_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("Unexpected token after type.\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mw_elab_type_atom_21_ (void){
    mw_sig_token_is_type_var_3F_();
    if (pop_u64()) {
        mw_elab_type_var_21_();
        {
            VAL d3 = pop_value();
            mw_TVar();
            push_value(d3);
        }
    } else {
        mw_sig_token_is_type_con_3F_();
        if (pop_u64()) {
            mw_elab_type_con_21_();
        } else {
            mw_token_is_underscore_3F_();
            if (pop_u64()) {
                mw_elab_type_dont_care_21_();
            } else {
                mw_sig_token_is_type_hole_3F_();
                if (pop_u64()) {
                    mw_elab_type_hole_21_();
                } else {
                    mw_token_is_lsquare_3F_();
                    if (pop_u64()) {
                        mw_elab_type_quote_21_();
                    } else {
                        mw_dup();
                        push_ptr("Expected type, got unknown token.\0\0\0\0");
                        mw_emit_error_21_();
                        {
                            VAL d7 = pop_value();
                            mw_TYPE_ERROR();
                            push_value(d7);
                        }
                        mw_token_next();
                    }
                }
            }
        }
    }
}

static void mw_elab_stack_var_21_ (void){
    mw_TYPE_STACK();
    mw_elab_implicit_var_21_();
}

static void mw_elab_type_var_21_ (void){
    mw_TYPE_TYPE();
    mw_elab_implicit_var_21_();
}

static void mw_elab_implicit_var_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_implicit_var_21__1);
    mw_prim_pack_cons();
    mw_dip2();
    mw_over();
    push_u64(0);
    push_fnptr(&mb_elab_implicit_var_21__2);
    mw_prim_pack_cons();
    mw_dip2();
    mw_rotl();
    switch (get_top_data_tag()) {
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_rotr();
            push_u64(0);
            push_fnptr(&mb_elab_implicit_var_21__4);
            mw_prim_pack_cons();
            mw_dip2();
            mw_elab_type_unify_21_();
            mw_nip();
            break;
        case 0LL:
            mw_prim_drop();
            {
                VAL d4 = pop_value();
                {
                    VAL d5 = pop_value();
                    mw_var_new_implicit_21_();
                    push_value(d5);
                }
                mw_over();
                mw_var_type();
                mw__21_();
                push_u64(0);
                push_fnptr(&mb_elab_implicit_var_21__8);
                mw_prim_pack_cons();
                mw_sip();
                push_value(d4);
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}    mw_token_next();
    push_u64(0);
    push_fnptr(&mb_elab_implicit_var_21__9);
    mw_prim_pack_cons();
    mw_dip2();
}

static void mw_elab_type_con_21_ (void){
    mw_token_name_3F_();
    mw_name_def();
    mw__40_();
    switch (get_top_data_tag()) {
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_over();
            mw_token_num_args();
            mw_over();
            mw_type_arity();
            mw__3D__3D_();
            if (pop_u64()) {
                mw_elab_type_args_21_();
            } else {
                mw_drop();
                mw_dup();
                push_ptr("Wrong number of arguments for type.\0\0\0\0");
                mw_emit_error_21_();
                mw_TYPE_ERROR();
            }
            break;
        case 0LL:
            mw_prim_drop();
            mw_dup();
            push_ptr("Unknown type.\0\0\0\0");
            mw_emit_error_21_();
            mw_TYPE_ERROR();
            break;
        default:
            mw_drop();
            mw_dup();
            push_ptr("Not a type.\0\0\0\0");
            mw_emit_error_21_();
            mw_TYPE_ERROR();
            break;
    
}    mw_swap();
    mw_token_next();
}

static void mw_elab_type_args_21_ (void){
    {
        VAL d2 = pop_value();
        mw_token_has_args_3F_();
        push_value(d2);
    }
    mw_swap();
    if (pop_u64()) {
        {
            VAL d3 = pop_value();
            mw_tuck();
            push_value(d3);
        }
        mw_swap();
        mw_token_succ();
        while(1) {
            mw_token_is_right_enclosure_3F_();
            mw_not();
            if (!pop_u64()) break;
            mw_token_succ();
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_elab_type_arg_21_();
                push_value(d4);
            }
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_swap();
                mw_TApp();
                push_value(d4);
            }
        }
        mw_drop();
        {
            VAL d3 = pop_value();
            mw_swap();
            push_value(d3);
        }
    } else {
        mw_id();
    }
}

static void mw_elab_type_hole_21_ (void){
    mw_over();
    mw_type_elab_holes_allowed();
    if (pop_u64()) {
        mw_token_has_args_3F_();
        if (pop_u64()) {
            mw_dup();
            push_ptr("Types with args not yet supported.\0\0\0\0");
            mw_emit_error_21_();
            mw_TYPE_ERROR();
        } else {
            mw_token_name_3F_();
            mw_THole();
        }
        mw_swap();
        mw_token_next();
    } else {
        push_ptr("type holes are not allowed here\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mw_elab_type_dont_care_21_ (void){
    mw_over();
    mw_type_elab_holes_allowed();
    if (pop_u64()) {
        mw_token_has_args_3F_();
        if (pop_u64()) {
            mw_dup();
            push_ptr("Types with args not yet supported.\0\0\0\0");
            mw_emit_error_21_();
            mw_TYPE_ERROR();
        } else {
            mw_TYPE_DONT_CARE();
        }
        mw_swap();
        mw_token_next();
    } else {
        push_ptr("type don't care is not allowed here\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mw_elab_type_quote_21_ (void){
    mw_token_args_1();
    mw_sig_has_dashes_3F_();
    if (pop_u64()) {
        mw_elab_type_sig_21_();
    } else {
        mw_elab_type_stack_21_();
    }
    mw_token_next();
}

static void mw_elab_type_unify_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_type_unify_21__1);
    mw_prim_pack_cons();
    mw_sip();
}

static void mw_elab_simple_type_arg_21_ (void){
    {
        VAL d2 = pop_value();
        mw_type_elab_default();
        push_value(d2);
    }
    mw_elab_type_arg_21_();
    mw_drop();
    mw_nip();
}

static void mw_ab_ctx (void){
    mw_ab_arrow();
    mw__40_();
    mw_arrow_ctx();
}

static void mw_ab_token (void){
    mw_ab_arrow();
    mw__40_();
    mw_arrow_token_end();
}

static void mw_ab_type (void){
    mw_ab_arrow();
    mw__40_();
    mw_arrow_cod();
}

static void mw_ab_save_21_ (void){
    {
        VAL var_f = pop_value();
        mw_ab_arrow();
        mw__40_();
        {
            VAL d3 = pop_value();
            push_value(var_f);
            incref(var_f);
            mw_prim_run();
            push_value(d3);
        }
        mw_ab_arrow();
        mw__21_();
        decref(var_f);
    }
}

static void mw_ab_build_21_ (void){
    {
        VAL var_f = pop_value();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_ab_build_21__2);
        mw_prim_pack_cons();
        mw_ab_save_21_();
        decref(var_f);
    }
}

static void mw_ab_build_hom_21_ (void){
    {
        VAL var_f = pop_value();
        mw_elab_expand_morphism_21_();
        {
            VAL d3 = pop_value();
            mw_rotr();
            push_value(d3);
        }
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_ab_build_hom_21__3);
        mw_prim_pack_cons();
        mw_ab_build_21_();
        decref(var_f);
    }
}

static void mw_ab_build_word_arrow_21_ (void){
    {
        VAL var_f = pop_value();
        mw_dup();
        mw_ab_home();
        mw__21_();
        push_i64(0LL);
        mw_ab_homeidx();
        mw__21_();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_ab_build_word_arrow_21__2);
        mw_prim_pack_cons();
        mw_sip();
        mw_word_body();
        mw__40_();
        push_value(var_f);
        incref(var_f);
        mw_ab_build_hom_21_();
        mw_nil();
        mw_ab_home();
        mw__21_();
        decref(var_f);
    }
}

static void mw_ab_build_word_21_ (void){
    {
        VAL var_f = pop_value();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_ab_build_word_21__2);
        mw_prim_pack_cons();
        mw_sip();
        mw_tuck();
        mw_word_arrow();
        mw__21_();
        decref(var_f);
    }
}

static void mw_ab_unify_type_21_ (void){
    {
        VAL d2 = pop_value();
        mw_ab_token();
        mw__40_();
        mw_GAMMA();
        mw_ab_type();
        mw__40_();
        push_value(d2);
    }
    mw_type_unify_21_();
    mw_ab_type();
    mw__21_();
    mw_drop();
}

static void mw_ab_atom_21_ (void){
    mw_dup();
    mw_atom_token();
    mw__40_();
    mw_ab_token();
    mw__21_();
    mw_dup();
    mw_atom_cod();
    mw__40_();
    mw_ab_type();
    mw__21_();
    {
        VAL d2 = pop_value();
        mw_ab_arrow();
        mw__40_();
        mw_arrow_atoms();
        mw__40_();
        push_value(d2);
    }
    mw_ab_optimized_snoc_21_();
    mw_ab_arrow();
    mw__40_();
    mw_arrow_atoms();
    mw__21_();
}

static void mw_ab_optimized_snoc_21_ (void){
    while(1) {
        push_u64(0);
        push_fnptr(&mb_ab_optimized_snoc_21__2);
        mw_prim_pack_cons();
        mw_dip_3F_();
        push_u64(0);
        push_fnptr(&mb_ab_optimized_snoc_21__3);
        mw_prim_pack_cons();
        mw_and();
        if (!pop_u64()) break;
        mw_swap();
        mw_atoms_turn_last_block_to_arg();
        mw_swap();
    }
    mw_snoc();
}

static void mw_atom_accepts_args_3F_ (void){
    mw_dup();
    mw_atom_op();
    mw__40_();
    switch (get_top_data_tag()) {
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            {
                VAL d4 = pop_value();
                mw_dup();
                mw_atom_args();
                mw__40_();
                mw_len();
                push_value(d4);
            }
            mw_elab_word_sig_21_();
            mw_type_max_num_params();
            mw__3C_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            switch (get_top_data_tag()) {
                case 8LL:
                    mw_prim_drop();
                    mw_dup();
                    mw_atom_args();
                    mw__40_();
                    mw_len();
                    push_i64(1LL);
                    mw__3C_();
                    break;
                case 4LL:
                    mw_prim_drop();
                    mw_dup();
                    mw_atom_args();
                    mw__40_();
                    mw_len();
                    push_i64(1LL);
                    mw__3C_();
                    break;
                case 5LL:
                    mw_prim_drop();
                    mw_dup();
                    mw_atom_args();
                    mw__40_();
                    mw_len();
                    push_i64(2LL);
                    mw__3C_();
                    break;
                case 6LL:
                    mw_prim_drop();
                    mw_dup();
                    mw_atom_args();
                    mw__40_();
                    mw_len();
                    push_i64(2LL);
                    mw__3C_();
                    break;
                default:
                    mw_drop();
                    mw_false();
                    break;
            
}            break;
        default:
            mw_drop();
            mw_false();
            break;
    
}}

static void mw_atoms_has_last_block_3F_ (void){
    mw_dup();
    mw_last();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_false();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_atom_op();
            mw__40_();
            switch (get_top_data_tag()) {
                case 14LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_drop();
                    mw_true();
                    break;
                default:
                    mw_drop();
                    mw_false();
                    break;
            
}            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_atoms_turn_last_block_to_arg (void){
    mw_List__3E_List_2B_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_L0();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_unsnoc();
            mw_dup();
            mw_atom_op();
            mw__40_();
            switch (get_top_data_tag()) {
                case 14LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_atom_cod();
                        mw__40_();
                        mw_rotl();
                        mw_tuck();
                        mw_atom_dom();
                        mw__21_();
                        push_value(d6);
                    }
                    mw_block_to_arg();
                    mw_atom_arg_add_left_21_();
                    mw_swap();
                    break;
                default:
                    mw_drop();
                    mw_snoc();
                    break;
            
}            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_block_to_arg (void){
    mw_dup();
    mw_block_arrow();
    mw_force_21_();
    mw_arrow_to_run_var();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_ARG_BLOCK();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_ARG_VAR_RUN();
            mw_nip();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_arrow_to_run_var (void){
    mw_arrow_atoms();
    mw__40_();
    switch (get_top_data_tag()) {
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_atom_to_run_var();
            break;
        default:
            mw_drop();
            mw_NONE();
            break;
    
}}

static void mw_atom_to_run_var (void){
    mw_atom_op();
    mw__40_();
    switch (get_top_data_tag()) {
        case 13LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_dup();
            mw_var_auto_run();
            mw__40_();
            if (pop_u64()) {
                mw_SOME();
            } else {
                mw_drop();
                mw_NONE();
            }
            break;
        default:
            mw_drop();
            mw_NONE();
            break;
    
}}

static void mw_ab_op_21_ (void){
    mw_Atom_2E_alloc_21_();
    mw_ab_ctx();
    mw__40_();
    mw_over();
    mw_atom_ctx();
    mw__21_();
    mw_ab_token();
    mw__40_();
    mw_over();
    mw_atom_token();
    mw__21_();
    mw_dup2();
    mw_atom_op();
    mw__21_();
    mw_swap();
    mw_elab_op_fresh_sig_21_();
    {
        VAL d2 = pop_value();
        mw_over();
        mw_atom_subst();
        mw__21_();
        push_value(d2);
    }
    mw_ab_expand_opsig_21_();
    {
        VAL d2 = pop_value();
        mw_over();
        mw_atom_dom();
        mw__21_();
        push_value(d2);
    }
    mw_over();
    mw_atom_cod();
    mw__21_();
    mw_ab_atom_21_();
}

static void mw_ab_expand_opsig_21_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_ab_type();
            mw__40_();
            mw_dup();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            {
                VAL d4 = pop_value();
                mw_ab_type();
                mw__40_();
                mw_dup();
                push_value(d4);
            }
            mw_TTensor();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            {
                VAL d4 = pop_value();
                mw_ab_type();
                mw__40_();
                push_value(d4);
            }
            mw_ab_token();
            mw__40_();
            mw_elab_expand_morphism_21_();
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_elab_type_unify_21_();
                mw_drop();
                push_value(d4);
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_ab_int_21_ (void){
    mw_OP_INT();
    mw_ab_op_21_();
}

static void mw_ab_str_21_ (void){
    mw_OP_STR();
    mw_ab_op_21_();
}

static void mw_ab_buffer_21_ (void){
    mw_OP_BUFFER();
    mw_ab_op_21_();
}

static void mw_ab_variable_21_ (void){
    mw_OP_VARIABLE();
    mw_ab_op_21_();
}

static void mw_ab_constant_21_ (void){
    mw_OP_CONSTANT();
    mw_ab_op_21_();
}

static void mw_ab_field_21_ (void){
    mw_OP_FIELD();
    mw_ab_op_21_();
}

static void mw_ab_var_21_ (void){
    mw_OP_VAR();
    mw_ab_op_21_();
}

static void mw_ab_tag_21_ (void){
    mw_OP_TAG();
    mw_ab_op_21_();
}

static void mw_ab_prim_21_ (void){
    mw_dup();
    mw_prim_type();
    mw__40_();
    mw_is_nil();
    if (pop_u64()) {
        mw_ab_token();
        mw__40_();
        push_ptr("compiler error: prim type missing\0\0\0\0");
        mw_emit_fatal_error_21_();
    } else {
        mw_OP_PRIM();
        mw_ab_op_21_();
    }
}

static void mw_ab_word_21_ (void){
    mw_OP_WORD();
    mw_ab_op_21_();
}

static void mw_ab_external_21_ (void){
    mw_OP_EXTERNAL();
    mw_ab_op_21_();
}

static void mw_ab_block_at_21_ (void){
    {
        VAL var_f = pop_value();
        mw_ab_ctx();
        mw__40_();
        mw_meta_alloc_21_();
        mw_TMeta();
        mw_rotl();
        push_value(var_f);
        incref(var_f);
        mw_ab_build_21_();
        mw_block_new_21_();
        mw_OP_BLOCK();
        mw_ab_op_21_();
        decref(var_f);
    }
}

static void mw_ab_block_21_ (void){
    {
        VAL var_f = pop_value();
        mw_ab_token();
        mw__40_();
        push_value(var_f);
        incref(var_f);
        mw_ab_block_at_21_();
        decref(var_f);
    }
}

static void mw_ab_dip_21_ (void){
    {
        VAL var_f = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_ab_block_21_();
        mw_PRIM_CORE_DIP();
        mw_ab_prim_21_();
        decref(var_f);
    }
}

static void mw_ab_if_21_ (void){
    {
        VAL var_g = pop_value();
        VAL var_f = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_ab_block_21_();
        push_value(var_g);
        incref(var_g);
        mw_ab_block_21_();
        mw_PRIM_CORE_IF();
        mw_ab_prim_21_();
        decref(var_g);
        decref(var_f);
    }
}

static void mw_ab_while_21_ (void){
    {
        VAL var_g = pop_value();
        VAL var_f = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_ab_block_21_();
        push_value(var_g);
        incref(var_g);
        mw_ab_block_21_();
        mw_PRIM_CORE_WHILE();
        mw_ab_prim_21_();
        decref(var_g);
        decref(var_f);
    }
}

static void mw_ab_lambda_21_ (void){
    {
        VAL var_f = pop_value();
        mw_Lambda_2E_alloc_21_();
        mw_ab_ctx();
        mw__40_();
        mw_over();
        mw_lambda_outer_ctx();
        mw__21_();
        mw_ab_type();
        mw__40_();
        mw_over();
        mw_lambda_dom();
        mw__21_();
        mw_ab_token();
        mw__40_();
        mw_over();
        mw_lambda_token();
        mw__21_();
        mw_dup2();
        mw_lambda_params();
        mw__21_();
        {
            VAL d3 = pop_value();
            mw_ab_ctx();
            mw__40_();
            mw_ab_type();
            mw__40_();
            mw_rotl();
            push_u64(0);
            push_value(var_f);
            incref(var_f);
            mw_prim_pack_cons();
            push_fnptr(&mb_ab_lambda_21__3);
            mw_prim_pack_cons();
            mw_reverse_for();
            push_value(d3);
        }
        mw_tuck();
        mw_lambda_mid();
        mw__21_();
        mw_tuck();
        mw_lambda_inner_ctx();
        mw__21_();
        mw_dup();
        mw_lambda_inner_ctx();
        mw__40_();
        mw_over();
        mw_lambda_mid();
        mw__40_();
        mw_ab_token();
        mw__40_();
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_ab_lambda_21__7);
        mw_prim_pack_cons();
        mw_ab_build_21_();
        mw_over();
        mw_lambda_body();
        mw__21_();
        mw_OP_LAMBDA();
        mw_ab_op_21_();
        decref(var_f);
    }
}

static void mw_elab_op_fresh_sig_21_ (void){
    mw_subst_nil();
    mw_swap();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_OPSIG_ID();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_VALUE_INT();
            mw_TValue();
            mw_OPSIG_PUSH();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_VALUE_STR();
            mw_TValue();
            mw_OPSIG_PUSH();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_TYPE_PTR();
            mw_OPSIG_PUSH();
            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_variable_type();
            mw_force_21_();
            mw_TMut();
            mw_OPSIG_PUSH();
            break;
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_TValue();
            mw_OPSIG_PUSH();
            break;
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_tag_sig_21_();
            mw_type_freshen_sig();
            mw_OPSIG_APPLY();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_word_sig_21_();
            mw_type_freshen_sig();
            mw_OPSIG_APPLY();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_type();
            mw__40_();
            mw_type_freshen_sig();
            mw_OPSIG_APPLY();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_external_sig_21_();
            mw_type_freshen_sig();
            mw_OPSIG_APPLY();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_field_type_21_();
            mw_type_freshen_sig();
            mw_OPSIG_APPLY();
            break;
        case 14LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_block_sig_21_();
            break;
        case 13LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_var_sig_21_();
            break;
        case 11LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_match_sig_21_();
            break;
        case 12LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_lambda_sig_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_elab_block_sig_21_ (void){
    mw_VALUE_BLOCK();
    mw_TValue();
    mw_OPSIG_PUSH();
}

static void mw_elab_match_sig_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_match_sig_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_match_cod();
    mw__40_();
    mw_T__3E_();
    mw_OPSIG_APPLY();
}

static void mw_elab_lambda_sig_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_lambda_sig_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_lambda_cod();
    mw__40_();
    mw_T__3E_();
    mw_OPSIG_APPLY();
}

static void mw_elab_var_sig_21_ (void){
    mw_dup();
    mw_var_auto_run();
    mw__40_();
    if (pop_u64()) {
        mw_var_type();
        mw__40_();
        mw_type_semifreshen_sig();
        mw_OPSIG_APPLY();
    } else {
        mw_var_type();
        mw__40_();
        mw_OPSIG_PUSH();
    }
}

static void mw_elab_tag_sig_21_ (void){
    mw_elab_tag_ctx_sig_21_();
    mw_nip();
}

static void mw_elab_tag_ctx_21_ (void){
    mw_elab_tag_ctx_sig_21_();
    mw_drop();
}

static void mw_elab_tag_ctx_sig_21_ (void){
    mw_tag_ctx_type();
    mw_force2_21_();
}

static void mw_elab_external_sig_21_ (void){
    mw_elab_external_ctx_sig_21_();
    mw_nip();
}

static void mw_elab_external_ctx_21_ (void){
    mw_elab_external_ctx_sig_21_();
    mw_drop();
}

static void mw_elab_external_ctx_sig_21_ (void){
    mw_external_ctx_type();
    mw_force2_21_();
}

static void mw_elab_word_sig_21_ (void){
    mw_elab_word_ctx_sig_21_();
    mw_nip();
}

static void mw_elab_word_ctx_21_ (void){
    mw_elab_word_ctx_sig_21_();
    mw_drop();
}

static void mw_elab_word_ctx_sig_21_ (void){
    mw_dup();
    mw_word_ctx_type();
    push_u64(0);
    push_fnptr(&mb_elab_word_ctx_sig_21__1);
    mw_prim_pack_cons();
    mw_force_or_21_();
    mw_nip();
    mw_unpack2();
}

static void mw_emit_recursive_word_fatal_error_21_ (void){
    mw_word_head();
    mw__40_();
    push_ptr("recursive word needs type signature\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mw_elab_word_ctx_sig_weak_21_ (void){
    mw_dup();
    mw_word_sig();
    mw__40_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_word_ctx_type();
            mw__40_();
            switch (get_top_data_tag()) {
                case 0LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_unpack2();
                    break;
                default:
                    mw_drop();
                    mw_ctx_empty();
                    mw_meta_alloc_21_();
                    mw_TMeta();
                    mw_meta_alloc_21_();
                    mw_TMeta();
                    mw_T__3E_();
                    break;
            
}            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_elab_word_ctx_sig_21_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_elab_word_body_21_ (void){
    mw_word_arrow();
    mw_force_21_();
}

static void mw_elab_arrow_21_ (void){
    mw_elab_expand_morphism_21_();
    mw_elab_arrow_hom_21_();
}

static void mw_elab_arrow_hom_21_ (void){
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_elab_arrow_fwd_21_();
        mw_dup();
        mw_arrow_token_end();
        mw__40_();
        mw_GAMMA();
        mw_over();
        mw_arrow_cod();
        mw__40_();
        push_value(d2);
    }
    mw_type_unify_21_();
    mw_drop2();
}

static void mw_elab_arrow_fwd_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_arrow_fwd_21__1);
    mw_prim_pack_cons();
    mw_ab_build_21_();
}

static void mw_elab_atoms_21_ (void){
    while(1) {
        mw_elab_atoms_done_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_elab_atom_21_();
        mw_ab_token();
        push_u64(0);
        push_fnptr(&mb_elab_atoms_21__3);
        mw_prim_pack_cons();
        mw_modify();
    }
}

static void mw_elab_atoms_done_3F_ (void){
    mw_ab_token();
    mw__40_();
    mw_token_run_end_3F_();
    mw_nip();
}

static void mw_elab_atom_21_ (void){
    mw_ab_token();
    mw__40_();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_atom_name_21_();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_ab_int_21_();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_ab_str_21_();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_elab_atom_block_21_();
            break;
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            mw_elab_atom_assert_21_();
            break;
        default:
            mw_ab_token();
            mw__40_();
            push_ptr("Unexpected token in elab-atom!\0\0\0\0");
            mw_emit_fatal_error_21_();
            break;
    
}}

static void mw_elab_atom_block_21_ (void){
    mw_ab_token();
    mw__40_();
    mw_token_args_1();
    mw_elab_block_at_21_();
}

static void mw_elab_block_at_21_ (void){
    mw_ab_ctx();
    mw__40_();
    mw_swap();
    mw_block_new_deferred_21_();
    mw_OP_BLOCK();
    mw_ab_op_21_();
}

static void mw_elab_args_21_ (void){
    mw_ab_token();
    mw__40_();
    mw_token_args();
    push_u64(0);
    push_fnptr(&mb_elab_args_21__1);
    mw_prim_pack_cons();
    mw_for();
}

static void mw_elab_no_args_21_ (void){
    mw_ab_token();
    mw__40_();
    mw_token_args_0();
}

static void mw_elab_atom_name_21_ (void){
    mw_dup();
    mw_ab_ctx();
    mw__40_();
    mw_ctx_lookup();
    switch (get_top_data_tag()) {
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_nip();
            mw_elab_args_21_();
            mw_ab_var_21_();
            break;
        case 0LL:
            mw_prim_drop();
            mw_name_def();
            mw__40_();
            switch (get_top_data_tag()) {
                case 6LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_elab_no_args_21_();
                    mw_ab_buffer_21_();
                    break;
                case 7LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_elab_no_args_21_();
                    mw_ab_variable_21_();
                    break;
                case 8LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_elab_no_args_21_();
                    mw_ab_constant_21_();
                    break;
                case 9LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_elab_no_args_21_();
                    mw_ab_external_21_();
                    break;
                case 10LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_elab_no_args_21_();
                    mw_ab_field_21_();
                    break;
                case 5LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_elab_args_21_();
                    mw_ab_word_21_();
                    break;
                case 3LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_elab_args_21_();
                    mw_ab_tag_21_();
                    break;
                case 4LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_elab_prim_21_();
                    break;
                default:
                    mw_drop();
                    mw_ab_token();
                    mw__40_();
                    push_ptr("Unknown word.\0\0\0\0");
                    mw_emit_error_21_();
                    mw_TYPE_ERROR();
                    mw_ab_type();
                    mw__21_();
                    break;
            
}            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_elab_prim_21_ (void){
    switch (get_top_data_tag()) {
        case 9LL:
            mw_prim_drop();
            mw_elab_atom_match_21_();
            break;
        case 10LL:
            mw_prim_drop();
            mw_elab_atom_lambda_21_();
            break;
        default:
            mw_elab_args_21_();
            mw_ab_prim_21_();
            break;
    
}}

static void mw_elab_atom_assert_21_ (void){
    mw_ab_token();
    mw__40_();
    mw_GAMMA();
    mw_ab_ctx();
    mw__40_();
    mw_type_elab_stack_assertion();
    mw_ab_token();
    mw__40_();
    mw_token_args_1();
    mw_elab_type_stack_21_();
    mw_drop();
    mw_nip();
    mw_ab_type();
    mw__40_();
    mw_swap();
    mw_type_unify_21_();
    mw_drop2();
}

static void mw_elab_atom_lambda_21_ (void){
    mw_Lambda_2E_alloc_21_();
    mw_ab_ctx();
    mw__40_();
    mw_over();
    mw_lambda_outer_ctx();
    mw__21_();
    mw_ab_type();
    mw__40_();
    mw_over();
    mw_lambda_dom();
    mw__21_();
    mw_ab_token();
    mw__40_();
    mw_over();
    mw_lambda_token();
    mw__21_();
    mw_elab_lambda_21_();
    mw_OP_LAMBDA();
    mw_ab_op_21_();
}

static void mw_elab_match_at_21_ (void){
    mw_Match_2E_alloc_21_();
    mw_ab_ctx();
    mw__40_();
    mw_over();
    mw_match_ctx();
    mw__21_();
    mw_ab_type();
    mw__40_();
    mw_over();
    mw_match_dom();
    mw__21_();
    mw_ab_token();
    mw__40_();
    mw_over();
    mw_match_token();
    mw__21_();
    mw_tuck();
    mw_match_body();
    mw__21_();
    mw_tuck();
    mw_match_cod();
    mw__21_();
    mw_elab_match_cases_21_();
    mw_elab_match_exhaustive_21_();
    mw_OP_MATCH();
    mw_ab_op_21_();
}

static void mw_elab_atom_match_21_ (void){
    mw_meta_alloc_21_();
    mw_TMeta();
    mw_ab_token();
    mw__40_();
    mw_token_args_2B_();
    mw_first_2B_();
    mw_elab_match_at_21_();
}

static void mw_elab_lambda_21_ (void){
    mw_elab_lambda_params_21_();
    mw_elab_lambda_body_21_();
}

static void mw_elab_expand_tensor_21_ (void){
    mw_swap();
    mw_type_expand();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            {
                VAL d4 = pop_value();
                mw_TYPE_ERROR();
                mw_TYPE_ERROR();
                push_value(d4);
            }
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_id();
            mw_rotl();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            {
                VAL d4 = pop_value();
                mw_meta_alloc_21_();
                mw_TMeta();
                mw_meta_alloc_21_();
                mw_TMeta();
                mw_dup2();
                mw_T_2A_();
                mw_SOME();
                push_value(d4);
            }
            mw_meta_type();
            mw__21_();
            mw_rotl();
            break;
        default:
            mw_drop();
            mw_dup();
            push_ptr("expected tuple type\0\0\0\0");
            mw_emit_error_21_();
            {
                VAL d4 = pop_value();
                mw_TYPE_ERROR();
                mw_TYPE_ERROR();
                push_value(d4);
            }
            break;
    
}}

static void mw_elab_expand_morphism_21_ (void){
    mw_swap();
    mw_type_expand();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            {
                VAL d4 = pop_value();
                mw_TYPE_ERROR();
                mw_TYPE_ERROR();
                push_value(d4);
            }
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_prim_pack_uncons(); mw_prim_swap();
            mw_id();
            mw_rotl();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            {
                VAL d4 = pop_value();
                mw_meta_alloc_21_();
                mw_TMeta();
                mw_meta_alloc_21_();
                mw_TMeta();
                mw_dup2();
                mw_T__3E_();
                mw_SOME();
                push_value(d4);
            }
            mw_meta_type();
            mw__21_();
            mw_rotl();
            break;
        default:
            mw_drop();
            mw_dup();
            push_ptr("expected block type\0\0\0\0");
            mw_emit_error_21_();
            {
                VAL d4 = pop_value();
                mw_TYPE_ERROR();
                mw_TYPE_ERROR();
                push_value(d4);
            }
            break;
    
}}

static void mw_elab_lambda_pop_from_mid_21_ (void){
    {
        VAL d2 = pop_value();
        mw_dup();
        mw_lambda_mid();
        mw__40_();
        push_value(d2);
    }
    mw_elab_expand_tensor_21_();
    push_u64(0);
    push_fnptr(&mb_elab_lambda_pop_from_mid_21__2);
    mw_prim_pack_cons();
    mw_dip2();
}

static void mw_token_is_lambda_param_3F_ (void){
    mw_sig_token_is_type_var_3F_();
    if (pop_u64()) {
        mw_token_has_args_3F_();
        mw_not();
    } else {
        mw_token_is_lsquare_3F_();
        if (pop_u64()) {
            mw_dup();
            mw_true();
            {
                VAL d4 = pop_value();
                mw_token_succ();
                mw_sig_token_is_type_var_3F_();
                push_value(d4);
            }
            mw__26__26_();
            {
                VAL d4 = pop_value();
                mw_token_succ();
                mw_token_is_rsquare_3F_();
                push_value(d4);
            }
            mw__26__26_();
            mw_nip();
        } else {
            mw_false();
        }
    }
}

static void mw_elab_lambda_params_21_ (void){
    mw_dup();
    mw_lambda_outer_ctx();
    mw__40_();
    mw_over();
    mw_lambda_inner_ctx();
    mw__21_();
    mw_dup();
    mw_lambda_dom();
    mw__40_();
    mw_over();
    mw_lambda_mid();
    mw__21_();
    mw_dup();
    mw_lambda_token();
    mw__40_();
    mw_token_args_1();
    while(1) {
        mw_token_is_lambda_param_3F_();
        if (!pop_u64()) break;
        mw_token_next();
    }
    mw_expect_token_arrow();
    mw_token_prev();
    while(1) {
        mw_token_is_lambda_param_3F_();
        if (!pop_u64()) break;
        mw_elab_lambda_pop_from_mid_21_();
        push_u64(0);
        push_fnptr(&mb_elab_lambda_params_21__5);
        mw_prim_pack_cons();
        mw_sip();
        mw_token_prev();
    }
    mw_drop();
}

static void mw_elab_lambda_body_21_ (void){
    mw_dup();
    mw_lambda_token();
    mw__40_();
    mw_token_args_1();
    while(1) {
        mw_token_is_lambda_param_3F_();
        if (!pop_u64()) break;
        mw_token_next();
    }
    mw_token_succ();
    {
        VAL d2 = pop_value();
        mw_dup();
        mw_lambda_mid();
        mw__40_();
        {
            VAL d3 = pop_value();
            mw_dup();
            mw_lambda_inner_ctx();
            mw__40_();
            push_value(d3);
        }
        push_value(d2);
    }
    mw_elab_arrow_fwd_21_();
    mw_dup2();
    mw_swap();
    mw_lambda_body();
    mw__21_();
    mw_arrow_cod();
    mw__40_();
    mw_over();
    mw_lambda_cod();
    mw__21_();
}

static void mw_elab_match_exhaustive_21_ (void){
    mw_match_is_exhaustive_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        mw_dup();
        mw_match_token();
        mw__40_();
        push_ptr("Pattern match not exhaustive.\0\0\0\0");
        mw_emit_error_21_();
    }
}

static void mw_elab_match_cases_21_ (void){
    mw_dup();
    mw_match_body();
    mw__40_();
    while(1) {
        mw_token_is_rparen_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_elab_match_case_21_();
    }
    mw_drop();
}

static void mw_elab_match_case_21_ (void){
    mw_Case_2E_alloc_21_();
    mw_dup2();
    mw_case_token();
    mw__21_();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_dup2();
        mw_case_match();
        mw__21_();
        push_value(d2);
    }
    mw_elab_case_pattern_21_();
    mw_expect_token_arrow();
    mw_token_succ();
    mw_elab_case_body_21_();
    {
        VAL d2 = pop_value();
        mw_match_add_case_21_();
        push_value(d2);
    }
    mw_token_is_comma_3F_();
    if (pop_u64()) {
        mw_token_succ();
    } else {
        mw_id();
    }
}

static void mw_elab_case_pattern_21_ (void){
    mw_token_is_underscore_3F_();
    if (pop_u64()) {
        {
            VAL d3 = pop_value();
            mw_PATTERN_UNDERSCORE();
            mw_over();
            mw_case_pattern();
            mw__21_();
            push_value(d3);
        }
        {
            VAL d3 = pop_value();
            mw_dup();
            mw_case_match();
            mw__40_();
            mw_match_dom();
            mw__40_();
            mw_TYPE_DONT_CARE();
            mw_TYPE_DONT_CARE();
            mw_T_2A_();
            push_value(d3);
        }
        mw_elab_type_unify_21_();
        {
            VAL d3 = pop_value();
            mw_over();
            mw_case_mid();
            mw__21_();
            push_value(d3);
        }
        mw_token_succ();
    } else {
        mw_token_is_name_3F_();
        if (pop_u64()) {
            mw_token_name_3F_();
            mw_name_def();
            mw__40_();
            switch (get_top_data_tag()) {
                case 3LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_dup();
                    mw_PATTERN_TAG();
                    mw_rotr();
                    push_u64(0);
                    push_fnptr(&mb_elab_case_pattern_21__8);
                    mw_prim_pack_cons();
                    mw_dip2();
                    push_u64(0);
                    push_fnptr(&mb_elab_case_pattern_21__9);
                    mw_prim_pack_cons();
                    mw_dip2();
                    mw_elab_tag_sig_21_();
                    mw_subst_nil();
                    mw_swap();
                    mw_type_freshen_sig();
                    mw_rotr();
                    {
                        VAL d6 = pop_value();
                        mw_elab_expand_morphism_21_();
                        push_u64(0);
                        push_fnptr(&mb_elab_case_pattern_21__11);
                        mw_prim_pack_cons();
                        mw_dip2();
                        mw_elab_type_unify_21_();
                        mw_nip();
                        {
                            VAL d7 = pop_value();
                            mw_over();
                            mw_case_mid();
                            mw__21_();
                            push_value(d7);
                        }
                        push_value(d6);
                    }
                    mw_swap();
                    {
                        VAL d6 = pop_value();
                        mw_over();
                        mw_case_subst();
                        mw__21_();
                        push_value(d6);
                    }
                    mw_token_succ();
                    break;
                case 0LL:
                    mw_prim_drop();
                    push_ptr("Unknown constructor.\0\0\0\0");
                    mw_emit_fatal_error_21_();
                    break;
                default:
                    mw_drop();
                    push_ptr("Not a constructor.\0\0\0\0");
                    mw_emit_fatal_error_21_();
                    break;
            
}        } else {
            push_ptr("Expected constructor name.\0\0\0\0");
            mw_emit_fatal_error_21_();
        }
    }
}

static void mw_elab_case_body_21_ (void){
    {
        VAL d2 = pop_value();
        mw_dup();
        mw_case_mid();
        mw__40_();
        {
            VAL d3 = pop_value();
            mw_dup();
            mw_case_match();
            mw__40_();
            mw_match_ctx();
            mw__40_();
            push_value(d3);
        }
        push_value(d2);
    }
    mw_elab_arrow_fwd_21_();
    mw_dup();
    mw_arrow_token_end();
    mw__40_();
    {
        VAL d2 = pop_value();
        mw_dup();
        mw_arrow_cod();
        mw__40_();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_elab_case_body_21__4);
    mw_prim_pack_cons();
    mw_dip2();
    push_u64(0);
    push_fnptr(&mb_elab_case_body_21__5);
    mw_prim_pack_cons();
    mw_dip2();
    mw_elab_type_unify_21_();
    mw_nip();
}

static void mw_elab_module_21_ (void){
    mw_dup();
    mw_module_start();
    mw__40_();
    mw_elab_module_header_21_();
    while(1) {
        mw_token_is_module_end_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_elab_module_decl_21_();
    }
    mw_drop();
}

static void mw_elab_module_header_21_ (void){
    mw_PRIM_SYNTAX_MODULE();
    mw_token_prim_3D__3F_();
    if (pop_u64()) {
        mw_dup();
        mw_token_args_1();
        mw_token_is_name_3F_();
        if (pop_u64()) {
            mw_id();
        } else {
            push_ptr("Expected module name.\0\0\0\0");
            mw_emit_fatal_error_21_();
        }
        mw_token_name_3F_();
        mw_name_defined_3F_();
        if (pop_u64()) {
            mw_drop();
            push_ptr("Module name already taken.\0\0\0\0");
            mw_emit_fatal_error_21_();
        } else {
            mw_id();
        }
        mw_over();
        mw_token_module();
        mw__40_();
        mw_dup2();
        mw_module_name();
        mw__21_();
        mw_dup2();
        mw_DEF_MODULE();
        mw_swap();
        mw_name_def();
        mw__21_();
        mw_module_path();
        mw__40_();
        mw_Path__3E_Str();
        mw_swap();
        mw_module_path_from_name();
        mw_Path__3E_Str();
        mw_str_eq();
        if (pop_u64()) {
            mw_drop();
        } else {
            push_ptr("Module name should match path.\0\0\0\0");
            mw_emit_error_21_();
        }
        mw_token_next();
    } else {
        mw_dup();
        push_ptr("Expected module header.\0\0\0\0");
        mw_emit_error_21_();
    }
}

static void mw_elab_module_decl_21_ (void){
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_name_def();
            mw__40_();
            switch (get_top_data_tag()) {
                case 4LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    mw_prim_decl();
                    mw__40_();
                    mw_is_nil_3F_();
                    if (pop_u64()) {
                        mw_drop();
                        push_ptr("unknown declaration\0\0\0\0");
                        mw_emit_fatal_error_21_();
                    } else {
                        mw_run();
                    }
                    break;
                default:
                    mw_drop();
                    push_ptr("unknown declaration\0\0\0\0");
                    mw_emit_fatal_error_21_();
                    break;
            
}            break;
        default:
            mw_drop();
            push_ptr("unsupported declaration\0\0\0\0");
            mw_emit_fatal_error_21_();
            break;
    
}}

static void mw_elab_module_import_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_module_import_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_token_args_1();
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_dup();
            mw_name_def();
            mw__40_();
            switch (get_top_data_tag()) {
                case 1LL:
                    mw_prim_pack_uncons(); mw_prim_drop();
                    {
                        VAL d6 = pop_value();
                        mw_drop2();
                        mw_dup();
                        mw_token_module();
                        mw__40_();
                        push_value(d6);
                    }
                    mw_module_add_import_21_();
                    break;
                case 0LL:
                    mw_prim_drop();
                    mw_module_path_from_name();
                    mw_run_lexer_21_();
                    mw_elab_module_21_();
                    {
                        VAL d6 = pop_value();
                        mw_drop();
                        mw_dup();
                        mw_token_module();
                        mw__40_();
                        push_value(d6);
                    }
                    mw_module_add_import_21_();
                    break;
                default:
                    mw_drop2();
                    push_ptr("module name already taken\0\0\0\0");
                    mw_emit_fatal_error_21_();
                    break;
            
}            break;
        default:
            mw_drop();
            push_ptr("expected module name\0\0\0\0");
            mw_emit_fatal_error_21_();
            break;
    
}}

static void mw_elab_data_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_data_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_token_next();
}

static void mw_elab_data_header_21_ (void){
    mw_dup2();
    mw_swap();
    mw_data_header();
    mw__21_();
    mw_sig_token_is_type_con_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("Expected type name.\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_dup2();
    mw_token_name_40_();
    mw_name_undefined_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        mw_drop2();
        push_ptr("Name already defined.\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_over();
    mw_TData();
    mw_DEF_TYPE();
    mw_over();
    mw_name_def();
    mw__21_();
    mw_swap();
    mw_data_name();
    mw__21_();
    mw_token_num_args();
    mw_over();
    mw_data_arity();
    mw__21_();
}

static void mw_elab_data_tag_21_ (void){
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("Expected constructor name.\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_token_name_3F_();
    mw_name_undefined_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        mw_drop();
        push_ptr("Name already defined. (Overlapping tags not supported.)\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_Tag_2E_alloc_21_();
    mw_dup2();
    mw_DEF_TAG();
    mw_swap();
    mw_name_def();
    mw__21_();
    mw_tuck();
    mw_tag_name();
    mw__21_();
    {
        VAL d2 = pop_value();
        mw_over();
        push_value(d2);
    }
    mw_dup2();
    mw_tag_data();
    mw__21_();
    mw_tuck();
    {
        VAL d2 = pop_value();
        mw_data_add_tag_21_();
        push_value(d2);
    }
    mw_swap();
    mw_token_succ();
    mw_token_is_arrow_3F_();
    if (pop_u64()) {
        mw_token_succ();
        mw_SOME();
        mw_over();
        mw_tag_sig();
        mw__21_();
    } else {
        mw_token_run_end_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_NONE();
            mw_over();
            mw_tag_sig();
            mw__21_();
        } else {
            push_ptr("Expected arrow, comma, or right paren.\0\0\0\0");
            mw_emit_fatal_error_21_();
        }
    }
    mw_dup();
    push_u64(0);
    push_fnptr(&mb_elab_data_tag_21__11);
    mw_prim_pack_cons();
    mw_delay();
    mw_swap();
    mw_tag_ctx_type();
    mw__21_();
}

static void mw_expect_token_comma (void){
    mw_token_is_comma_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("Expected comma.\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mw_expect_token_rparen (void){
    mw_token_is_rparen_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("Expected right parenthesis.\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mw_expect_token_arrow (void){
    mw_token_is_arrow_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("Expected arrow.\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mw_token_def_args (void){
    mw_dup();
    mw_token_args();
    mw_len_3F_();
    push_i64(2LL);
    mw__3E__3D_();
    if (pop_u64()) {
        mw_nip();
    } else {
        mw_drop();
        push_ptr("def expects at least two arguments\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_List__3E_List_2B_();
    mw_unwrap();
    mw_uncons();
    mw_List__3E_List_2B_();
    mw_unwrap();
    mw_uncons();
    mw_is_nil_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_L1_2B_();
        {
            VAL d3 = pop_value();
            mw_NONE();
            push_value(d3);
        }
    } else {
        mw_over();
        mw_token_run_has_arrow();
        if (pop_u64()) {
            mw_cons_2B_();
            {
                VAL d4 = pop_value();
                mw_NONE();
                push_value(d4);
            }
        } else {
            mw_List__3E_List_2B_();
            mw_unwrap();
            {
                VAL d4 = pop_value();
                mw_SOME();
                push_value(d4);
            }
        }
    }
}

static void mw_elab_def_missing_21_ (void){
    mw_dup();
    mw_token_succ();
    mw_token_succ();
    mw_token_name_40_();
    mw_name_defined_3F_();
    mw_nip();
    if (pop_u64()) {
        mw_token_next();
    } else {
        mw_elab_def_21_();
    }
}

static void mw_elab_def_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_def_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_token_def_args();
    mw_uncons();
    mw_is_nil();
    if (pop_u64()) {
        mw_id();
    } else {
        mw_dup();
        mw_token_run_has_arrow();
        if (pop_u64()) {
            mw_id();
        } else {
            push_ptr("expected match case\0\0\0\0");
            mw_emit_fatal_error_21_();
        }
    }
    mw_rotl();
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("expected word name\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_token_name_3F_();
    mw_name_undefined_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        mw_drop();
        push_ptr("word already defined\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_Word_2E_alloc_21_();
    mw_dup2();
    mw_DEF_WORD();
    mw_swap();
    mw_name_def();
    mw__21_();
    mw_tuck();
    mw_word_name();
    mw__21_();
    mw_tuck();
    mw_word_head();
    mw__21_();
    mw_tuck();
    mw_word_body();
    mw__21_();
    mw_tuck();
    mw_word_sig();
    mw__21_();
    mw_dup();
    push_u64(0);
    push_fnptr(&mb_elab_def_21__10);
    mw_prim_pack_cons();
    mw_delay();
    mw_over();
    mw_word_ctx_type();
    mw__21_();
    mw_dup();
    push_u64(0);
    push_fnptr(&mb_elab_def_21__15);
    mw_prim_pack_cons();
    mw_delay();
    mw_over();
    mw_word_params();
    mw__21_();
    mw_dup();
    push_u64(0);
    push_fnptr(&mb_elab_def_21__16);
    mw_prim_pack_cons();
    mw_delay();
    mw_swap();
    mw_word_arrow();
    mw__21_();
}

static void mw_elab_def_params_21_ (void){
    mw_L0();
    mw_over();
    mw_elab_word_ctx_sig_weak_21_();
    mw_nip();
    mw_rotl();
    mw_word_head();
    mw__40_();
    mw_elab_expand_morphism_21_();
    mw_nip();
    mw_token_args();
    push_u64(0);
    push_fnptr(&mb_elab_def_params_21__1);
    mw_prim_pack_cons();
    mw_reverse_for();
    mw_drop();
}

static void mw_elab_def_body_21_ (void){
    mw_ab_token();
    mw__40_();
    mw_token_run_has_arrow();
    if (pop_u64()) {
        mw_dup();
        mw_ab_token();
        mw__40_();
        mw_elab_match_at_21_();
    } else {
        mw_elab_atoms_21_();
    }
}

static void mw_elab_def_external_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_def_external_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_token_args_2();
    mw_swap();
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_token_name_3F_();
        mw_name_undefined_3F_();
        if (pop_u64()) {
            mw_nip();
            mw_External_2E_alloc_21_();
            mw_dup2();
            mw_DEF_EXTERNAL();
            mw_swap();
            mw_name_def();
            mw__21_();
            mw_tuck();
            mw_external_name();
            mw__21_();
            mw_tuck();
            mw_external_sig();
            mw__21_();
            mw_dup();
            push_u64(0);
            push_fnptr(&mb_elab_def_external_21__4);
            mw_prim_pack_cons();
            mw_delay();
            mw_swap();
            mw_external_ctx_type();
            mw__21_();
        } else {
            mw_drop();
            push_ptr("word already defined\0\0\0\0");
            mw_emit_fatal_error_21_();
        }
    } else {
        push_ptr("expected word name\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mw_elab_def_type_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_def_type_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_token_args_2();
    mw_swap();
    mw_sig_token_is_type_con_3F_();
    if (pop_u64()) {
        mw_token_name_3F_();
        mw_name_undefined_3F_();
        if (pop_u64()) {
            mw_nip();
            mw_swap();
            mw_elab_simple_type_arg_21_();
            mw_DEF_TYPE();
            mw_swap();
            mw_name_def();
            mw__21_();
        } else {
            mw_drop();
            push_ptr("type already defined\0\0\0\0");
            mw_emit_fatal_error_21_();
        }
    } else {
        push_ptr("expected type constructor\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mw_elab_buffer_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_buffer_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_token_args_2();
    mw_swap();
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_token_name_3F_();
        mw_name_undefined_3F_();
        if (pop_u64()) {
            mw_nip();
            mw_swap();
            mw_token_int_40_();
            mw_buffer_new_21_();
            mw_drop();
        } else {
            mw_drop();
            push_ptr("buffer already defined\0\0\0\0");
            mw_emit_fatal_error_21_();
        }
    } else {
        push_ptr("expected buffer name\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mw_elab_variable_21_ (void){
    mw_dup();
    mw_token_args_2();
    mw_swap();
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("expected variable name\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_token_name_3F_();
    mw_name_undefined_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        mw_drop();
        push_ptr("name already defined\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_nip();
    mw_swap();
    push_u64(0);
    push_fnptr(&mb_elab_variable_21__5);
    mw_prim_pack_cons();
    mw_delay();
    mw_variable_new_21_();
    mw_drop();
    mw_token_next();
}

static void mw_elab_table_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_table_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_token_args_1();
    mw_sig_token_is_type_con_3F_();
    if (pop_u64()) {
        mw_token_name_40_();
        mw_table_new_21_();
        mw_drop();
    } else {
        push_ptr("expected table name\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mw_elab_target_c99_21_ (void){
    mw_typecheck_everything_21_();
    push_u64(0);
    push_fnptr(&mb_elab_target_c99_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_token_args_2();
    {
        VAL d2 = pop_value();
        mw_token_str_40_();
        mw_Str__3E_Path();
        push_value(d2);
    }
    {
        VAL d2 = pop_value();
        mw_ctx_empty();
        mw_T0();
        mw_T0();
        mw_T__3E_();
        push_value(d2);
    }
    mw_elab_arrow_21_();
    mw_swap();
    mw_run_output_c99_21_();
}

static void mw_elab_embed_str_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_embed_str_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_token_args_2();
    mw_swap();
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("expected name\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_token_name_3F_();
    mw_name_undefined_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        mw_drop();
        push_ptr("name already defined\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_nip();
    mw_swap();
    mw_token_is_str_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("expected path\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_token_str_3F_();
    push_u64(0);
    push_fnptr(&mb_elab_embed_str_21__8);
    mw_prim_pack_cons();
    push_u64(0);
    push_fnptr(&mb_elab_embed_str_21__9);
    mw_prim_pack_cons();
    mw_with_open_file_21_();
    mw_VALUE_STR();
    mw_DEF_CONSTANT();
    mw_swap();
    mw_name_def();
    mw__21_();
}

static void mw_typecheck_everything_21_ (void){
    push_u64(0);
    push_fnptr(&mb_typecheck_everything_21__1);
    mw_prim_pack_cons();
    mw_Name_2E_for();
    push_u64(0);
    push_fnptr(&mb_typecheck_everything_21__2);
    mw_prim_pack_cons();
    mw_Block_2E_for();
}

static void mw_typecheck_def_21_ (void){
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_id();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            break;
        case 6LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            break;
        case 4LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            break;
        case 2LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            break;
        case 9LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_external_sig_21_();
            mw_drop();
            break;
        case 5LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_word_arrow();
            mw_force_21_();
            mw_drop();
            break;
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_field_type_21_();
            mw_drop();
            break;
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_tag_sig_21_();
            mw_drop();
            break;
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_variable_type();
            mw_force_21_();
            mw_drop();
            break;
        case 8LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_drop();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mw_TABLE_MAX_SIZE (void){
    push_i64(65536LL);
}

static void mw_table_new_21_ (void){
    mw_Table_2E_alloc_21_();
    mw_dup2();
    mw_TTable();
    mw_DEF_TYPE();
    mw_swap();
    mw_name_def();
    mw__21_();
    mw_tuck();
    mw_table_name();
    mw__21_();
    mw_TABLE_MAX_SIZE();
    mw_over();
    mw_table_max_count();
    mw__21_();
    mw_dup();
    mw_table_name();
    mw__40_();
    push_ptr(".MAX\0\0\0\0");
    mw_name_cat_21_();
    mw_Word_2E_alloc_21_();
    mw_dup2();
    mw_DEF_WORD();
    mw_swap();
    mw_name_def();
    mw__21_();
    mw_tuck();
    mw_word_name();
    mw__21_();
    mw_L0();
    mw_CTX();
    mw_T0();
    mw_TYPE_INT();
    mw_T1();
    mw_T__3E_();
    mw_ready2();
    mw_over();
    mw_word_ctx_type();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_table_new_21__1);
    mw_prim_pack_cons();
    mw_ab_build_word_21_();
    mw_drop();
    mw_dup();
    mw_table_name();
    mw__40_();
    push_ptr(".NUM\0\0\0\0");
    mw_name_cat_21_();
    push_i64(8LL);
    mw_buffer_new_21_();
    mw_over();
    mw_table_num_buffer();
    mw__21_();
    mw_dup();
    mw_table_name();
    mw__40_();
    push_ptr(".id\0\0\0\0");
    mw_name_cat_21_();
    mw_Word_2E_alloc_21_();
    mw_dup2();
    mw_DEF_WORD();
    mw_swap();
    mw_name_def();
    mw__21_();
    mw_tuck();
    mw_word_name();
    mw__21_();
    mw_L0();
    mw_CTX();
    mw_over2();
    mw_TTable();
    mw_T1();
    mw_TYPE_INT();
    mw_T1();
    mw_T__3E_();
    mw_ready2();
    mw_over();
    mw_word_ctx_type();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_table_new_21__2);
    mw_prim_pack_cons();
    mw_ab_build_word_21_();
    mw_drop();
    mw_dup();
    mw_table_name();
    mw__40_();
    push_ptr(".succ\0\0\0\0");
    mw_name_cat_21_();
    mw_Word_2E_alloc_21_();
    mw_dup2();
    mw_DEF_WORD();
    mw_swap();
    mw_name_def();
    mw__21_();
    mw_tuck();
    mw_word_name();
    mw__21_();
    mw_L0();
    mw_CTX();
    mw_over2();
    mw_TTable();
    mw_T1();
    mw_dup();
    mw_T__3E_();
    mw_ready2();
    mw_over();
    mw_word_ctx_type();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_table_new_21__3);
    mw_prim_pack_cons();
    mw_ab_build_word_21_();
    mw_drop();
    mw_dup();
    mw_table_name();
    mw__40_();
    push_ptr(".pred\0\0\0\0");
    mw_name_cat_21_();
    mw_Word_2E_alloc_21_();
    mw_dup2();
    mw_DEF_WORD();
    mw_swap();
    mw_name_def();
    mw__21_();
    mw_tuck();
    mw_word_name();
    mw__21_();
    mw_L0();
    mw_CTX();
    mw_over2();
    mw_TTable();
    mw_T1();
    mw_dup();
    mw_T__3E_();
    mw_ready2();
    mw_over();
    mw_word_ctx_type();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_table_new_21__4);
    mw_prim_pack_cons();
    mw_ab_build_word_21_();
    mw_drop();
    mw_dup();
    mw_table_name();
    mw__40_();
    push_ptr(".for\0\0\0\0");
    mw_name_cat_21_();
    mw_Word_2E_alloc_21_();
    mw_dup2();
    mw_DEF_WORD();
    mw_swap();
    mw_name_def();
    mw__21_();
    mw_tuck();
    mw_word_name();
    mw__21_();
    push_ptr("x\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_21_();
    {
        VAL var_x = pop_value();
        VAL var_w = pop_value();
        VAL var_t = pop_value();
        push_ptr("a\0\0\0\0");
        mw_name_new_21_();
        mw_var_new_implicit_21_();
        mw_TYPE_STACK();
        mw_over();
        mw_var_type();
        mw__21_();
        mw_dup();
        mw_L1();
        mw_CTX();
        mw_swap();
        mw_TVar();
        {
            VAL var_a = pop_value();
            push_value(var_a);
            incref(var_a);
            push_value(var_a);
            incref(var_a);
            push_value(var_t);
            incref(var_t);
            mw_TTable();
            mw_T_2A_();
            push_value(var_a);
            incref(var_a);
            mw_T__3E_();
            mw_T_2A_();
            push_value(var_a);
            incref(var_a);
            mw_T__3E_();
            mw_ready2();
            push_value(var_w);
            incref(var_w);
            mw_word_ctx_type();
            mw__21_();
            push_value(var_a);
            incref(var_a);
            push_value(var_t);
            incref(var_t);
            mw_TTable();
            mw_T_2A_();
            push_value(var_a);
            incref(var_a);
            mw_T__3E_();
            push_value(var_x);
            incref(var_x);
            mw_var_type();
            mw__21_();
            mw_true();
            push_value(var_x);
            incref(var_x);
            mw_var_auto_run();
            mw__21_();
            decref(var_a);
        }
        push_value(var_w);
        incref(var_w);
        push_u64(0);
        push_value(var_x);
        incref(var_x);
        mw_prim_pack_cons();
        push_value(var_w);
        incref(var_w);
        mw_prim_pack_cons();
        push_value(var_t);
        incref(var_t);
        mw_prim_pack_cons();
        push_fnptr(&mb_table_new_21__9);
        mw_prim_pack_cons();
        mw_ab_build_word_21_();
        mw_drop();
        push_value(var_t);
        incref(var_t);
        decref(var_x);
        decref(var_w);
        decref(var_t);
    }
    mw_dup();
    mw_table_name();
    mw__40_();
    push_ptr(".alloc!\0\0\0\0");
    mw_name_cat_21_();
    mw_Word_2E_alloc_21_();
    mw_dup2();
    mw_DEF_WORD();
    mw_swap();
    mw_name_def();
    mw__21_();
    mw_tuck();
    mw_word_name();
    mw__21_();
    mw_L0();
    mw_CTX();
    mw_T0();
    mw_over3();
    mw_TTable();
    mw_T1();
    mw_T__3E_();
    mw_ready2();
    mw_over();
    mw_word_ctx_type();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_table_new_21__14);
    mw_prim_pack_cons();
    mw_ab_build_word_21_();
    mw_drop();
}

static void mw_elab_field_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_field_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_token_args_3();
    mw_rotl();
    mw_dup();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_name_undefined_3F_();
            if (pop_u64()) {
                mw_id();
            } else {
                mw_drop();
                push_ptr("name already defined\0\0\0\0");
                mw_emit_fatal_error_21_();
            }
            mw_nip();
            mw_rotr();
            mw_field_new_21_();
            mw_drop();
            break;
        default:
            mw_drop();
            push_ptr("expected field name\0\0\0\0");
            mw_emit_fatal_error_21_();
            break;
    
}}

static void mw_field_new_21_ (void){
    mw_Field_2E_alloc_21_();
    mw_swap();
    push_u64(0);
    push_fnptr(&mb_field_new_21__1);
    mw_prim_pack_cons();
    mw_delay();
    mw_over();
    mw_field_value_type();
    mw__21_();
    mw_swap();
    push_u64(0);
    push_fnptr(&mb_field_new_21__2);
    mw_prim_pack_cons();
    mw_delay();
    mw_over();
    mw_field_index_type();
    mw__21_();
    mw_tuck();
    mw_dup2();
    mw_field_name();
    mw__21_();
    mw_DEF_FIELD();
    mw_swap();
    mw_name_def();
    mw__21_();
}

static void mw_elab_field_type_21_ (void){
    push_u64(0);
    push_fnptr(&mb_elab_field_type_21__1);
    mw_prim_pack_cons();
    mw_sip();
    mw_field_value_type();
    mw_force_21_();
    mw_TMut();
    mw_T1();
    mw_T__3E_();
}

static void mw_name_prim_3D_ (void){
    {
        VAL d2 = pop_value();
        mw_name_def();
        mw__40_();
        push_value(d2);
    }
    mw_DEF_PRIM();
    mw__3D__3D_();
}

static void mw_token_prim_3D__3F_ (void){
    {
        VAL d2 = pop_value();
        mw_dup();
        push_value(d2);
    }
    mw_token_prim_3D_();
}

static void mw_token_prim_3D_ (void){
    mw_swap();
    mw_token_value();
    mw__40_();
    switch (get_top_data_tag()) {
        case 10LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_swap();
            mw_name_prim_3D_();
            break;
        default:
            mw_drop2();
            mw_false();
            break;
    
}}

static void mw_def_prim_21_ (void){
    mw_name_new_21_();
    mw_dup2();
    {
        VAL d2 = pop_value();
        mw_DEF_PRIM();
        push_value(d2);
    }
    mw_name_def();
    mw__21_();
    mw_swap();
    mw_prim_name();
    mw__21_();
}

static void mw_init_prims_21_ (void){
    mw_PRIM_SYNTAX_MODULE();
    push_ptr("module\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_IMPORT();
    push_ptr("import\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_DEF();
    push_ptr("def\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_DEF_TYPE();
    push_ptr("def-type\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_DEF_MISSING();
    push_ptr("def-missing\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_BUFFER();
    push_ptr("buffer\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_DEF_EXTERNAL();
    push_ptr("def-external\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_TABLE();
    push_ptr("table\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_FIELD();
    push_ptr("field\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_TARGET_C99();
    push_ptr("target-c99\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_EMBED_STR();
    push_ptr("embed-str\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_DATA();
    push_ptr("data\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_VARIABLE();
    push_ptr("var\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_ARROW();
    push_ptr("->\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYNTAX_DASHES();
    push_ptr("--\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_DIP();
    push_ptr("dip\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_IF();
    push_ptr("if\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_WHILE();
    push_ptr("while\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_MATCH();
    push_ptr("match\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_LAMBDA();
    push_ptr("\\\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_ID();
    push_ptr("prim-id\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_DUP();
    push_ptr("prim-dup\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_DROP();
    push_ptr("prim-drop\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_SWAP();
    push_ptr("prim-swap\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_DIP();
    push_ptr("prim-dip\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_IF();
    push_ptr("prim-if\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_WHILE();
    push_ptr("prim-while\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_DEBUG();
    push_ptr("prim-debug\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_RUN();
    push_ptr("prim-run\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_MATCH();
    push_ptr("prim-match\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_CORE_LAMBDA();
    push_ptr("prim-lambda\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_UNSAFE_CAST();
    push_ptr("prim-unsafe-cast\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_VALUE_EQ();
    push_ptr("prim-value-eq\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_VALUE_LT();
    push_ptr("prim-value-lt\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_VALUE_LE();
    push_ptr("prim-value-le\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_VALUE_GET();
    push_ptr("prim-value-get\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_VALUE_SET();
    push_ptr("prim-value-set\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_INT_ADD();
    push_ptr("prim-int-add\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_INT_SUB();
    push_ptr("prim-int-sub\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_INT_MUL();
    push_ptr("prim-int-mul\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_INT_DIV();
    push_ptr("prim-int-div\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_INT_MOD();
    push_ptr("prim-int-mod\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_INT_AND();
    push_ptr("prim-int-and\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_INT_OR();
    push_ptr("prim-int-or\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_INT_XOR();
    push_ptr("prim-int-xor\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_INT_SHL();
    push_ptr("prim-int-shl\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_INT_SHR();
    push_ptr("prim-int-shr\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_INT_GET();
    push_ptr("prim-int-get\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_INT_SET();
    push_ptr("prim-int-set\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_BOOL_TRUE();
    push_ptr("prim-bool-true\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_BOOL_FALSE();
    push_ptr("prim-bool-false\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_BOOL_AND();
    push_ptr("prim-bool-and\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_BOOL_OR();
    push_ptr("prim-bool-or\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_PACK_NIL();
    push_ptr("prim-pack-nil\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_PACK_CONS();
    push_ptr("prim-pack-cons\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_PACK_UNCONS();
    push_ptr("prim-pack-uncons\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_MUT_NEW();
    push_ptr("prim-mut-new\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_MUT_GET();
    push_ptr("prim-mut-get\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_MUT_SET();
    push_ptr("prim-mut-set\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_PTR_ADD();
    push_ptr("prim-ptr-add\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_PTR_SIZE();
    push_ptr("prim-ptr-size\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_PTR_GET();
    push_ptr("prim-ptr-get\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_PTR_SET();
    push_ptr("prim-ptr-set\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_PTR_ALLOC();
    push_ptr("prim-ptr-alloc\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_PTR_REALLOC();
    push_ptr("prim-ptr-realloc\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_PTR_FILL();
    push_ptr("prim-ptr-fill\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_PTR_COPY();
    push_ptr("prim-ptr-copy\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_PTR_RAW();
    push_ptr("prim-ptr-raw\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_STR_ALLOC();
    push_ptr("prim-str-alloc\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_STR_SIZE();
    push_ptr("prim-str-size\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_STR_BASE();
    push_ptr("prim-str-base\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_STR_EQ();
    push_ptr("prim-str-eq\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_U8_GET();
    push_ptr("prim-u8-get\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_U8_SET();
    push_ptr("prim-u8-set\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_U16_GET();
    push_ptr("prim-u16-get\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_U16_SET();
    push_ptr("prim-u16-set\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_U32_GET();
    push_ptr("prim-u32-get\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_U32_SET();
    push_ptr("prim-u32-set\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_U64_GET();
    push_ptr("prim-u64-get\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_U64_SET();
    push_ptr("prim-u64-set\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_I8_GET();
    push_ptr("prim-i8-get\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_I8_SET();
    push_ptr("prim-i8-set\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_I16_GET();
    push_ptr("prim-i16-get\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_I16_SET();
    push_ptr("prim-i16-set\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_I32_GET();
    push_ptr("prim-i32-get\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_I32_SET();
    push_ptr("prim-i32-set\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_I64_GET();
    push_ptr("prim-i64-get\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_I64_SET();
    push_ptr("prim-i64-set\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYS_OS();
    push_ptr("prim-sys-os\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYS_ARGC();
    push_ptr("prim-sys-argc\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_SYS_ARGV();
    push_ptr("prim-sys-argv\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_POSIX_READ();
    push_ptr("prim-posix-read\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_POSIX_WRITE();
    push_ptr("prim-posix-write\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_POSIX_OPEN();
    push_ptr("prim-posix-open\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_POSIX_CLOSE();
    push_ptr("prim-posix-close\0\0\0\0");
    mw_def_prim_21_();
    mw_PRIM_POSIX_EXIT();
    push_ptr("prim-posix-exit\0\0\0\0");
    mw_def_prim_21_();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__1);
    mw_prim_pack_cons();
    mw_PRIM_SYNTAX_IMPORT();
    mw_prim_decl();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__2);
    mw_prim_pack_cons();
    mw_PRIM_SYNTAX_DEF();
    mw_prim_decl();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__3);
    mw_prim_pack_cons();
    mw_PRIM_SYNTAX_DEF_MISSING();
    mw_prim_decl();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__4);
    mw_prim_pack_cons();
    mw_PRIM_SYNTAX_DEF_EXTERNAL();
    mw_prim_decl();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__5);
    mw_prim_pack_cons();
    mw_PRIM_SYNTAX_DEF_TYPE();
    mw_prim_decl();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__6);
    mw_prim_pack_cons();
    mw_PRIM_SYNTAX_BUFFER();
    mw_prim_decl();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__7);
    mw_prim_pack_cons();
    mw_PRIM_SYNTAX_VARIABLE();
    mw_prim_decl();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__8);
    mw_prim_pack_cons();
    mw_PRIM_SYNTAX_TABLE();
    mw_prim_decl();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__9);
    mw_prim_pack_cons();
    mw_PRIM_SYNTAX_FIELD();
    mw_prim_decl();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__10);
    mw_prim_pack_cons();
    mw_PRIM_SYNTAX_DATA();
    mw_prim_decl();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__11);
    mw_prim_pack_cons();
    mw_PRIM_SYNTAX_TARGET_C99();
    mw_prim_decl();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__12);
    mw_prim_pack_cons();
    mw_PRIM_SYNTAX_EMBED_STR();
    mw_prim_decl();
    mw__21_();
    mw_T0();
    mw_T0();
    mw_T__3E_();
    mw_dup();
    mw_PRIM_CORE_ID();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_CORE_DEBUG();
    mw_prim_type();
    mw__21_();
    mw_drop();
    mw_TYPE_INT();
    mw_TYPE_INT();
    mw_T2();
    mw_TYPE_INT();
    mw_T1();
    mw_T__3E_();
    mw_dup();
    mw_PRIM_INT_ADD();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_INT_SUB();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_INT_MUL();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_INT_DIV();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_INT_MOD();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_INT_AND();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_INT_OR();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_INT_XOR();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_INT_SHL();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_INT_SHR();
    mw_prim_type();
    mw__21_();
    mw_drop();
    mw_TYPE_PTR();
    mw_T1();
    mw_TYPE_INT();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_INT_GET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_T1();
    mw_TYPE_PTR();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_PTR_GET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_T1();
    mw_TYPE_U8();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_U8_GET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_T1();
    mw_TYPE_U16();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_U16_GET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_T1();
    mw_TYPE_U32();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_U32_GET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_T1();
    mw_TYPE_U64();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_U64_GET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_T1();
    mw_TYPE_I8();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_I8_GET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_T1();
    mw_TYPE_I16();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_I16_GET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_T1();
    mw_TYPE_I32();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_I32_GET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_T1();
    mw_TYPE_I64();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_I64_GET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_INT();
    mw_TYPE_PTR();
    mw_T2();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_INT_SET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_TYPE_PTR();
    mw_T2();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_PTR_SET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_U8();
    mw_TYPE_PTR();
    mw_T2();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_U8_SET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_U16();
    mw_TYPE_PTR();
    mw_T2();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_U16_SET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_U32();
    mw_TYPE_PTR();
    mw_T2();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_U32_SET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_U64();
    mw_TYPE_PTR();
    mw_T2();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_U64_SET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_I8();
    mw_TYPE_PTR();
    mw_T2();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_I8_SET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_I16();
    mw_TYPE_PTR();
    mw_T2();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_I16_SET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_I32();
    mw_TYPE_PTR();
    mw_T2();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_I32_SET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_I64();
    mw_TYPE_PTR();
    mw_T2();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_I64_SET();
    mw_prim_type();
    mw__21_();
    mw_TYPE_INT();
    mw_TYPE_PTR();
    mw_TYPE_INT();
    mw_T3();
    mw_TYPE_INT();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_POSIX_READ();
    mw_prim_type();
    mw__21_();
    mw_TYPE_INT();
    mw_TYPE_PTR();
    mw_TYPE_INT();
    mw_T3();
    mw_TYPE_INT();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_POSIX_WRITE();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_TYPE_INT();
    mw_TYPE_INT();
    mw_T3();
    mw_TYPE_INT();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_POSIX_OPEN();
    mw_prim_type();
    mw__21_();
    mw_TYPE_INT();
    mw_T1();
    mw_TYPE_INT();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_POSIX_CLOSE();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_TYPE_INT();
    mw_TYPE_INT();
    mw_TYPE_INT();
    mw_TYPE_INT();
    mw_TYPE_INT();
    mw_T6();
    mw_TYPE_PTR();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_POSIX_MMAP();
    mw_prim_type();
    mw__21_();
    mw_TYPE_INT();
    mw_TYPE_PTR();
    mw_T2();
    mw_TYPE_PTR();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_PTR_ADD();
    mw_prim_type();
    mw__21_();
    mw_T0();
    mw_TYPE_INT();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_PTR_SIZE();
    mw_prim_type();
    mw__21_();
    mw_TYPE_INT();
    mw_T1();
    mw_TYPE_PTR();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_PTR_ALLOC();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_TYPE_INT();
    mw_T2();
    mw_TYPE_PTR();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_PTR_REALLOC();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_TYPE_INT();
    mw_TYPE_PTR();
    mw_T3();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_PTR_COPY();
    mw_prim_type();
    mw__21_();
    mw_TYPE_INT();
    mw_TYPE_INT();
    mw_TYPE_PTR();
    mw_T3();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_PTR_FILL();
    mw_prim_type();
    mw__21_();
    mw_TYPE_PTR();
    mw_T1();
    mw_TYPE_PTR();
    mw_TYPE_INT();
    mw_T2();
    mw_T__3E_();
    mw_PRIM_PTR_RAW();
    mw_prim_type();
    mw__21_();
    mw_TYPE_INT();
    mw_T1();
    mw_TYPE_STR();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_STR_ALLOC();
    mw_prim_type();
    mw__21_();
    mw_TYPE_STR();
    mw_T1();
    mw_TYPE_STR();
    mw_TYPE_INT();
    mw_T2();
    mw_T__3E_();
    mw_PRIM_STR_SIZE();
    mw_prim_type();
    mw__21_();
    mw_TYPE_STR();
    mw_T1();
    mw_TYPE_PTR();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_STR_BASE();
    mw_prim_type();
    mw__21_();
    mw_TYPE_STR();
    mw_TYPE_STR();
    mw_T2();
    mw_TYPE_BOOL();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_STR_EQ();
    mw_prim_type();
    mw__21_();
    mw_T0();
    mw_TYPE_INT();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_SYS_OS();
    mw_prim_type();
    mw__21_();
    mw_T0();
    mw_TYPE_INT();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_SYS_ARGC();
    mw_prim_type();
    mw__21_();
    mw_T0();
    mw_TYPE_PTR();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_SYS_ARGV();
    mw_prim_type();
    mw__21_();
    mw_T0();
    mw_TYPE_BOOL();
    mw_T1();
    mw_T__3E_();
    mw_dup();
    mw_PRIM_BOOL_TRUE();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_BOOL_FALSE();
    mw_prim_type();
    mw__21_();
    mw_drop();
    mw_TYPE_BOOL();
    mw_TYPE_BOOL();
    mw_T2();
    mw_TYPE_BOOL();
    mw_T1();
    mw_T__3E_();
    mw_dup();
    mw_PRIM_BOOL_AND();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_BOOL_OR();
    mw_prim_type();
    mw__21_();
    mw_drop();
    mw_T0();
    mw_T0();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_PACK_NIL();
    mw_prim_type();
    mw__21_();
    push_ptr("a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_CORE_DROP();
    mw_prim_ctx();
    mw__21_();
    mw_TVar();
    mw_T1();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_CORE_DROP();
    mw_prim_type();
    mw__21_();
    push_ptr("a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_CORE_DUP();
    mw_prim_ctx();
    mw__21_();
    mw_TVar();
    mw_dup();
    mw_T1();
    mw_dup();
    mw_rotl();
    mw_T_2A_();
    mw_T__3E_();
    mw_PRIM_CORE_DUP();
    mw_prim_type();
    mw__21_();
    push_ptr("a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_dup();
    mw_PRIM_VALUE_EQ();
    mw_prim_ctx();
    mw__21_();
    mw_dup();
    mw_PRIM_VALUE_LT();
    mw_prim_ctx();
    mw__21_();
    mw_dup();
    mw_PRIM_VALUE_LE();
    mw_prim_ctx();
    mw__21_();
    mw_drop();
    mw_TVar();
    mw_dup();
    mw_T2();
    mw_TYPE_BOOL();
    mw_T1();
    mw_T__3E_();
    mw_dup();
    mw_PRIM_VALUE_EQ();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_VALUE_LT();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_PRIM_VALUE_LE();
    mw_prim_type();
    mw__21_();
    mw_drop();
    push_ptr("a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    push_ptr("b\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup2();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_CORE_SWAP();
    mw_prim_ctx();
    mw__21_();
    {
        VAL d2 = pop_value();
        mw_TVar();
        push_value(d2);
    }
    mw_TVar();
    mw_dup2();
    mw_T2();
    mw_rotr();
    mw_swap();
    mw_T2();
    mw_T__3E_();
    mw_PRIM_CORE_SWAP();
    mw_prim_type();
    mw__21_();
    push_ptr("a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    push_ptr("b\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup2();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_UNSAFE_CAST();
    mw_prim_ctx();
    mw__21_();
    {
        VAL d2 = pop_value();
        mw_TVar();
        mw_T1();
        push_value(d2);
    }
    mw_TVar();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_UNSAFE_CAST();
    mw_prim_type();
    mw__21_();
    push_ptr("*a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_STACK();
    mw_over();
    mw_var_type();
    mw__21_();
    push_ptr("*b\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_STACK();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup2();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_CORE_RUN();
    mw_prim_ctx();
    mw__21_();
    {
        VAL d2 = pop_value();
        mw_TVar();
        push_value(d2);
    }
    mw_TVar();
    mw_dup2();
    mw_T__3E_();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_T_2A_();
        push_value(d2);
    }
    mw_T__3E_();
    mw_PRIM_CORE_RUN();
    mw_prim_type();
    mw__21_();
    push_ptr("*a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_STACK();
    mw_over();
    mw_var_type();
    mw__21_();
    push_ptr("*b\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_STACK();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup2();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_POSIX_EXIT();
    mw_prim_ctx();
    mw__21_();
    {
        VAL d2 = pop_value();
        mw_TVar();
        push_value(d2);
    }
    mw_TVar();
    {
        VAL d2 = pop_value();
        mw_TYPE_INT();
        mw_T_2A_();
        push_value(d2);
    }
    mw_T__3E_();
    mw_PRIM_POSIX_EXIT();
    mw_prim_type();
    mw__21_();
    push_ptr("*a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_STACK();
    mw_over();
    mw_var_type();
    mw__21_();
    push_ptr("*b\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_STACK();
    mw_over();
    mw_var_type();
    mw__21_();
    push_ptr("c\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup3();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_swap();
    mw_ctx_new_21_();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_CORE_DIP();
    mw_prim_ctx();
    mw__21_();
    {
        VAL d2 = pop_value();
        {
            VAL d3 = pop_value();
            mw_TVar();
            push_value(d3);
        }
        mw_TVar();
        push_value(d2);
    }
    mw_TVar();
    {
        VAL d2 = pop_value();
        mw_dup2();
        push_value(d2);
    }
    mw_tuck();
    mw_T_2A_();
    {
        VAL d2 = pop_value();
        mw_T_2A_();
        push_value(d2);
    }
    {
        VAL d2 = pop_value();
        {
            VAL d3 = pop_value();
            mw_T__3E_();
            push_value(d3);
        }
        mw_swap();
        mw_T_2A_();
        push_value(d2);
    }
    mw_T__3E_();
    mw_PRIM_CORE_DIP();
    mw_prim_type();
    mw__21_();
    push_ptr("*a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_STACK();
    mw_over();
    mw_var_type();
    mw__21_();
    push_ptr("*b\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_STACK();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup2();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_CORE_IF();
    mw_prim_ctx();
    mw__21_();
    {
        VAL d2 = pop_value();
        mw_TVar();
        push_value(d2);
    }
    mw_TVar();
    mw_dup2();
    mw_T__3E_();
    mw_swap();
    {
        VAL d2 = pop_value();
        {
            VAL d3 = pop_value();
            mw_TYPE_BOOL();
            mw_T_2A_();
            push_value(d3);
        }
        push_u64(0);
        push_fnptr(&mb_init_prims_21__28);
        mw_prim_pack_cons();
        mw_sip();
        mw_T_2A_();
        push_value(d2);
    }
    mw_T__3E_();
    mw_PRIM_CORE_IF();
    mw_prim_type();
    mw__21_();
    push_ptr("*a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_STACK();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_CORE_WHILE();
    mw_prim_ctx();
    mw__21_();
    mw_TVar();
    mw_dup();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__29);
    mw_prim_pack_cons();
    mw_sip();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__30);
    mw_prim_pack_cons();
    mw_sip();
    mw_T__3E_();
    mw_PRIM_CORE_WHILE();
    mw_prim_type();
    mw__21_();
    push_ptr("*a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_STACK();
    mw_over();
    mw_var_type();
    mw__21_();
    push_ptr("b\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup2();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_PACK_CONS();
    mw_prim_ctx();
    mw__21_();
    {
        VAL d2 = pop_value();
        mw_TVar();
        push_value(d2);
    }
    mw_TVar();
    mw_dup2();
    mw_T2();
    mw_rotr();
    mw_T_2A_();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_PACK_CONS();
    mw_prim_type();
    mw__21_();
    push_ptr("*a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_STACK();
    mw_over();
    mw_var_type();
    mw__21_();
    push_ptr("b\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup2();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_PACK_UNCONS();
    mw_prim_ctx();
    mw__21_();
    {
        VAL d2 = pop_value();
        mw_TVar();
        push_value(d2);
    }
    mw_TVar();
    mw_dup2();
    mw_T_2A_();
    mw_T1();
    mw_rotr();
    mw_T2();
    mw_T__3E_();
    mw_PRIM_PACK_UNCONS();
    mw_prim_type();
    mw__21_();
    push_ptr("a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_VALUE_GET();
    mw_prim_ctx();
    mw__21_();
    mw_TVar();
    {
        VAL d2 = pop_value();
        mw_TYPE_PTR();
        mw_T1();
        push_value(d2);
    }
    mw_T1();
    mw_T__3E_();
    mw_PRIM_VALUE_GET();
    mw_prim_type();
    mw__21_();
    push_ptr("a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_PRIM_VALUE_SET();
    mw_prim_ctx();
    mw__21_();
    mw_TVar();
    mw_TYPE_PTR();
    mw_T2();
    mw_T0();
    mw_T__3E_();
    mw_PRIM_VALUE_SET();
    mw_prim_type();
    mw__21_();
    push_ptr("a\0\0\0\0");
    mw_name_new_21_();
    mw_var_new_implicit_21_();
    mw_TYPE_TYPE();
    mw_over();
    mw_var_type();
    mw__21_();
    mw_dup();
    mw_ctx_empty();
    mw_swap();
    mw_ctx_new_21_();
    mw_dup();
    mw_PRIM_MUT_NEW();
    mw_prim_ctx();
    mw__21_();
    mw_dup();
    mw_PRIM_MUT_GET();
    mw_prim_ctx();
    mw__21_();
    mw_dup();
    mw_PRIM_MUT_SET();
    mw_prim_ctx();
    mw__21_();
    mw_drop();
    mw_dup();
    mw_TVar();
    mw_dup();
    mw_TMut();
    {
        VAL d2 = pop_value();
        mw_T1();
        push_value(d2);
    }
    mw_T1();
    mw_T__3E_();
    mw_PRIM_MUT_NEW();
    mw_prim_type();
    mw__21_();
    mw_dup();
    mw_TVar();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__35);
    mw_prim_pack_cons();
    mw_sip();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_MUT_GET();
    mw_prim_type();
    mw__21_();
    mw_TVar();
    mw_dup();
    mw_TMut();
    push_u64(0);
    push_fnptr(&mb_init_prims_21__36);
    mw_prim_pack_cons();
    mw_sip();
    mw_T1();
    mw_T__3E_();
    mw_PRIM_MUT_SET();
    mw_prim_type();
    mw__21_();
}

static void mw_init_21_ (void){
    mw_init_paths_21_();
    mw_init_prims_21_();
    mw_init_types_21_();
}

static void mw_compile_21_ (void){
    push_ptr("Compiling \0\0\0\0");
    mw_str_trace_21_();
    mw_dup();
    mw_Path__3E_Str();
    mw_str_trace_ln_21_();
    mw_run_lexer_21_();
    push_ptr("Building.\0\0\0\0");
    mw_str_trace_ln_21_();
    mw_elab_module_21_();
    mw_drop();
    mw_typecheck_everything_21_();
    mw_num_errors();
    mw__40_();
    push_i64(0LL);
    mw__3E_();
    if (pop_u64()) {
        mw_num_errors();
        mw__40_();
        mw_int_trace_21_();
        push_ptr(" errors.\0\0\0\0");
        mw_str_trace_ln_21_();
        push_i64(1LL);
        mw_posix_exit_21_();
    } else {
        push_ptr("Done.\0\0\0\0");
        mw_str_trace_ln_21_();
    }
}

static void mw_main (void){
    mw_init_21_();
    push_i64(1LL);
    mw_argc();
    mw__3C_();
    if (pop_u64()) {
        push_i64(1LL);
        mw_argv();
        mw_ptr_40__40_();
        mw_Ptr__3E_Str();
        mw_Str__3E_Path();
        mw_compile_21_();
    } else {
        push_ptr("Expected at least one argument\0\0\0\0");
        mw_panic_21_();
    }
}


static void mb_Name_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Name_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Name_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Name_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Name_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Name_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Module_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Module_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Module_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Module_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Module_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Module_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Token_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Token_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Token_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Token_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Token_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Token_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Buffer_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Buffer_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Buffer_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Buffer_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Buffer_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Buffer_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_MetaVar_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_MetaVar_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_MetaVar_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_MetaVar_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_MetaVar_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_MetaVar_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Data_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Data_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Data_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Data_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Data_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Data_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Tag_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Tag_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Tag_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Tag_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Tag_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Tag_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Atom_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Atom_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Atom_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Atom_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Atom_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Atom_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Arrow_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Arrow_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Arrow_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Arrow_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Arrow_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Arrow_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Lambda_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Lambda_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Lambda_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Lambda_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Lambda_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Lambda_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Block_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Block_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Block_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Block_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Block_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Block_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Match_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Match_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Match_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Match_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Match_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Match_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Case_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Case_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Case_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Case_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Case_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Case_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Var_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Var_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Var_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Var_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Var_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Var_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Word_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Word_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Word_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Word_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Word_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Word_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Table_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Table_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Table_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Table_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Table_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Table_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Field_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Field_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Field_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Field_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Field_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Field_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_External_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_External_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_External_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_External_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_External_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_External_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_Variable_2E_pred_1 (void) {
    mw_prim_drop();
}

static void mb_Variable_2E_pred_2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_prim_int_sub();
}

static void mb_Variable_2E_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    mw_Variable_2E_NUM();
    mw_prim_int_get();
    mw_prim_value_le();
    decref(var_x);
}

static void mb_Variable_2E_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_unsafe_cast();
    push_value(var_x);
    incref(var_x);
    mw_prim_run();
    decref(var_x);
}

static void mb_Variable_2E_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_prim_dup();
    {
        VAL d2 = pop_value();
        mw_prim_unsafe_cast();
        push_value(var_x);
        incref(var_x);
        mw_prim_run();
        push_value(d2);
    }
    push_i64(1LL);
    mw_prim_int_add();
    decref(var_x);
}

static void mb_init_prims_21__1 (void) {
    mw_prim_drop();
    mw_elab_module_import_21_();
}

static void mb_init_prims_21__2 (void) {
    mw_prim_drop();
    mw_elab_def_21_();
}

static void mb_init_prims_21__3 (void) {
    mw_prim_drop();
    mw_elab_def_missing_21_();
}

static void mb_init_prims_21__4 (void) {
    mw_prim_drop();
    mw_elab_def_external_21_();
}

static void mb_init_prims_21__5 (void) {
    mw_prim_drop();
    mw_elab_def_type_21_();
}

static void mb_init_prims_21__6 (void) {
    mw_prim_drop();
    mw_elab_buffer_21_();
}

static void mb_init_prims_21__7 (void) {
    mw_prim_drop();
    mw_elab_variable_21_();
}

static void mb_init_prims_21__8 (void) {
    mw_prim_drop();
    mw_elab_table_21_();
}

static void mb_init_prims_21__9 (void) {
    mw_prim_drop();
    mw_elab_field_21_();
}

static void mb_init_prims_21__10 (void) {
    mw_prim_drop();
    mw_elab_data_21_();
}

static void mb_init_prims_21__11 (void) {
    mw_prim_drop();
    mw_elab_target_c99_21_();
}

static void mb_init_prims_21__12 (void) {
    mw_prim_drop();
    mw_elab_embed_str_21_();
}

static void mb_init_prims_21__13 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_init_prims_21__14 (void) {
    mw_prim_drop();
    mw_TVar();
    mw_T1();
}

static void mb_init_prims_21__15 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_init_prims_21__16 (void) {
    mw_prim_drop();
    mw_T_2A_();
}

static void mb_init_prims_21__17 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_init_prims_21__18 (void) {
    mw_prim_drop();
    mw_TYPE_INT();
    mw_T_2A_();
}

static void mb_init_prims_21__19 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_TVar();
        push_value(d2);
    }
    mw_TVar();
}

static void mb_init_prims_21__20 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_init_prims_21__21 (void) {
    mw_prim_drop();
    mw_dup2();
}

static void mb_init_prims_21__22 (void) {
    mw_prim_drop();
    mw_T_2A_();
}

static void mb_init_prims_21__23 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_T__3E_();
        push_value(d2);
    }
    mw_swap();
    mw_T_2A_();
}

static void mb_init_prims_21__24 (void) {
    mw_prim_drop();
    mw_T__3E_();
}

static void mb_init_prims_21__25 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_init_prims_21__26 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_TYPE_BOOL();
        mw_T_2A_();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_init_prims_21__28);
    mw_prim_pack_cons();
    mw_sip();
    mw_T_2A_();
}

static void mb_init_prims_21__27 (void) {
    mw_prim_drop();
    mw_TYPE_BOOL();
    mw_T_2A_();
}

static void mb_init_prims_21__28 (void) {
    mw_prim_drop();
    mw_T_2A_();
}

static void mb_init_prims_21__29 (void) {
    mw_prim_drop();
    mw_dup();
    mw_TYPE_BOOL();
    mw_T_2A_();
    mw_T__3E_();
    mw_T_2A_();
}

static void mb_init_prims_21__30 (void) {
    mw_prim_drop();
    mw_dup();
    mw_T__3E_();
    mw_T_2A_();
}

static void mb_init_prims_21__31 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_init_prims_21__32 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_init_prims_21__33 (void) {
    mw_prim_drop();
    mw_TYPE_PTR();
    mw_T1();
}

static void mb_init_prims_21__34 (void) {
    mw_prim_drop();
    mw_T1();
}

static void mb_init_prims_21__35 (void) {
    mw_prim_drop();
    mw_TMut();
    mw_T1();
}

static void mb_init_prims_21__36 (void) {
    mw_prim_drop();
    mw_T2();
}

static void mb_compile_21__1 (void) {
    mw_prim_drop();
    mw_num_errors();
    mw__40_();
    mw_int_trace_21_();
    push_ptr(" errors.\0\0\0\0");
    mw_str_trace_ln_21_();
    push_i64(1LL);
    mw_posix_exit_21_();
}

static void mb_compile_21__2 (void) {
    mw_prim_drop();
    push_ptr("Done.\0\0\0\0");
    mw_str_trace_ln_21_();
}

static void mb_run_lexer_21__1 (void) {
    mw_prim_drop();
    mw_lexer_done_3F_();
    mw_not();
}

static void mb_run_lexer_21__2 (void) {
    mw_prim_drop();
    mw_lexer_next_21_();
}

static void mb_elab_module_21__1 (void) {
    mw_prim_drop();
    mw_token_is_module_end_3F_();
    mw_not();
}

static void mb_elab_module_21__2 (void) {
    mw_prim_drop();
    mw_elab_module_decl_21_();
}

static void mb_typecheck_everything_21__1 (void) {
    mw_prim_drop();
    mw_name_def();
    mw__40_();
    mw_typecheck_def_21_();
}

static void mb_typecheck_everything_21__2 (void) {
    mw_prim_drop();
    mw_block_force_21_();
}

static void mb_main_1 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_argv();
    mw_ptr_40__40_();
    mw_Ptr__3E_Str();
    mw_Str__3E_Path();
    mw_compile_21_();
}

static void mb_main_2 (void) {
    mw_prim_drop();
    push_ptr("Expected at least one argument\0\0\0\0");
    mw_panic_21_();
}

static void mb_ptr_40__40__1 (void) {
    mw_prim_drop();
    mw_ptrs();
}

static void mb_ptr_40__40__2 (void) {
    mw_prim_drop();
    mw_ptr_40_();
}

static void mb_with_ptr_2B__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_prim_ptr_add();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_with_raw_ptr_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_Int__3E_OS_1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_OS_WINDOWS();
}

static void mb_Int__3E_OS_2 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(2LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_OS_LINUX();
    } else {
        mw_dup();
        push_i64(3LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            mw_OS_MACOS();
        } else {
            mw_drop();
            mw_OS_UNKNOWN();
        }
    }
}

static void mb_Int__3E_OS_3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_OS_LINUX();
}

static void mb_Int__3E_OS_4 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(3LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_OS_MACOS();
    } else {
        mw_drop();
        mw_OS_UNKNOWN();
    }
}

static void mb_Int__3E_OS_5 (void) {
    mw_prim_drop();
    mw_drop();
    mw_OS_MACOS();
}

static void mb_Int__3E_OS_6 (void) {
    mw_prim_drop();
    mw_drop();
    mw_OS_UNKNOWN();
}

static void mb_posix_open_21__1 (void) {
    mw_prim_drop();
    mw_Str__3E_Ptr();
}

static void mb_dip2_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    decref(var_f);
}

static void mb_dip2_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_rotr_1 (void) {
    mw_prim_drop();
    mw_swap();
}

static void mb_rotl_1 (void) {
    mw_prim_drop();
    mw_swap();
}

static void mb_over_1 (void) {
    mw_prim_drop();
    mw_dup();
}

static void mb_over2_1 (void) {
    mw_prim_drop();
    mw_over();
}

static void mb_over3_1 (void) {
    mw_prim_drop();
    mw_over2();
}

static void mb_tuck_1 (void) {
    mw_prim_drop();
    mw_swap();
}

static void mb_nip_1 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_dup3_1 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_dup2();
        push_value(d2);
    }
    mw_rotr();
}

static void mb_dup3_2 (void) {
    mw_prim_drop();
    mw_dup2();
}

static void mb_dip_3F__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_dip_27__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_dip3_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        {
            VAL d3 = pop_value();
            push_value(var_f);
            incref(var_f);
            mw_prim_run();
            push_value(d3);
        }
        push_value(d2);
    }
    decref(var_f);
}

static void mb_dip3_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    decref(var_f);
}

static void mb_dip3_4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_sip_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_sip2_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    decref(var_f);
}

static void mb_sip2_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_rot4r_1 (void) {
    mw_prim_drop();
    mw_rotr();
}

static void mb_rot4l_1 (void) {
    mw_prim_drop();
    mw_rotl();
}

static void mb_or_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_true();
    decref(var_f);
}

static void mb_or_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_and_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_and_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_false();
    decref(var_f);
}

static void mb_repeat_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_dup();
    push_i64(0LL);
    mw__3E_();
    decref(var_f);
}

static void mb_repeat_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    mw_1_();
    decref(var_f);
}

static void mb_repeat_4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_count_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_dup();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    mw_1_2B_();
    decref(var_f);
}

static void mb_count_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_countdown_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_dup();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    mw_1_();
    decref(var_f);
}

static void mb_countdown_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_cmp_1 (void) {
    mw_prim_drop();
    mw_drop2();
    mw_EQ();
}

static void mb_cmp_2 (void) {
    mw_prim_drop();
    mw__3C_();
    if (pop_u64()) {
        mw_LT();
    } else {
        mw_GT();
    }
}

static void mb_cmp_3 (void) {
    mw_prim_drop();
    mw_LT();
}

static void mb_cmp_4 (void) {
    mw_prim_drop();
    mw_GT();
}

static void mb_max_1 (void) {
    mw_prim_drop();
    mw_nip();
}

static void mb_max_2 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_min_1 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_min_2 (void) {
    mw_prim_drop();
    mw_nip();
}

static void mb_ptr_21__21__1 (void) {
    mw_prim_drop();
    mw_ptrs();
}

static void mb_ptr_21__21__2 (void) {
    mw_prim_drop();
    mw_ptr_21_();
}

static void mb_u8_40__40__1 (void) {
    mw_prim_drop();
    mw_u8_40_();
}

static void mb_u8_21__21__1 (void) {
    mw_prim_drop();
    mw_u8_21_();
}

static void mb_int_40__40__1 (void) {
    mw_prim_drop();
    mw_ints();
}

static void mb_int_40__40__2 (void) {
    mw_prim_drop();
    mw_int_40_();
}

static void mb_int_21__21__1 (void) {
    mw_prim_drop();
    mw_ints();
}

static void mb_int_21__21__2 (void) {
    mw_prim_drop();
    mw_int_21_();
}

static void mb_value_40__40__1 (void) {
    mw_prim_drop();
    mw_values();
}

static void mb_value_40__40__2 (void) {
    mw_prim_drop();
    mw_value_40_();
}

static void mb_value_21__21__1 (void) {
    mw_prim_drop();
    mw_values();
}

static void mb_value_21__21__2 (void) {
    mw_prim_drop();
    mw_value_21_();
}

static void mb_in_range_1 (void) {
    mw_prim_drop();
    mw_over();
    {
        VAL d2 = pop_value();
        mw__3E__3D_();
        push_value(d2);
    }
}

static void mb_in_range_2 (void) {
    mw_prim_drop();
    mw__3E__3D_();
}

static void mb_abs_1 (void) {
    mw_prim_drop();
    mw_negate();
}

static void mb_abs_2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_pack1_1 (void) {
    mw_prim_drop();
    mw_pack0();
}

static void mb_pack2_1 (void) {
    mw_prim_drop();
    mw_pack1();
}

static void mb_pack3_1 (void) {
    mw_prim_drop();
    mw_pack2();
}

static void mb_pack4_1 (void) {
    mw_prim_drop();
    mw_pack3();
}

static void mb_pack5_1 (void) {
    mw_prim_drop();
    mw_pack4();
}

static void mb_unpack2_1 (void) {
    mw_prim_drop();
    mw_unpack1();
}

static void mb_unpack3_1 (void) {
    mw_prim_drop();
    mw_unpack2();
}

static void mb_unpack4_1 (void) {
    mw_prim_drop();
    mw_unpack3();
}

static void mb_unpack5_1 (void) {
    mw_prim_drop();
    mw_unpack4();
}

static void mb_modify_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw__40_();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_file_21__1 (void) {
    mw_prim_drop();
    mw_File__3E_Int();
}

static void mb_str_write_21__1 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_Str__3E_Ptr();
        push_value(d2);
    }
    mw_posix_write_21_();
}

static void mb_str_write_21__2 (void) {
    mw_prim_drop();
    mw_Str__3E_Ptr();
}

static void mb_str_write_21__3 (void) {
    mw_prim_drop();
    push_ptr("error: write failed!\0\0\0\0");
    mw_panic_21_();
}

static void mb_str_write_21__4 (void) {
    mw_prim_drop();
    mw_swap();
    mw__3C_();
    if (pop_u64()) {
        push_ptr("error: write output fewer bytes than expected!\0\0\0\0");
        mw_panic_21_();
    } else {
        mw_id();
    }
}

static void mb_str_write_21__5 (void) {
    mw_prim_drop();
    push_ptr("error: write output fewer bytes than expected!\0\0\0\0");
    mw_panic_21_();
}

static void mb_str_write_21__6 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_str_buf_write_21__1 (void) {
    mw_prim_drop();
    push_ptr("error: str-buf write failed!\0\0\0\0");
    mw_panic_21_();
}

static void mb_str_buf_write_21__2 (void) {
    mw_prim_drop();
    mw_str_buf_length_3F_();
    mw__3C_();
    if (pop_u64()) {
        push_ptr("error: str-buf write wrote fewer bytes than expected!\0\0\0\0");
        mw_panic_21_();
    } else {
        mw_id();
    }
}

static void mb_str_buf_write_21__3 (void) {
    mw_prim_drop();
    push_ptr("error: str-buf write wrote fewer bytes than expected!\0\0\0\0");
    mw_panic_21_();
}

static void mb_str_buf_write_21__4 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_str_buf_read_21__1 (void) {
    mw_prim_drop();
    push_ptr("str-buf-read! failed\0\0\0\0");
    mw_panic_21_();
}

static void mb_str_buf_read_21__2 (void) {
    mw_prim_drop();
    mw_str_buf_length_21_();
}

static void mb_str_buf_length_21__1 (void) {
    mw_prim_drop();
    push_i64(0LL);
    mw_Int__3E_U8();
}

static void mb_print_char_21__1 (void) {
    mw_prim_drop();
    mw_str_buf_push_char_21_();
}

static void mb_build_str_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    mw_str_buf_dup_21_();
    decref(var_f);
}

static void mb_str_buf_push_char_21__1 (void) {
    mw_prim_drop();
    mw_char_21_();
}

static void mb_trace_char_21__1 (void) {
    mw_prim_drop();
    mw_str_buf_push_char_21_();
}

static void mb_int_write_21__1 (void) {
    mw_prim_drop();
    mw_str_buf_int_21_();
}

static void mb_str_buf_int_21__1 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("0\0\0\0\0");
    mw_str_buf_21_();
}

static void mb_str_buf_int_21__2 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_str_buf_int_21__3);
    mw_prim_pack_cons();
    mw_sip();
    push_i64(0LL);
    mw__3C_();
    if (pop_u64()) {
        push_i64(45LL);
        mw_Int__3E_Char();
        mw_str_buf_push_char_21_();
    } else {
        mw_id();
    }
    mw_str_buf_reverse_21_();
}

static void mb_str_buf_int_21__3 (void) {
    mw_prim_drop();
    mw_abs();
    mw_str_buf_clear_21_();
    while(1) {
        mw_dup();
        mw_0_3E_();
        if (!pop_u64()) break;
        mw_dup();
        mw_to_digit();
        mw_str_buf_push_char_21_();
        push_i64(10LL);
        mw__2F_();
    }
    mw_drop();
}

static void mb_str_buf_int_21__4 (void) {
    mw_prim_drop();
    mw_dup();
    mw_0_3E_();
}

static void mb_str_buf_int_21__5 (void) {
    mw_prim_drop();
    mw_dup();
    mw_to_digit();
    mw_str_buf_push_char_21_();
    push_i64(10LL);
    mw__2F_();
}

static void mb_str_buf_int_21__6 (void) {
    mw_prim_drop();
    push_i64(45LL);
    mw_Int__3E_Char();
    mw_str_buf_push_char_21_();
}

static void mb_str_buf_int_21__7 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_with_open_file_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    mw_drop();
    push_value(var_g);
    incref(var_g);
    mw_prim_run();
    decref(var_f);
    decref(var_g);
}

static void mb_with_open_file_21__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    mw_dup();
    {
        VAL d2 = pop_value();
        mw_Int__3E_File();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    mw_posix_close_21_();
    mw_drop();
    decref(var_f);
    decref(var_g);
}

static void mb_with_open_file_21__4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    mw_Int__3E_File();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
    decref(var_g);
}

static void mb_read_file_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_fp = pop_value();
    mw_prim_drop();
    mw_dup();
    push_i64(0LL);
    mw__3E_();
    decref(var_fp);
}

static void mb_read_file_21__3 (void) {
    mw_prim_pack_uncons();
    VAL var_fp = pop_value();
    mw_prim_drop();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw__2B_();
        mw_dup();
        push_value(d2);
    }
    mw_swap();
    push_i64(2LL);
    mw__2A_();
    mw_prim_ptr_realloc();
    mw_dup2();
    push_u64(0);
    push_value(var_fp);
    incref(var_fp);
    mw_prim_pack_cons();
    push_fnptr(&mb_read_file_21__5);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
    decref(var_fp);
}

static void mb_read_file_21__4 (void) {
    mw_prim_pack_uncons();
    VAL var_fp = pop_value();
    mw_prim_drop();
    mw__2B_();
    mw_dup();
    decref(var_fp);
}

static void mb_read_file_21__5 (void) {
    mw_prim_pack_uncons();
    VAL var_fp = pop_value();
    mw_prim_drop();
    mw_over2();
    push_u64(0);
    push_value(var_fp);
    incref(var_fp);
    mw_prim_pack_cons();
    push_fnptr(&mb_read_file_21__6);
    mw_prim_pack_cons();
    mw_dip2();
    mw_prim_posix_read();
    decref(var_fp);
}

static void mb_read_file_21__6 (void) {
    mw_prim_pack_uncons();
    VAL var_fp = pop_value();
    mw_prim_drop();
    push_value(var_fp);
    incref(var_fp);
    decref(var_fp);
}

static void mb_read_file_21__7 (void) {
    mw_prim_pack_uncons();
    VAL var_fp = pop_value();
    mw_prim_drop();
    push_ptr("io error while reading file\0\0\0\0");
    mw_panic_21_();
    decref(var_fp);
}

static void mb_read_file_21__8 (void) {
    mw_prim_pack_uncons();
    VAL var_fp = pop_value();
    mw_prim_drop();
    mw_id();
    decref(var_fp);
}

static void mb_read_file_21__9 (void) {
    mw_prim_pack_uncons();
    VAL var_fp = pop_value();
    mw_prim_drop();
    push_i64(0LL);
    mw_Int__3E_U32();
    mw_swap();
    mw_u32_21_();
    decref(var_fp);
}

static void mb_open_file_21__1 (void) {
    mw_prim_drop();
    push_ptr("Failed to open file!\0\0\0\0");
    mw_panic_21_();
}

static void mb_open_file_21__2 (void) {
    mw_prim_drop();
    mw_Int__3E_File();
}

static void mb_create_file_21__1 (void) {
    mw_prim_drop();
    push_ptr("Failed to create file!\0\0\0\0");
    mw_panic_21_();
}

static void mb_create_file_21__2 (void) {
    mw_prim_drop();
    mw_Int__3E_File();
}

static void mb_close_file_21__1 (void) {
    mw_prim_drop();
    push_ptr("failed to close file.\0\0\0\0");
    mw_panic_21_();
}

static void mb_close_file_21__2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_with_raw_path_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_is_directory_3F__1 (void) {
    mw_prim_drop();
    push_i64(256LL);
    mw_prim_ptr_alloc();
    push_u64(0);
    push_fnptr(&mb_is_directory_3F__2);
    mw_prim_pack_cons();
    mw_with_raw_ptr();
    mw_swap();
    push_i64(0LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_st_mode_40_();
        mw_S_ISDIR();
    } else {
        mw_drop();
        mw_false();
    }
}

static void mb_is_directory_3F__2 (void) {
    mw_prim_drop();
    mw_stat();
}

static void mb_is_directory_3F__3 (void) {
    mw_prim_drop();
    mw_st_mode_40_();
    mw_S_ISDIR();
}

static void mb_is_directory_3F__4 (void) {
    mw_prim_drop();
    mw_drop();
    mw_false();
}

static void mb_st_mode_40__4 (void) {
    mw_prim_drop();
    mw_u16_40_();
}

static void mb_char_40__1 (void) {
    mw_prim_drop();
    push_i64(-4203265827220226049LL);
}

static void mb_char_40__2 (void) {
    mw_prim_drop();
    push_i64(4294967295LL);
}

static void mb_str_tail_1 (void) {
    mw_prim_drop();
    mw_str_head_width();
}

static void mb_str_length_1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_1_2B_();
}

static void mb_str_for_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    while(1) {
        mw_str_is_empty_3F_();
        mw_not();
        if (!pop_u64()) break;
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_str_for_5);
        mw_prim_pack_cons();
        mw_sip();
        mw_str_tail();
    }
    decref(var_f);
}

static void mb_str_for_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_str_is_empty_3F_();
    mw_not();
    decref(var_f);
}

static void mb_str_for_4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_u64(0);
    push_value(var_f);
    incref(var_f);
    mw_prim_pack_cons();
    push_fnptr(&mb_str_for_5);
    mw_prim_pack_cons();
    mw_sip();
    mw_str_tail();
    decref(var_f);
}

static void mb_str_for_5 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_str_head();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_str_concat_1 (void) {
    mw_prim_drop();
    mw_str_size();
    mw__2B_();
}

static void mb_str_concat_2 (void) {
    mw_prim_drop();
    mw_prim_str_base();
    mw_swap();
    push_u64(0);
    push_fnptr(&mb_str_concat_3);
    mw_prim_pack_cons();
    mw_for();
    mw_nil();
    mw_swap();
    mw_u8_21_();
}

static void mb_str_concat_3 (void) {
    mw_prim_drop();
    mw_swap();
    mw_str_copy_partial_21_();
}

static void mb_for_5 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_for_7 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_for_8 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_for_10 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_for_2B_();
    decref(var_f);
}

static void mb_for_11 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_for_12 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_str_copy_partial_21__1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_Str__3E_Ptr();
    mw_swap();
    mw_str_size();
}

static void mb_str_copy_partial_21__2 (void) {
    mw_prim_drop();
    mw_prim_ptr_copy();
}

static void mb_str_is_empty_1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_true();
}

static void mb_str_is_empty_2 (void) {
    mw_prim_drop();
    mw_str_head();
    mw_is_nil();
}

static void mb_str_buf_dup_21__1 (void) {
    mw_prim_drop();
    mw_prim_str_base();
    mw_prim_ptr_copy();
}

static void mb_str_buf_char_40__1 (void) {
    mw_prim_drop();
    mw_char_40_();
}

static void mb_char_21__1 (void) {
    mw_prim_drop();
    mw_Char__3E_Int();
    mw_Int__3E_U32();
}

static void mb_char_width_1 (void) {
    mw_prim_drop();
    push_i64(4203265827220226048LL);
}

static void mb_str_buf_push_ptr_21__1 (void) {
    mw_prim_drop();
    mw_prim_ptr_copy();
}

static void mb_str_buf_reverse_21__1 (void) {
    mw_prim_drop();
    mw_dup2();
    mw__3C_();
}

static void mb_str_buf_reverse_21__2 (void) {
    mw_prim_drop();
    mw_dup2();
    mw_str_buf_swap_u8_21_();
    {
        VAL d2 = pop_value();
        mw_1_2B_();
        push_value(d2);
    }
    mw_1_();
}

static void mb_str_buf_reverse_21__3 (void) {
    mw_prim_drop();
    mw_1_2B_();
}

static void mb_int_show_1 (void) {
    mw_prim_drop();
    mw_str_buf_int_21_();
}

static void mb_str_buf_swap_u8_21__1 (void) {
    mw_prim_drop();
    mw_str_buf_u8_40_();
}

static void mb_str_buf_swap_u8_21__2 (void) {
    mw_prim_drop();
    mw_str_buf_u8_40_();
}

static void mb_str_transduce_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_u64(0);
    push_value(var_f);
    incref(var_f);
    mw_prim_pack_cons();
    push_fnptr(&mb_str_transduce_3);
    mw_prim_pack_cons();
    mw_build_str_21_();
    decref(var_f);
}

static void mb_str_transduce_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    while(1) {
        mw_str_is_empty_3F_();
        mw_not();
        if (!pop_u64()) break;
        push_u64(0);
        push_value(var_f);
        incref(var_f);
        mw_prim_pack_cons();
        push_fnptr(&mb_str_transduce_6);
        mw_prim_pack_cons();
        mw_sip();
        mw_str_tail();
        mw_swap();
        mw_str_transduce_step();
    }
    mw_drop();
    decref(var_f);
}

static void mb_str_transduce_4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_str_is_empty_3F_();
    mw_not();
    decref(var_f);
}

static void mb_str_transduce_5 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_u64(0);
    push_value(var_f);
    incref(var_f);
    mw_prim_pack_cons();
    push_fnptr(&mb_str_transduce_6);
    mw_prim_pack_cons();
    mw_sip();
    mw_str_tail();
    mw_swap();
    mw_str_transduce_step();
    decref(var_f);
}

static void mb_str_transduce_6 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_str_head();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_str_transduce_step_5 (void) {
    mw_prim_drop();
    mw_str_buf_push_char_21_();
}

static void mb_str_chars_1 (void) {
    mw_prim_drop();
    mw_L0();
}

static void mb_str_chars_2 (void) {
    mw_prim_drop();
    mw_snoc();
}

static void mb_str_codepoints_1 (void) {
    mw_prim_drop();
    mw_L0();
}

static void mb_str_codepoints_2 (void) {
    mw_prim_drop();
    mw_char_codepoint();
    mw_snoc();
}

static void mb_char_codepoint_1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_char_codepoint_1();
}

static void mb_char_codepoint_2 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(2LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_char_codepoint_2();
    } else {
        mw_dup();
        push_i64(3LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            mw_char_codepoint_3();
        } else {
            mw_drop();
            mw_char_codepoint_4();
        }
    }
}

static void mb_char_codepoint_3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_char_codepoint_2();
}

static void mb_char_codepoint_4 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(3LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_char_codepoint_3();
    } else {
        mw_drop();
        mw_char_codepoint_4();
    }
}

static void mb_char_codepoint_5 (void) {
    mw_prim_drop();
    mw_drop();
    mw_char_codepoint_3();
}

static void mb_char_codepoint_6 (void) {
    mw_prim_drop();
    mw_drop();
    mw_char_codepoint_4();
}

static void mb_str_bytes_1 (void) {
    mw_prim_drop();
    mw_Str__3E_Ptr();
    mw_swap();
    mw_str_size();
    while(1) {
        mw_dup();
        push_i64(0LL);
        mw__3E_();
        if (!pop_u64()) break;
        mw_1_();
        {
            VAL d3 = pop_value();
            push_u64(0);
            push_fnptr(&mb_str_bytes_5);
            mw_prim_pack_cons();
            mw_sip();
            mw_ptr_2B_();
            push_value(d3);
        }
    }
}

static void mb_str_bytes_2 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(0LL);
    mw__3E_();
}

static void mb_str_bytes_3 (void) {
    mw_prim_drop();
    mw_1_();
    {
        VAL d2 = pop_value();
        push_u64(0);
        push_fnptr(&mb_str_bytes_5);
        mw_prim_pack_cons();
        mw_sip();
        mw_ptr_2B_();
        push_value(d2);
    }
}

static void mb_str_bytes_4 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_str_bytes_5);
    mw_prim_pack_cons();
    mw_sip();
    mw_ptr_2B_();
}

static void mb_str_bytes_5 (void) {
    mw_prim_drop();
    mw_u8_40_();
    mw_snoc();
    push_i64(1LL);
}

static void mb_L4_2B__1 (void) {
    mw_prim_drop();
    mw_L2_2B_();
}

static void mb_L5_2B__1 (void) {
    mw_prim_drop();
    mw_L2_2B_();
}

static void mb_L6_2B__1 (void) {
    mw_prim_drop();
    mw_L3_2B_();
}

static void mb_L7_2B__1 (void) {
    mw_prim_drop();
    mw_L3_2B_();
}

static void mb_L8_2B__1 (void) {
    mw_prim_drop();
    mw_L3_2B_();
}

static void mb_L9_2B__1 (void) {
    mw_prim_drop();
    mw_L3_2B_();
}

static void mb_L10_2B__1 (void) {
    mw_prim_drop();
    mw_L5_2B_();
}

static void mb_L11_2B__1 (void) {
    mw_prim_drop();
    mw_L5_2B_();
}

static void mb_L12_2B__1 (void) {
    mw_prim_drop();
    mw_L6_2B_();
}

static void mb_len_6 (void) {
    mw_prim_drop();
    mw_drop2();
}

static void mb_len_2B__5 (void) {
    mw_prim_drop();
    mw_drop2();
}

static void mb_cons_2B__6 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_cons_2B__2B_();
        push_value(d2);
    }
    mw_rebalance_2B_();
}

static void mb_cons_2B__7 (void) {
    mw_prim_drop();
    mw_cons_2B__2B_();
}

static void mb_rebalance_2B__1 (void) {
    mw_prim_drop();
    mw_len_2B_();
}

static void mb_rebalance_2B__2 (void) {
    mw_prim_drop();
    mw_drop2();
    {
        VAL d2 = pop_value();
        mw_split_half_left();
        push_value(d2);
    }
    mw_cat__2B_();
    mw_rebalance_2B_();
}

static void mb_rebalance_2B__4 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_i64(3LL);
        mw__2A_();
        push_value(d2);
    }
    mw__3C_();
    if (pop_u64()) {
        mw_split_half_right();
        {
            VAL d3 = pop_value();
            mw_cat_2B__();
            push_value(d3);
        }
        mw_rebalance_2B_();
    } else {
        mw_id();
    }
}

static void mb_rebalance_2B__3 (void) {
    mw_prim_drop();
    mw_split_half_left();
}

static void mb_rebalance_2B__5 (void) {
    mw_prim_drop();
    push_i64(3LL);
    mw__2A_();
}

static void mb_rebalance_2B__6 (void) {
    mw_prim_drop();
    mw_split_half_right();
    {
        VAL d2 = pop_value();
        mw_cat_2B__();
        push_value(d2);
    }
    mw_rebalance_2B_();
}

static void mb_rebalance_2B__8 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_rebalance_2B__7 (void) {
    mw_prim_drop();
    mw_cat_2B__();
}

static void mb_snoc_2B__6 (void) {
    mw_prim_drop();
    mw_rotl();
    mw_snoc_2B__2B_();
    mw_rebalance_2B_();
}

static void mb_snoc_2B__2B__1 (void) {
    mw_prim_drop();
    mw_List_2B___3E_List();
}

static void mb_uncons_5 (void) {
    mw_prim_drop();
    mw_uncons();
}

static void mb_unsnoc_2 (void) {
    mw_prim_drop();
    mw_L0();
}

static void mb_unsnoc_4 (void) {
    mw_prim_drop();
    mw_L1();
}

static void mb_unsnoc_6 (void) {
    mw_prim_drop();
    mw_L2();
}

static void mb_unsnoc_8 (void) {
    mw_prim_drop();
    mw_cat_2B__();
    mw_List_2B___3E_List();
}

static void mb_cat_2B__7 (void) {
    mw_prim_drop();
    mw_L2_2B_();
}

static void mb_cat_2B__13 (void) {
    mw_prim_drop();
    mw_L3_2B_();
}

static void mb_cat_aux_1 (void) {
    mw_prim_drop();
    mw_len_2B_();
}

static void mb_split_half_left_2 (void) {
    mw_prim_drop();
    mw_L1_2B_();
}

static void mb_split_half_left_4 (void) {
    mw_prim_drop();
    mw_L1_2B_();
}

static void mb_split_half_left_6 (void) {
    mw_prim_drop();
    mw_L2_2B_();
}

static void mb_split_half_right_2 (void) {
    mw_prim_drop();
    mw_L0();
}

static void mb_split_half_right_4 (void) {
    mw_prim_drop();
    mw_L1();
}

static void mb_split_half_right_6 (void) {
    mw_prim_drop();
    mw_L1();
}

static void mb_split_half_right_8 (void) {
    mw_prim_drop();
    mw_List_2B___3E_List();
}

static void mb_split_half_3 (void) {
    mw_prim_drop();
    mw_L0();
}

static void mb_split_half_5 (void) {
    mw_prim_drop();
    mw_L1();
}

static void mb_split_half_7 (void) {
    mw_prim_drop();
    mw_L1();
}

static void mb_split_half_9 (void) {
    mw_prim_drop();
    mw_List_2B___3E_List();
}

static void mb_first_1 (void) {
    mw_prim_drop();
    mw_first_2B_();
}

static void mb_last_1 (void) {
    mw_prim_drop();
    mw_last_2B_();
}

static void mb_last_2B__3 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_last_2B__5 (void) {
    mw_prim_drop();
    mw_drop2();
}

static void mb_middle_1 (void) {
    mw_prim_drop();
    mw_middle_2B_();
}

static void mb_reverse_6 (void) {
    mw_prim_drop();
    mw_reverse_2B_();
    mw_swap();
    mw_reverse_2B_();
}

static void mb_reverse_2B__5 (void) {
    mw_prim_drop();
    mw_reverse_2B_();
    mw_swap();
    mw_reverse_2B_();
}

static void mb_map_5 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_6 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_8 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    decref(var_f);
}

static void mb_map_9 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_10 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    decref(var_f);
}

static void mb_map_11 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_12 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    decref(var_f);
}

static void mb_map_13 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_15 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_map_2B_();
        push_value(d2);
    }
    mw_swap();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_map_2B_();
        push_value(d2);
    }
    mw_swap();
    decref(var_f);
}

static void mb_map_16 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_map_2B_();
    decref(var_f);
}

static void mb_map_17 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_18 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_map_2B_();
    decref(var_f);
}

static void mb_map_19 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_2B__4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_2B__5 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_2B__7 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    decref(var_f);
}

static void mb_map_2B__8 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_2B__9 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    decref(var_f);
}

static void mb_map_2B__10 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_2B__11 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    decref(var_f);
}

static void mb_map_2B__12 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_2B__14 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_map_2B_();
        push_value(d2);
    }
    mw_swap();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_map_2B_();
        push_value(d2);
    }
    mw_swap();
    decref(var_f);
}

static void mb_map_2B__15 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_map_2B_();
    decref(var_f);
}

static void mb_map_2B__16 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_map_2B__17 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_map_2B_();
    decref(var_f);
}

static void mb_map_2B__18 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_for_2B__4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_for_2B__6 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_for_2B__7 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_for_2B__9 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_for_2B_();
    decref(var_f);
}

static void mb_for_2B__10 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_for_2B__11 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reverse_for_5 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reverse_for_7 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reverse_for_8 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reverse_for_10 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_reverse_for_2B_();
    decref(var_f);
}

static void mb_reverse_for_11 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reverse_for_12 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reverse_for_2B__4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reverse_for_2B__6 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reverse_for_2B__7 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reverse_for_2B__9 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_reverse_for_2B_();
    decref(var_f);
}

static void mb_reverse_for_2B__10 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reverse_for_2B__11 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reduce_2 (void) {
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    push_value(var_g);
    incref(var_g);
    mw_reduce_2B_();
    decref(var_g);
}

static void mb_reduce_3 (void) {
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    push_value(var_g);
    incref(var_g);
    mw_prim_run();
    decref(var_g);
}

static void mb_reduce_2B__6 (void) {
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    push_value(var_g);
    incref(var_g);
    mw_reduce_2B_();
    decref(var_g);
}

static void mb_reduce_2B__7 (void) {
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    push_value(var_g);
    incref(var_g);
    mw_prim_run();
    decref(var_g);
}

static void mb_reduce_2B__8 (void) {
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    push_value(var_g);
    incref(var_g);
    mw_prim_run();
    decref(var_g);
}

static void mb_filter_4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_filter_2B__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_filter_2B_();
    decref(var_f);
}

static void mb_filter_2B__4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_filter_2B__5 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_filter_2B_();
    decref(var_f);
}

static void mb_filter_2B__6 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_filter_2B__8 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_filter_2B__9 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_u64(0);
    push_value(var_f);
    incref(var_f);
    mw_prim_pack_cons();
    push_fnptr(&mb_filter_2B__10);
    mw_prim_pack_cons();
    mw_dip_27_();
    mw_cons();
    decref(var_f);
}

static void mb_filter_2B__12 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_nip();
    push_value(var_f);
    incref(var_f);
    mw_filter();
    decref(var_f);
}

static void mb_filter_2B__10 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_filter();
    decref(var_f);
}

static void mb_filter_2B__11 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_filter_2B__13 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_find_4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_find_2B__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_find_2B_();
    decref(var_f);
}

static void mb_find_2B__4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_find_2B__7 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_find_2B__9 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_find_2B__10 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_drop();
    mw_SOME();
    decref(var_f);
}

static void mb_find_2B__11 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_nip();
    push_value(var_f);
    incref(var_f);
    mw_find();
    decref(var_f);
}

static void mb_find_2B__12 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_find_3F__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_find();
    decref(var_f);
}

static void mb_find_3F__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reverse_find_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_reverse_find_3F__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_reverse_find();
    decref(var_f);
}

static void mb_reverse_find_3F__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_any_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_any_3F__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_all_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    mw_not();
    decref(var_f);
}

static void mb_all_3F__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    mw_not();
    decref(var_f);
}

static void mb_collect_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_collect_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_snoc();
        push_value(d2);
    }
    decref(var_f);
}

static void mb_collect_4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_snoc();
    decref(var_f);
}

static void mb_while_some_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    mw_is_some_3F_();
    decref(var_f);
    decref(var_g);
}

static void mb_while_some_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    mw_unwrap();
    push_value(var_g);
    incref(var_g);
    mw_prim_run();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
    decref(var_g);
}

static void mb_collect_while_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    mw_swap();
    decref(var_f);
    decref(var_g);
}

static void mb_collect_while_4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_g);
        incref(var_g);
        mw_prim_run();
        push_value(d2);
    }
    mw_swap();
    mw_snoc();
    decref(var_f);
    decref(var_g);
}

static void mb_collect_while_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
    decref(var_g);
}

static void mb_collect_while_5 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    push_value(var_g);
    incref(var_g);
    mw_prim_run();
    decref(var_f);
    decref(var_g);
}

static void mb_maybe_filter_4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_SOME();
    decref(var_f);
}

static void mb_maybe_filter_5 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_drop();
    mw_NONE();
    decref(var_f);
}

static void mb_char_bytes_1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_Char__3E_Int();
    mw_Int__3E_U8();
    mw_L1();
}

static void mb_char_bytes_2 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(2LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_Char__3E_Int();
        mw_dup();
        push_i64(8LL);
        mw__3E__3E_();
        mw_Int__3E_U8();
        {
            VAL d3 = pop_value();
            push_i64(255LL);
            mw__26_();
            mw_Int__3E_U8();
            push_value(d3);
        }
        mw_L2();
    } else {
        push_i64(3LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_Char__3E_Int();
            mw_dup();
            push_i64(16LL);
            mw__3E__3E_();
            mw_Int__3E_U8();
            {
                VAL d4 = pop_value();
                mw_dup();
                push_i64(8LL);
                mw__3E__3E_();
                push_i64(255LL);
                mw__26_();
                mw_Int__3E_U8();
                {
                    VAL d5 = pop_value();
                    push_i64(255LL);
                    mw__26_();
                    mw_Int__3E_U8();
                    push_value(d5);
                }
                push_value(d4);
            }
            mw_L3();
        } else {
            mw_Char__3E_Int();
            mw_dup();
            push_i64(24LL);
            mw__3E__3E_();
            mw_Int__3E_U8();
            {
                VAL d4 = pop_value();
                mw_dup();
                push_i64(16LL);
                mw__3E__3E_();
                push_i64(255LL);
                mw__26_();
                mw_Int__3E_U8();
                {
                    VAL d5 = pop_value();
                    mw_dup();
                    push_i64(8LL);
                    mw__3E__3E_();
                    push_i64(255LL);
                    mw__26_();
                    mw_Int__3E_U8();
                    {
                        VAL d6 = pop_value();
                        push_i64(255LL);
                        mw__26_();
                        mw_Int__3E_U8();
                        push_value(d6);
                    }
                    push_value(d5);
                }
                push_value(d4);
            }
            mw_L4();
        }
    }
}

static void mb_char_bytes_3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_Char__3E_Int();
    mw_dup();
    push_i64(8LL);
    mw__3E__3E_();
    mw_Int__3E_U8();
    {
        VAL d2 = pop_value();
        push_i64(255LL);
        mw__26_();
        mw_Int__3E_U8();
        push_value(d2);
    }
    mw_L2();
}

static void mb_char_bytes_5 (void) {
    mw_prim_drop();
    push_i64(3LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_Char__3E_Int();
        mw_dup();
        push_i64(16LL);
        mw__3E__3E_();
        mw_Int__3E_U8();
        {
            VAL d3 = pop_value();
            mw_dup();
            push_i64(8LL);
            mw__3E__3E_();
            push_i64(255LL);
            mw__26_();
            mw_Int__3E_U8();
            {
                VAL d4 = pop_value();
                push_i64(255LL);
                mw__26_();
                mw_Int__3E_U8();
                push_value(d4);
            }
            push_value(d3);
        }
        mw_L3();
    } else {
        mw_Char__3E_Int();
        mw_dup();
        push_i64(24LL);
        mw__3E__3E_();
        mw_Int__3E_U8();
        {
            VAL d3 = pop_value();
            mw_dup();
            push_i64(16LL);
            mw__3E__3E_();
            push_i64(255LL);
            mw__26_();
            mw_Int__3E_U8();
            {
                VAL d4 = pop_value();
                mw_dup();
                push_i64(8LL);
                mw__3E__3E_();
                push_i64(255LL);
                mw__26_();
                mw_Int__3E_U8();
                {
                    VAL d5 = pop_value();
                    push_i64(255LL);
                    mw__26_();
                    mw_Int__3E_U8();
                    push_value(d5);
                }
                push_value(d4);
            }
            push_value(d3);
        }
        mw_L4();
    }
}

static void mb_char_bytes_4 (void) {
    mw_prim_drop();
    push_i64(255LL);
    mw__26_();
    mw_Int__3E_U8();
}

static void mb_char_bytes_6 (void) {
    mw_prim_drop();
    mw_Char__3E_Int();
    mw_dup();
    push_i64(16LL);
    mw__3E__3E_();
    mw_Int__3E_U8();
    {
        VAL d2 = pop_value();
        mw_dup();
        push_i64(8LL);
        mw__3E__3E_();
        push_i64(255LL);
        mw__26_();
        mw_Int__3E_U8();
        {
            VAL d3 = pop_value();
            push_i64(255LL);
            mw__26_();
            mw_Int__3E_U8();
            push_value(d3);
        }
        push_value(d2);
    }
    mw_L3();
}

static void mb_char_bytes_9 (void) {
    mw_prim_drop();
    mw_Char__3E_Int();
    mw_dup();
    push_i64(24LL);
    mw__3E__3E_();
    mw_Int__3E_U8();
    {
        VAL d2 = pop_value();
        mw_dup();
        push_i64(16LL);
        mw__3E__3E_();
        push_i64(255LL);
        mw__26_();
        mw_Int__3E_U8();
        {
            VAL d3 = pop_value();
            mw_dup();
            push_i64(8LL);
            mw__3E__3E_();
            push_i64(255LL);
            mw__26_();
            mw_Int__3E_U8();
            {
                VAL d4 = pop_value();
                push_i64(255LL);
                mw__26_();
                mw_Int__3E_U8();
                push_value(d4);
            }
            push_value(d3);
        }
        push_value(d2);
    }
    mw_L4();
}

static void mb_char_bytes_7 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(8LL);
    mw__3E__3E_();
    push_i64(255LL);
    mw__26_();
    mw_Int__3E_U8();
    {
        VAL d2 = pop_value();
        push_i64(255LL);
        mw__26_();
        mw_Int__3E_U8();
        push_value(d2);
    }
}

static void mb_char_bytes_8 (void) {
    mw_prim_drop();
    push_i64(255LL);
    mw__26_();
    mw_Int__3E_U8();
}

static void mb_char_bytes_10 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(16LL);
    mw__3E__3E_();
    push_i64(255LL);
    mw__26_();
    mw_Int__3E_U8();
    {
        VAL d2 = pop_value();
        mw_dup();
        push_i64(8LL);
        mw__3E__3E_();
        push_i64(255LL);
        mw__26_();
        mw_Int__3E_U8();
        {
            VAL d3 = pop_value();
            push_i64(255LL);
            mw__26_();
            mw_Int__3E_U8();
            push_value(d3);
        }
        push_value(d2);
    }
}

static void mb_char_bytes_11 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(8LL);
    mw__3E__3E_();
    push_i64(255LL);
    mw__26_();
    mw_Int__3E_U8();
    {
        VAL d2 = pop_value();
        push_i64(255LL);
        mw__26_();
        mw_Int__3E_U8();
        push_value(d2);
    }
}

static void mb_char_bytes_12 (void) {
    mw_prim_drop();
    push_i64(255LL);
    mw__26_();
    mw_Int__3E_U8();
}

static void mb_char_valid_wobbly_1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_char_valid_1();
}

static void mb_char_valid_wobbly_2 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(2LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_char_valid_2();
    } else {
        mw_dup();
        push_i64(3LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            mw_char_valid_3_wobbly();
        } else {
            mw_drop();
            mw_char_valid_4();
        }
    }
}

static void mb_char_valid_wobbly_3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_char_valid_2();
}

static void mb_char_valid_wobbly_4 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(3LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_char_valid_3_wobbly();
    } else {
        mw_drop();
        mw_char_valid_4();
    }
}

static void mb_char_valid_wobbly_5 (void) {
    mw_prim_drop();
    mw_drop();
    mw_char_valid_3_wobbly();
}

static void mb_char_valid_wobbly_6 (void) {
    mw_prim_drop();
    mw_drop();
    mw_char_valid_4();
}

static void mb_char_codepoint_3_1 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(16128LL);
    mw__26_();
    push_i64(2LL);
    mw__3E__3E_();
    {
        VAL d2 = pop_value();
        push_i64(15LL);
        mw__26_();
        push_i64(12LL);
        mw__3C__3C_();
        push_value(d2);
    }
}

static void mb_char_codepoint_3_2 (void) {
    mw_prim_drop();
    push_i64(15LL);
    mw__26_();
    push_i64(12LL);
    mw__3C__3C_();
}

static void mb_char_valid_1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_char_valid_1();
}

static void mb_char_valid_2 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(2LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_char_valid_2();
    } else {
        mw_dup();
        push_i64(3LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            mw_char_valid_3();
        } else {
            mw_drop();
            mw_char_valid_4();
        }
    }
}

static void mb_char_valid_3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_char_valid_2();
}

static void mb_char_valid_4 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(3LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_char_valid_3();
    } else {
        mw_drop();
        mw_char_valid_4();
    }
}

static void mb_char_valid_5 (void) {
    mw_prim_drop();
    mw_drop();
    mw_char_valid_3();
}

static void mb_char_valid_6 (void) {
    mw_prim_drop();
    mw_drop();
    mw_char_valid_4();
}

static void mb_char_codepoint_4_1 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(4128768LL);
    mw__26_();
    push_i64(10LL);
    mw__3E__3E_();
    {
        VAL d2 = pop_value();
        mw_dup();
        push_i64(16128LL);
        mw__26_();
        push_i64(4LL);
        mw__3C__3C_();
        {
            VAL d3 = pop_value();
            push_i64(7LL);
            mw__26_();
            push_i64(18LL);
            mw__3C__3C_();
            push_value(d3);
        }
        push_value(d2);
    }
}

static void mb_char_codepoint_4_2 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(16128LL);
    mw__26_();
    push_i64(4LL);
    mw__3C__3C_();
    {
        VAL d2 = pop_value();
        push_i64(7LL);
        mw__26_();
        push_i64(18LL);
        mw__3C__3C_();
        push_value(d2);
    }
}

static void mb_char_codepoint_4_3 (void) {
    mw_prim_drop();
    push_i64(7LL);
    mw__26_();
    push_i64(18LL);
    mw__3C__3C_();
}

static void mb_char_codepoint_2_1 (void) {
    mw_prim_drop();
    push_i64(31LL);
    mw__26_();
    push_i64(6LL);
    mw__3C__3C_();
}

static void mb_char_21__precise_1 (void) {
    mw_prim_drop();
    mw_char_width_3F_();
}

static void mb_char_21__precise_2 (void) {
    mw_prim_drop();
    mw_drop();
    {
        VAL d2 = pop_value();
        mw_Char__3E_Int();
        mw_Int__3E_U8();
        push_value(d2);
    }
    mw_u8_21_();
}

static void mb_char_21__precise_4 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(2LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        {
            VAL d3 = pop_value();
            mw_Char__3E_Int();
            mw_Int__3E_U16();
            push_value(d3);
        }
        mw_u16_21_();
    } else {
        mw_dup();
        push_i64(3LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            {
                VAL d4 = pop_value();
                mw_Char__3E_Int();
                mw_dup();
                push_i64(65535LL);
                mw__26_();
                mw_Int__3E_U16();
                push_value(d4);
            }
            mw_dup();
            {
                VAL d4 = pop_value();
                mw_u16_21_();
                push_i64(16LL);
                mw__3E__3E_();
                mw_Int__3E_U8();
                push_i64(2LL);
                push_value(d4);
            }
            push_u64(0);
            push_fnptr(&mb_char_21__precise_11);
            mw_prim_pack_cons();
            mw_with_ptr_2B_();
        } else {
            mw_drop();
            {
                VAL d4 = pop_value();
                mw_Char__3E_Int();
                mw_Int__3E_U32();
                push_value(d4);
            }
            mw_u32_21_();
        }
    }
}

static void mb_char_21__precise_3 (void) {
    mw_prim_drop();
    mw_Char__3E_Int();
    mw_Int__3E_U8();
}

static void mb_char_21__precise_5 (void) {
    mw_prim_drop();
    mw_drop();
    {
        VAL d2 = pop_value();
        mw_Char__3E_Int();
        mw_Int__3E_U16();
        push_value(d2);
    }
    mw_u16_21_();
}

static void mb_char_21__precise_7 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(3LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        {
            VAL d3 = pop_value();
            mw_Char__3E_Int();
            mw_dup();
            push_i64(65535LL);
            mw__26_();
            mw_Int__3E_U16();
            push_value(d3);
        }
        mw_dup();
        {
            VAL d3 = pop_value();
            mw_u16_21_();
            push_i64(16LL);
            mw__3E__3E_();
            mw_Int__3E_U8();
            push_i64(2LL);
            push_value(d3);
        }
        push_u64(0);
        push_fnptr(&mb_char_21__precise_11);
        mw_prim_pack_cons();
        mw_with_ptr_2B_();
    } else {
        mw_drop();
        {
            VAL d3 = pop_value();
            mw_Char__3E_Int();
            mw_Int__3E_U32();
            push_value(d3);
        }
        mw_u32_21_();
    }
}

static void mb_char_21__precise_6 (void) {
    mw_prim_drop();
    mw_Char__3E_Int();
    mw_Int__3E_U16();
}

static void mb_char_21__precise_8 (void) {
    mw_prim_drop();
    mw_drop();
    {
        VAL d2 = pop_value();
        mw_Char__3E_Int();
        mw_dup();
        push_i64(65535LL);
        mw__26_();
        mw_Int__3E_U16();
        push_value(d2);
    }
    mw_dup();
    {
        VAL d2 = pop_value();
        mw_u16_21_();
        push_i64(16LL);
        mw__3E__3E_();
        mw_Int__3E_U8();
        push_i64(2LL);
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_char_21__precise_11);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mb_char_21__precise_12 (void) {
    mw_prim_drop();
    mw_drop();
    {
        VAL d2 = pop_value();
        mw_Char__3E_Int();
        mw_Int__3E_U32();
        push_value(d2);
    }
    mw_u32_21_();
}

static void mb_char_21__precise_9 (void) {
    mw_prim_drop();
    mw_Char__3E_Int();
    mw_dup();
    push_i64(65535LL);
    mw__26_();
    mw_Int__3E_U16();
}

static void mb_char_21__precise_10 (void) {
    mw_prim_drop();
    mw_u16_21_();
    push_i64(16LL);
    mw__3E__3E_();
    mw_Int__3E_U8();
    push_i64(2LL);
}

static void mb_char_21__precise_11 (void) {
    mw_prim_drop();
    mw_u8_21_();
}

static void mb_char_21__precise_13 (void) {
    mw_prim_drop();
    mw_Char__3E_Int();
    mw_Int__3E_U32();
}

static void mb_char_21__2B__2B__1 (void) {
    mw_prim_drop();
    mw_char_width();
}

static void mb_char_3F__2B__2B__1 (void) {
    mw_prim_drop();
    mw_ptr_2B_();
}

static void mb_is_whitespace_3F__1 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_i64(4295101952LL);
        push_value(d2);
    }
    mw__3E__3E_();
    push_i64(1LL);
    mw__26_();
    push_i64(0LL);
    mw__3C__3E_();
}

static void mb_is_whitespace_3F__2 (void) {
    mw_prim_drop();
    push_i64(4295101952LL);
}

static void mb_is_hexdigit_3F__1 (void) {
    mw_prim_drop();
    mw_is_upper_hexdigit_3F_();
}

static void mb_is_hexdigit_3F__2 (void) {
    mw_prim_drop();
    mw_is_lower_hexdigit_3F_();
}

static void mb_is_sign_3F__1 (void) {
    mw_prim_drop();
    mw_is_minus_3F_();
}

static void mb_is_string_end_3F__1 (void) {
    mw_prim_drop();
    push_i64(17179870209LL);
    mw_over();
    mw__3E__3E_();
    push_i64(1LL);
    mw__26_();
    push_i64(0LL);
    mw__3C__3E_();
}

static void mb_is_name_char_3F__1 (void) {
    mw_prim_drop();
    mw_is_special_char_3F_();
    mw_not();
}

static void mb_is_special_char_3F__1 (void) {
    mw_prim_drop();
    push_i64(288251318412247040LL);
    mw_swap();
    mw__3E__3E_();
}

static void mb_is_special_char_3F__2 (void) {
    mw_prim_drop();
    push_i64(64LL);
    mw__();
    mw_dup();
    push_i64(64LL);
    mw__3C_();
    if (pop_u64()) {
        push_i64(2882303762188206080LL);
        mw_swap();
        mw__3E__3E_();
    } else {
        mw_drop();
        push_i64(0LL);
    }
}

static void mb_is_special_char_3F__3 (void) {
    mw_prim_drop();
    push_i64(2882303762188206080LL);
    mw_swap();
    mw__3E__3E_();
}

static void mb_is_special_char_3F__4 (void) {
    mw_prim_drop();
    mw_drop();
    push_i64(0LL);
}

static void mb_path_21__1 (void) {
    mw_prim_drop();
    mw_Path__3E_Str();
    mw_Str__3E_Ptr();
}

static void mb_path_separator_1 (void) {
    mw_prim_drop();
    push_ptr("\\\0\0\0\0");
}

static void mb_path_separator_2 (void) {
    mw_prim_drop();
    push_ptr("/\0\0\0\0");
}

static void mb_path_join_1 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_path_join_2 (void) {
    mw_prim_drop();
    mw_Path__3E_Str();
    mw_path_separator();
    mw_rotl();
    mw_Path__3E_Str();
    mw_L3();
    mw_str_concat();
    mw_Str__3E_Path();
}

static void mb_input_fill_buffer_21__1 (void) {
    mw_prim_drop();
    mw_input_handle();
    mw__40_();
    mw_File__3E_Int();
    mw_INPUT_BUFFER();
    mw_INPUT_BUFFER_SIZE();
    mw_posix_read_21_();
    mw_dup();
    push_i64(0LL);
    mw__3E__3D_();
    if (pop_u64()) {
        mw_dup();
        push_i64(0LL);
        mw__3E_();
        if (pop_u64()) {
            mw_input_length();
            mw__21_();
            push_i64(0LL);
            mw_input_offset();
            mw__21_();
        } else {
            mw_drop();
            mw_input_end_21_();
        }
    } else {
        mw_drop();
        push_ptr("error: failed to read from file\0\0\0\0");
        mw_panic_21_();
    }
}

static void mb_input_fill_buffer_21__6 (void) {
    mw_prim_drop();
    push_ptr("error: attempted to fill input buffer when file is closed\0\0\0\0");
    mw_panic_21_();
}

static void mb_input_fill_buffer_21__2 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(0LL);
    mw__3E_();
    if (pop_u64()) {
        mw_input_length();
        mw__21_();
        push_i64(0LL);
        mw_input_offset();
        mw__21_();
    } else {
        mw_drop();
        mw_input_end_21_();
    }
}

static void mb_input_fill_buffer_21__5 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("error: failed to read from file\0\0\0\0");
    mw_panic_21_();
}

static void mb_input_fill_buffer_21__3 (void) {
    mw_prim_drop();
    mw_input_length();
    mw__21_();
    push_i64(0LL);
    mw_input_offset();
    mw__21_();
}

static void mb_input_fill_buffer_21__4 (void) {
    mw_prim_drop();
    mw_drop();
    mw_input_end_21_();
}

static void mb_input_end_21__1 (void) {
    mw_prim_drop();
    mw_input_handle();
    mw__40_();
    mw_close_file_21_();
}

static void mb_input_end_21__2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_input_peek_1 (void) {
    mw_prim_drop();
    mw_input_offset();
    mw__40_();
    mw_INPUT_BUFFER();
    push_u64(0);
    push_fnptr(&mb_input_peek_2);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mb_input_peek_3 (void) {
    mw_prim_drop();
    push_ptr("error: attempted to read input buffer when file is already closed\0\0\0\0");
    mw_panic_21_();
}

static void mb_input_peek_2 (void) {
    mw_prim_drop();
    mw_char_40_();
}

static void mb_input_move_21__1 (void) {
    mw_prim_drop();
    mw_input_offset();
    mw__40_();
    mw_dup();
    mw_INPUT_BUFFER();
    push_u64(0);
    push_fnptr(&mb_input_move_21__2);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
}

static void mb_input_move_21__3 (void) {
    mw_prim_drop();
    push_ptr("error: attempted to move input buffer when file is already closed\0\0\0\0");
    mw_panic_21_();
}

static void mb_input_move_21__2 (void) {
    mw_prim_drop();
    mw_char_40__width();
    mw__2B_();
    mw_input_offset();
    mw__21_();
    mw_input_prepare_for_more_21_();
}

static void mb_input_prepare_for_more_21__1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_input_length();
    mw__40_();
    mw__3E__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_input_fill_buffer_21_();
    } else {
        mw_dup();
        mw_INPUT_BUFFER();
        push_u64(0);
        push_fnptr(&mb_input_prepare_for_more_21__4);
        mw_prim_pack_cons();
        mw_with_ptr_2B_();
        mw__2B_();
        mw_input_length();
        mw__40_();
        mw__3E_();
        if (pop_u64()) {
            mw_input_fill_buffer_tragic_21_();
        } else {
            mw_id();
        }
    }
}

static void mb_input_prepare_for_more_21__7 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_input_prepare_for_more_21__2 (void) {
    mw_prim_drop();
    mw_drop();
    mw_input_fill_buffer_21_();
}

static void mb_input_prepare_for_more_21__3 (void) {
    mw_prim_drop();
    mw_dup();
    mw_INPUT_BUFFER();
    push_u64(0);
    push_fnptr(&mb_input_prepare_for_more_21__4);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
    mw__2B_();
    mw_input_length();
    mw__40_();
    mw__3E_();
    if (pop_u64()) {
        mw_input_fill_buffer_tragic_21_();
    } else {
        mw_id();
    }
}

static void mb_input_prepare_for_more_21__4 (void) {
    mw_prim_drop();
    mw_char_40__width();
}

static void mb_input_prepare_for_more_21__5 (void) {
    mw_prim_drop();
    mw_input_fill_buffer_tragic_21_();
}

static void mb_input_prepare_for_more_21__6 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_input_fill_buffer_tragic_21__1 (void) {
    mw_prim_drop();
    mw_u32_40_();
}

static void mb_input_fill_buffer_tragic_21__2 (void) {
    mw_prim_drop();
    mw_input_handle();
    mw__40_();
    mw_File__3E_Int();
    mw_input_length();
    mw__40_();
    mw_INPUT_BUFFER();
    push_u64(0);
    push_fnptr(&mb_input_fill_buffer_tragic_21__3);
    mw_prim_pack_cons();
    mw_with_ptr_2B_();
    mw_dup();
    push_i64(0LL);
    mw__3E__3D_();
    if (pop_u64()) {
        mw_dup();
        push_i64(0LL);
        mw__3E_();
        if (pop_u64()) {
            mw_input_length();
            push_u64(0);
            push_fnptr(&mb_input_fill_buffer_tragic_21__6);
            mw_prim_pack_cons();
            mw_modify();
        } else {
            mw_drop();
        }
    } else {
        mw_drop();
        push_ptr("error: failed to read from file\0\0\0\0");
        mw_panic_21_();
    }
}

static void mb_input_fill_buffer_tragic_21__9 (void) {
    mw_prim_drop();
    push_ptr("error: attempted to fill input buffer when file is closed\0\0\0\0");
    mw_panic_21_();
}

static void mb_input_fill_buffer_tragic_21__3 (void) {
    mw_prim_drop();
    mw_INPUT_BUFFER_SIZE();
    mw_posix_read_21_();
}

static void mb_input_fill_buffer_tragic_21__4 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(0LL);
    mw__3E_();
    if (pop_u64()) {
        mw_input_length();
        push_u64(0);
        push_fnptr(&mb_input_fill_buffer_tragic_21__6);
        mw_prim_pack_cons();
        mw_modify();
    } else {
        mw_drop();
    }
}

static void mb_input_fill_buffer_tragic_21__8 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("error: failed to read from file\0\0\0\0");
    mw_panic_21_();
}

static void mb_input_fill_buffer_tragic_21__5 (void) {
    mw_prim_drop();
    mw_input_length();
    push_u64(0);
    push_fnptr(&mb_input_fill_buffer_tragic_21__6);
    mw_prim_pack_cons();
    mw_modify();
}

static void mb_input_fill_buffer_tragic_21__7 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_input_fill_buffer_tragic_21__6 (void) {
    mw_prim_drop();
    mw__2B_();
}

static void mb_module_source_path_1 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("<generated>\0\0\0\0");
    mw_Str__3E_Path();
}

static void mb_module_source_path_2 (void) {
    mw_prim_drop();
    mw_module_path();
    mw__40_();
    mw_make_source_path();
}

static void mb_lexer_next_21__1 (void) {
    mw_prim_drop();
    push_ptr("invalid character\0\0\0\0");
    mw_lexer_emit_fatal_error_21_();
}

static void mb_lexer_next_21__2 (void) {
    mw_prim_drop();
    mw_is_name_char_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_lexer_emit_name_21_();
    } else {
        mw_is_newline_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_lexer_newline_21_();
        } else {
            mw_is_whitespace_3F_();
            if (pop_u64()) {
                mw_drop();
            } else {
                mw_is_pound_3F_();
                if (pop_u64()) {
                    mw_drop();
                    mw_lexer_skip_comment_21_();
                } else {
                    mw_is_comma_3F_();
                    if (pop_u64()) {
                        mw_drop();
                        mw_TOKEN_COMMA();
                        mw_lexer_emit_21_();
                    } else {
                        mw_is_lparen_3F_();
                        if (pop_u64()) {
                            mw_drop();
                            mw_lexer_emit_lparen_21_();
                        } else {
                            mw_is_rparen_3F_();
                            if (pop_u64()) {
                                mw_drop();
                                mw_lexer_emit_rparen_21_();
                            } else {
                                mw_is_lsquare_3F_();
                                if (pop_u64()) {
                                    mw_drop();
                                    mw_lexer_emit_lsquare_21_();
                                } else {
                                    mw_is_rsquare_3F_();
                                    if (pop_u64()) {
                                        mw_drop();
                                        mw_lexer_emit_rsquare_21_();
                                    } else {
                                        mw_is_lcurly_3F_();
                                        if (pop_u64()) {
                                            mw_drop();
                                            mw_lexer_emit_lcurly_21_();
                                        } else {
                                            mw_is_rcurly_3F_();
                                            if (pop_u64()) {
                                                mw_drop();
                                                mw_lexer_emit_rcurly_21_();
                                            } else {
                                                mw_is_quote_3F_();
                                                if (pop_u64()) {
                                                    mw_drop();
                                                    mw_lexer_emit_string_21_();
                                                } else {
                                                    push_ptr("unrecognized token\0\0\0\0");
                                                    mw_lexer_emit_fatal_error_21_();
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        mw_lexer_move_21_();
    }
}

static void mb_lexer_next_21__3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_lexer_emit_name_21_();
}

static void mb_lexer_next_21__4 (void) {
    mw_prim_drop();
    mw_is_newline_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_lexer_newline_21_();
    } else {
        mw_is_whitespace_3F_();
        if (pop_u64()) {
            mw_drop();
        } else {
            mw_is_pound_3F_();
            if (pop_u64()) {
                mw_drop();
                mw_lexer_skip_comment_21_();
            } else {
                mw_is_comma_3F_();
                if (pop_u64()) {
                    mw_drop();
                    mw_TOKEN_COMMA();
                    mw_lexer_emit_21_();
                } else {
                    mw_is_lparen_3F_();
                    if (pop_u64()) {
                        mw_drop();
                        mw_lexer_emit_lparen_21_();
                    } else {
                        mw_is_rparen_3F_();
                        if (pop_u64()) {
                            mw_drop();
                            mw_lexer_emit_rparen_21_();
                        } else {
                            mw_is_lsquare_3F_();
                            if (pop_u64()) {
                                mw_drop();
                                mw_lexer_emit_lsquare_21_();
                            } else {
                                mw_is_rsquare_3F_();
                                if (pop_u64()) {
                                    mw_drop();
                                    mw_lexer_emit_rsquare_21_();
                                } else {
                                    mw_is_lcurly_3F_();
                                    if (pop_u64()) {
                                        mw_drop();
                                        mw_lexer_emit_lcurly_21_();
                                    } else {
                                        mw_is_rcurly_3F_();
                                        if (pop_u64()) {
                                            mw_drop();
                                            mw_lexer_emit_rcurly_21_();
                                        } else {
                                            mw_is_quote_3F_();
                                            if (pop_u64()) {
                                                mw_drop();
                                                mw_lexer_emit_string_21_();
                                            } else {
                                                push_ptr("unrecognized token\0\0\0\0");
                                                mw_lexer_emit_fatal_error_21_();
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    mw_lexer_move_21_();
}

static void mb_lexer_next_21__5 (void) {
    mw_prim_drop();
    mw_drop();
    mw_lexer_newline_21_();
}

static void mb_lexer_next_21__6 (void) {
    mw_prim_drop();
    mw_is_whitespace_3F_();
    if (pop_u64()) {
        mw_drop();
    } else {
        mw_is_pound_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_lexer_skip_comment_21_();
        } else {
            mw_is_comma_3F_();
            if (pop_u64()) {
                mw_drop();
                mw_TOKEN_COMMA();
                mw_lexer_emit_21_();
            } else {
                mw_is_lparen_3F_();
                if (pop_u64()) {
                    mw_drop();
                    mw_lexer_emit_lparen_21_();
                } else {
                    mw_is_rparen_3F_();
                    if (pop_u64()) {
                        mw_drop();
                        mw_lexer_emit_rparen_21_();
                    } else {
                        mw_is_lsquare_3F_();
                        if (pop_u64()) {
                            mw_drop();
                            mw_lexer_emit_lsquare_21_();
                        } else {
                            mw_is_rsquare_3F_();
                            if (pop_u64()) {
                                mw_drop();
                                mw_lexer_emit_rsquare_21_();
                            } else {
                                mw_is_lcurly_3F_();
                                if (pop_u64()) {
                                    mw_drop();
                                    mw_lexer_emit_lcurly_21_();
                                } else {
                                    mw_is_rcurly_3F_();
                                    if (pop_u64()) {
                                        mw_drop();
                                        mw_lexer_emit_rcurly_21_();
                                    } else {
                                        mw_is_quote_3F_();
                                        if (pop_u64()) {
                                            mw_drop();
                                            mw_lexer_emit_string_21_();
                                        } else {
                                            push_ptr("unrecognized token\0\0\0\0");
                                            mw_lexer_emit_fatal_error_21_();
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

static void mb_lexer_next_21__7 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_lexer_next_21__8 (void) {
    mw_prim_drop();
    mw_is_pound_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_lexer_skip_comment_21_();
    } else {
        mw_is_comma_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_TOKEN_COMMA();
            mw_lexer_emit_21_();
        } else {
            mw_is_lparen_3F_();
            if (pop_u64()) {
                mw_drop();
                mw_lexer_emit_lparen_21_();
            } else {
                mw_is_rparen_3F_();
                if (pop_u64()) {
                    mw_drop();
                    mw_lexer_emit_rparen_21_();
                } else {
                    mw_is_lsquare_3F_();
                    if (pop_u64()) {
                        mw_drop();
                        mw_lexer_emit_lsquare_21_();
                    } else {
                        mw_is_rsquare_3F_();
                        if (pop_u64()) {
                            mw_drop();
                            mw_lexer_emit_rsquare_21_();
                        } else {
                            mw_is_lcurly_3F_();
                            if (pop_u64()) {
                                mw_drop();
                                mw_lexer_emit_lcurly_21_();
                            } else {
                                mw_is_rcurly_3F_();
                                if (pop_u64()) {
                                    mw_drop();
                                    mw_lexer_emit_rcurly_21_();
                                } else {
                                    mw_is_quote_3F_();
                                    if (pop_u64()) {
                                        mw_drop();
                                        mw_lexer_emit_string_21_();
                                    } else {
                                        push_ptr("unrecognized token\0\0\0\0");
                                        mw_lexer_emit_fatal_error_21_();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

static void mb_lexer_next_21__9 (void) {
    mw_prim_drop();
    mw_drop();
    mw_lexer_skip_comment_21_();
}

static void mb_lexer_next_21__10 (void) {
    mw_prim_drop();
    mw_is_comma_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_TOKEN_COMMA();
        mw_lexer_emit_21_();
    } else {
        mw_is_lparen_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_lexer_emit_lparen_21_();
        } else {
            mw_is_rparen_3F_();
            if (pop_u64()) {
                mw_drop();
                mw_lexer_emit_rparen_21_();
            } else {
                mw_is_lsquare_3F_();
                if (pop_u64()) {
                    mw_drop();
                    mw_lexer_emit_lsquare_21_();
                } else {
                    mw_is_rsquare_3F_();
                    if (pop_u64()) {
                        mw_drop();
                        mw_lexer_emit_rsquare_21_();
                    } else {
                        mw_is_lcurly_3F_();
                        if (pop_u64()) {
                            mw_drop();
                            mw_lexer_emit_lcurly_21_();
                        } else {
                            mw_is_rcurly_3F_();
                            if (pop_u64()) {
                                mw_drop();
                                mw_lexer_emit_rcurly_21_();
                            } else {
                                mw_is_quote_3F_();
                                if (pop_u64()) {
                                    mw_drop();
                                    mw_lexer_emit_string_21_();
                                } else {
                                    push_ptr("unrecognized token\0\0\0\0");
                                    mw_lexer_emit_fatal_error_21_();
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

static void mb_lexer_next_21__11 (void) {
    mw_prim_drop();
    mw_drop();
    mw_TOKEN_COMMA();
    mw_lexer_emit_21_();
}

static void mb_lexer_next_21__12 (void) {
    mw_prim_drop();
    mw_is_lparen_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_lexer_emit_lparen_21_();
    } else {
        mw_is_rparen_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_lexer_emit_rparen_21_();
        } else {
            mw_is_lsquare_3F_();
            if (pop_u64()) {
                mw_drop();
                mw_lexer_emit_lsquare_21_();
            } else {
                mw_is_rsquare_3F_();
                if (pop_u64()) {
                    mw_drop();
                    mw_lexer_emit_rsquare_21_();
                } else {
                    mw_is_lcurly_3F_();
                    if (pop_u64()) {
                        mw_drop();
                        mw_lexer_emit_lcurly_21_();
                    } else {
                        mw_is_rcurly_3F_();
                        if (pop_u64()) {
                            mw_drop();
                            mw_lexer_emit_rcurly_21_();
                        } else {
                            mw_is_quote_3F_();
                            if (pop_u64()) {
                                mw_drop();
                                mw_lexer_emit_string_21_();
                            } else {
                                push_ptr("unrecognized token\0\0\0\0");
                                mw_lexer_emit_fatal_error_21_();
                            }
                        }
                    }
                }
            }
        }
    }
}

static void mb_lexer_next_21__13 (void) {
    mw_prim_drop();
    mw_drop();
    mw_lexer_emit_lparen_21_();
}

static void mb_lexer_next_21__14 (void) {
    mw_prim_drop();
    mw_is_rparen_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_lexer_emit_rparen_21_();
    } else {
        mw_is_lsquare_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_lexer_emit_lsquare_21_();
        } else {
            mw_is_rsquare_3F_();
            if (pop_u64()) {
                mw_drop();
                mw_lexer_emit_rsquare_21_();
            } else {
                mw_is_lcurly_3F_();
                if (pop_u64()) {
                    mw_drop();
                    mw_lexer_emit_lcurly_21_();
                } else {
                    mw_is_rcurly_3F_();
                    if (pop_u64()) {
                        mw_drop();
                        mw_lexer_emit_rcurly_21_();
                    } else {
                        mw_is_quote_3F_();
                        if (pop_u64()) {
                            mw_drop();
                            mw_lexer_emit_string_21_();
                        } else {
                            push_ptr("unrecognized token\0\0\0\0");
                            mw_lexer_emit_fatal_error_21_();
                        }
                    }
                }
            }
        }
    }
}

static void mb_lexer_next_21__15 (void) {
    mw_prim_drop();
    mw_drop();
    mw_lexer_emit_rparen_21_();
}

static void mb_lexer_next_21__16 (void) {
    mw_prim_drop();
    mw_is_lsquare_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_lexer_emit_lsquare_21_();
    } else {
        mw_is_rsquare_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_lexer_emit_rsquare_21_();
        } else {
            mw_is_lcurly_3F_();
            if (pop_u64()) {
                mw_drop();
                mw_lexer_emit_lcurly_21_();
            } else {
                mw_is_rcurly_3F_();
                if (pop_u64()) {
                    mw_drop();
                    mw_lexer_emit_rcurly_21_();
                } else {
                    mw_is_quote_3F_();
                    if (pop_u64()) {
                        mw_drop();
                        mw_lexer_emit_string_21_();
                    } else {
                        push_ptr("unrecognized token\0\0\0\0");
                        mw_lexer_emit_fatal_error_21_();
                    }
                }
            }
        }
    }
}

static void mb_lexer_next_21__17 (void) {
    mw_prim_drop();
    mw_drop();
    mw_lexer_emit_lsquare_21_();
}

static void mb_lexer_next_21__18 (void) {
    mw_prim_drop();
    mw_is_rsquare_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_lexer_emit_rsquare_21_();
    } else {
        mw_is_lcurly_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_lexer_emit_lcurly_21_();
        } else {
            mw_is_rcurly_3F_();
            if (pop_u64()) {
                mw_drop();
                mw_lexer_emit_rcurly_21_();
            } else {
                mw_is_quote_3F_();
                if (pop_u64()) {
                    mw_drop();
                    mw_lexer_emit_string_21_();
                } else {
                    push_ptr("unrecognized token\0\0\0\0");
                    mw_lexer_emit_fatal_error_21_();
                }
            }
        }
    }
}

static void mb_lexer_next_21__19 (void) {
    mw_prim_drop();
    mw_drop();
    mw_lexer_emit_rsquare_21_();
}

static void mb_lexer_next_21__20 (void) {
    mw_prim_drop();
    mw_is_lcurly_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_lexer_emit_lcurly_21_();
    } else {
        mw_is_rcurly_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_lexer_emit_rcurly_21_();
        } else {
            mw_is_quote_3F_();
            if (pop_u64()) {
                mw_drop();
                mw_lexer_emit_string_21_();
            } else {
                push_ptr("unrecognized token\0\0\0\0");
                mw_lexer_emit_fatal_error_21_();
            }
        }
    }
}

static void mb_lexer_next_21__21 (void) {
    mw_prim_drop();
    mw_drop();
    mw_lexer_emit_lcurly_21_();
}

static void mb_lexer_next_21__22 (void) {
    mw_prim_drop();
    mw_is_rcurly_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_lexer_emit_rcurly_21_();
    } else {
        mw_is_quote_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_lexer_emit_string_21_();
        } else {
            push_ptr("unrecognized token\0\0\0\0");
            mw_lexer_emit_fatal_error_21_();
        }
    }
}

static void mb_lexer_next_21__23 (void) {
    mw_prim_drop();
    mw_drop();
    mw_lexer_emit_rcurly_21_();
}

static void mb_lexer_next_21__24 (void) {
    mw_prim_drop();
    mw_is_quote_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_lexer_emit_string_21_();
    } else {
        push_ptr("unrecognized token\0\0\0\0");
        mw_lexer_emit_fatal_error_21_();
    }
}

static void mb_lexer_next_21__25 (void) {
    mw_prim_drop();
    mw_drop();
    mw_lexer_emit_string_21_();
}

static void mb_lexer_next_21__26 (void) {
    mw_prim_drop();
    push_ptr("unrecognized token\0\0\0\0");
    mw_lexer_emit_fatal_error_21_();
}

static void mb_stack_pop_21__1 (void) {
    mw_prim_drop();
    mw_stack_uncons();
}

static void mb_emit_fatal_error_21__1 (void) {
    mw_prim_drop();
    mw_token_location();
}

static void mb_lexer_emit_fatal_error_21__1 (void) {
    mw_prim_drop();
    mw_lexer_location();
}

static void mb_lexer_emit_name_21__1 (void) {
    mw_prim_drop();
    mw_is_name_char_3F_();
}

static void mb_lexer_emit_name_21__2 (void) {
    mw_prim_drop();
    mw_char_valid_3F_();
    if (pop_u64()) {
        mw_str_buf_push_char_21_();
        mw_lexer_move_21_();
        mw_lexer_peek();
    } else {
        push_ptr("invalid character\0\0\0\0");
        mw_lexer_emit_fatal_error_21_();
    }
}

static void mb_lexer_emit_name_21__3 (void) {
    mw_prim_drop();
    mw_str_buf_push_char_21_();
    mw_lexer_move_21_();
    mw_lexer_peek();
}

static void mb_lexer_emit_name_21__4 (void) {
    mw_prim_drop();
    push_ptr("invalid character\0\0\0\0");
    mw_lexer_emit_fatal_error_21_();
}

static void mb_lexer_emit_name_21__5 (void) {
    mw_prim_drop();
    mw_drop3();
    mw_lexer_skip_doc_21_();
}

static void mb_lexer_emit_name_21__6 (void) {
    mw_prim_drop();
    mw_str_buf_is_int_3F_();
    if (pop_u64()) {
        mw_str_buf_int_3F_();
        mw_TOKEN_INT();
    } else {
        mw_str_buf_dup_21_();
        mw_name_new_21_();
        mw_TOKEN_NAME();
    }
    mw_token_alloc_21_();
    mw_tuck();
    mw_token_value();
    mw__21_();
    mw_tuck();
    mw_token_col();
    mw__21_();
    mw_tuck();
    mw_token_row();
    mw__21_();
    mw_token_module();
    mw__21_();
}

static void mb_lexer_emit_name_21__7 (void) {
    mw_prim_drop();
    mw_str_buf_int_3F_();
    mw_TOKEN_INT();
}

static void mb_lexer_emit_name_21__8 (void) {
    mw_prim_drop();
    mw_str_buf_dup_21_();
    mw_name_new_21_();
    mw_TOKEN_NAME();
}

static void mb_lexer_newline_21__1 (void) {
    mw_prim_drop();
    mw_Row__3E_Int();
    mw_1_2B_();
    mw_Int__3E_Row();
}

static void mb_lexer_skip_comment_21__1 (void) {
    mw_prim_drop();
    mw_lexer_comment_end_3F_();
    mw_not();
}

static void mb_lexer_skip_comment_21__2 (void) {
    mw_prim_drop();
    mw_lexer_move_21_();
}

static void mb_lexer_skip_comment_21__3 (void) {
    mw_prim_drop();
    mw_lexer_newline_21_();
}

static void mb_lexer_skip_comment_21__4 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_lexer_emit_rparen_21__3 (void) {
    mw_prim_drop();
    mw_dup();
    mw_TOKEN_RPAREN();
    mw_lexer_make_21_();
    mw_TOKEN_LPAREN();
    mw_swap();
    mw_token_value();
    mw__21_();
}

static void mb_lexer_emit_rparen_21__4 (void) {
    mw_prim_drop();
    push_ptr("Mismatched right parenthesis.\0\0\0\0");
    mw_lexer_emit_fatal_error_21_();
}

static void mb_lexer_emit_rsquare_21__3 (void) {
    mw_prim_drop();
    mw_dup();
    mw_TOKEN_RSQUARE();
    mw_lexer_make_21_();
    mw_TOKEN_LSQUARE();
    mw_swap();
    mw_token_value();
    mw__21_();
}

static void mb_lexer_emit_rsquare_21__4 (void) {
    mw_prim_drop();
    push_ptr("Mismatched right bracket.\0\0\0\0");
    mw_lexer_emit_fatal_error_21_();
}

static void mb_lexer_emit_rcurly_21__3 (void) {
    mw_prim_drop();
    mw_dup();
    mw_TOKEN_RCURLY();
    mw_lexer_make_21_();
    mw_TOKEN_LCURLY();
    mw_swap();
    mw_token_value();
    mw__21_();
}

static void mb_lexer_emit_rcurly_21__4 (void) {
    mw_prim_drop();
    push_ptr("Mismatched right brace.\0\0\0\0");
    mw_lexer_emit_fatal_error_21_();
}

static void mb_lexer_emit_string_21__1 (void) {
    mw_prim_drop();
    mw_is_string_end_3F_();
    mw_not();
}

static void mb_lexer_emit_string_21__2 (void) {
    mw_prim_drop();
    mw_char_valid_3F_();
    if (pop_u64()) {
        mw_lexer_push_string_char_21_();
        mw_lexer_move_21_();
        mw_lexer_peek();
    } else {
        push_ptr("invalid character in string literal\0\0\0\0");
        mw_lexer_emit_fatal_error_21_();
    }
}

static void mb_lexer_emit_string_21__3 (void) {
    mw_prim_drop();
    mw_lexer_push_string_char_21_();
    mw_lexer_move_21_();
    mw_lexer_peek();
}

static void mb_lexer_emit_string_21__4 (void) {
    mw_prim_drop();
    push_ptr("invalid character in string literal\0\0\0\0");
    mw_lexer_emit_fatal_error_21_();
}

static void mb_lexer_move_21__1 (void) {
    mw_prim_drop();
    mw_Col__3E_Int();
    mw_1_2B_();
    mw_Int__3E_Col();
}

static void mb_stack_push_21__1 (void) {
    mw_prim_drop();
    mw_STACK_CONS();
}

static void mb_str_buf_is_doc_start_3F__1 (void) {
    mw_prim_drop();
    push_i64(0LL);
    mw_str_buf_char_40_();
    mw_is_pipe_3F_();
    mw_nip();
    push_u64(0);
    push_fnptr(&mb_str_buf_is_doc_start_3F__2);
    mw_prim_pack_cons();
    mw_and();
    push_u64(0);
    push_fnptr(&mb_str_buf_is_doc_start_3F__3);
    mw_prim_pack_cons();
    mw_and();
}

static void mb_str_buf_is_doc_start_3F__2 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_str_buf_char_40_();
    mw_is_pipe_3F_();
    mw_nip();
}

static void mb_str_buf_is_doc_start_3F__3 (void) {
    mw_prim_drop();
    push_i64(2LL);
    mw_str_buf_char_40_();
    mw_is_pipe_3F_();
    mw_nip();
}

static void mb_lexer_skip_doc_21__1 (void) {
    mw_prim_drop();
    mw_lexer_comment_end_3F_();
    mw_not();
}

static void mb_lexer_skip_doc_21__2 (void) {
    mw_prim_drop();
    mw_lexer_move_21_();
}

static void mb_str_buf_is_int_3F__1 (void) {
    mw_prim_drop();
    mw_true();
}

static void mb_str_buf_is_int_3F__2 (void) {
    mw_prim_drop();
    mw_str_buf_is_hex_int_3F_();
}

static void mb_str_buf_int_3F__1 (void) {
    mw_prim_drop();
    mw_str_buf_dec_int_3F_();
}

static void mb_str_buf_int_3F__2 (void) {
    mw_prim_drop();
    mw_str_buf_hex_int_3F_();
}

static void mb_name_new_21__1 (void) {
    mw_prim_drop();
    mw_name_keep_going_3F_();
}

static void mb_name_new_21__2 (void) {
    mw_prim_drop();
    mw_next_hash();
}

static void mb_name_new_21__3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_Name_2E_alloc_21_();
    mw_tuck();
    mw_swap();
    mw_hash_name_21_();
    mw_tuck();
    mw_name_str();
    mw__21_();
    mw_dup();
    push_u64(0);
    push_fnptr(&mb_name_new_21__4);
    mw_prim_pack_cons();
    mw_delay();
    mw_over();
    mw_name_mangled();
    mw__21_();
}

static void mb_name_new_21__5 (void) {
    mw_prim_drop();
    mw_nip();
    mw_nip();
}

static void mb_name_new_21__4 (void) {
    mw_prim_drop();
    mw_name_mangle_compute_21_();
}

static void mb_str_buf_is_arrow_3F__1 (void) {
    mw_prim_drop();
    push_i64(0LL);
    mw_str_buf_char_40_();
    mw_is_dash_3F_();
    mw_nip();
    push_i64(1LL);
    mw_str_buf_char_40_();
    mw_is_gt_3F_();
    mw_nip();
    mw__26__26_();
}

static void mb_str_buf_is_dashes_3F__1 (void) {
    mw_prim_drop();
    push_i64(0LL);
    mw_str_buf_char_40_();
    mw_is_dash_3F_();
    mw_nip();
    push_i64(1LL);
    mw_str_buf_char_40_();
    mw_is_dash_3F_();
    mw_nip();
    mw__26__26_();
}

static void mb_str_buf_is_dashes_3F__2 (void) {
    mw_prim_drop();
    mw_false();
}

static void mb_str_buf_is_equal_3F__1 (void) {
    mw_prim_drop();
    push_i64(0LL);
    mw_str_buf_char_40_();
    mw_is_eq_3F_();
    mw_nip();
}

static void mb_str_buf_is_equal_3F__2 (void) {
    mw_prim_drop();
    mw_false();
}

static void mb_str_buf_is_dec_int_3F__1 (void) {
    mw_prim_drop();
    mw_1_2B_();
}

static void mb_str_buf_is_dec_int_3F__2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_str_buf_is_dec_int_3F__3 (void) {
    mw_prim_drop();
    mw_dup();
    mw_str_buf_char_40_();
    mw_is_digit_3F_();
    mw_nip();
}

static void mb_str_buf_is_dec_int_3F__4 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_1_2B_();
        push_value(d2);
    }
    mw_1_2B_();
}

static void mb_str_buf_is_dec_int_3F__5 (void) {
    mw_prim_drop();
    mw_1_2B_();
}

static void mb_str_buf_is_dec_int_3F__6 (void) {
    mw_prim_drop();
    mw_str_buf_length_3F_();
    mw__3D__3D_();
}

static void mb_str_buf_is_dec_int_3F__7 (void) {
    mw_prim_drop();
    mw_drop();
    mw_false();
}

static void mb_str_buf_is_hex_int_3F__1 (void) {
    mw_prim_drop();
    mw_1_2B_();
}

static void mb_str_buf_is_hex_int_3F__2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_str_buf_is_hex_int_3F__3 (void) {
    mw_prim_drop();
    mw_1_2B_();
    mw_dup();
    mw_str_buf_char_40_();
    mw_is_xX_char();
    if (pop_u64()) {
        mw_1_2B_();
        while(1) {
            mw_dup();
            mw_str_buf_char_40_();
            mw_is_hexdigit_3F_();
            mw_nip();
            if (!pop_u64()) break;
            {
                VAL d4 = pop_value();
                mw_1_2B_();
                push_value(d4);
            }
            mw_1_2B_();
        }
        mw_swap();
        push_i64(1LL);
        mw__3E__3D_();
        if (pop_u64()) {
            mw_str_buf_length_3F_();
            mw__3D__3D_();
        } else {
            mw_drop();
            mw_false();
        }
    } else {
        mw_drop2();
        mw_false();
    }
}

static void mb_str_buf_is_hex_int_3F__11 (void) {
    mw_prim_drop();
    mw_drop2();
    mw_false();
}

static void mb_str_buf_is_hex_int_3F__4 (void) {
    mw_prim_drop();
    mw_1_2B_();
    while(1) {
        mw_dup();
        mw_str_buf_char_40_();
        mw_is_hexdigit_3F_();
        mw_nip();
        if (!pop_u64()) break;
        {
            VAL d3 = pop_value();
            mw_1_2B_();
            push_value(d3);
        }
        mw_1_2B_();
    }
    mw_swap();
    push_i64(1LL);
    mw__3E__3D_();
    if (pop_u64()) {
        mw_str_buf_length_3F_();
        mw__3D__3D_();
    } else {
        mw_drop();
        mw_false();
    }
}

static void mb_str_buf_is_hex_int_3F__10 (void) {
    mw_prim_drop();
    mw_drop2();
    mw_false();
}

static void mb_str_buf_is_hex_int_3F__5 (void) {
    mw_prim_drop();
    mw_dup();
    mw_str_buf_char_40_();
    mw_is_hexdigit_3F_();
    mw_nip();
}

static void mb_str_buf_is_hex_int_3F__6 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_1_2B_();
        push_value(d2);
    }
    mw_1_2B_();
}

static void mb_str_buf_is_hex_int_3F__7 (void) {
    mw_prim_drop();
    mw_1_2B_();
}

static void mb_str_buf_is_hex_int_3F__8 (void) {
    mw_prim_drop();
    mw_str_buf_length_3F_();
    mw__3D__3D_();
}

static void mb_str_buf_is_hex_int_3F__9 (void) {
    mw_prim_drop();
    mw_drop();
    mw_false();
}

static void mb_is_xX_char_1 (void) {
    mw_prim_drop();
    push_i64(120LL);
    mw__3D__3D_();
}

static void mb_str_buf_dec_int_3F__1 (void) {
    mw_prim_drop();
    mw_is_dash_3F_();
    mw_nip();
    if (pop_u64()) {
        push_u64(0);
        push_fnptr(&mb_str_buf_dec_int_3F__3);
        mw_prim_pack_cons();
        mw_dip2();
    } else {
        mw_id();
    }
    mw_1_2B_();
}

static void mb_str_buf_dec_int_3F__5 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_str_buf_dec_int_3F__2 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_str_buf_dec_int_3F__3);
    mw_prim_pack_cons();
    mw_dip2();
}

static void mb_str_buf_dec_int_3F__4 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_str_buf_dec_int_3F__3 (void) {
    mw_prim_drop();
    mw_drop();
    push_i64(-1LL);
}

static void mb_str_buf_dec_int_3F__6 (void) {
    mw_prim_drop();
    mw_dup();
    mw_str_buf_length_3F_();
    mw__3C_();
}

static void mb_str_buf_dec_int_3F__7 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_str_buf_dec_int_3F__8);
    mw_prim_pack_cons();
    mw_sip();
    mw_1_2B_();
}

static void mb_str_buf_dec_int_3F__8 (void) {
    mw_prim_drop();
    mw_str_buf_char_40_();
    mw_Char__3E_Int();
    {
        VAL d2 = pop_value();
        push_i64(10LL);
        mw__2A_();
        push_value(d2);
    }
    push_i64(48LL);
    mw__();
    mw__2B_();
}

static void mb_str_buf_dec_int_3F__9 (void) {
    mw_prim_drop();
    push_i64(10LL);
    mw__2A_();
}

static void mb_str_buf_hex_int_3F__1 (void) {
    mw_prim_drop();
    mw_is_dash_3F_();
    mw_nip();
    if (pop_u64()) {
        push_u64(0);
        push_fnptr(&mb_str_buf_hex_int_3F__3);
        mw_prim_pack_cons();
        mw_dip2();
    } else {
        mw_id();
    }
    mw_1_2B_();
}

static void mb_str_buf_hex_int_3F__5 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_str_buf_hex_int_3F__2 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_str_buf_hex_int_3F__3);
    mw_prim_pack_cons();
    mw_dip2();
}

static void mb_str_buf_hex_int_3F__4 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_str_buf_hex_int_3F__3 (void) {
    mw_prim_drop();
    mw_drop();
    push_i64(-1LL);
}

static void mb_str_buf_hex_int_3F__6 (void) {
    mw_prim_drop();
    mw_dup();
    mw_str_buf_length_3F_();
    mw__3C_();
}

static void mb_str_buf_hex_int_3F__7 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_str_buf_hex_int_3F__8);
    mw_prim_pack_cons();
    mw_sip();
    mw_1_2B_();
}

static void mb_str_buf_hex_int_3F__8 (void) {
    mw_prim_drop();
    mw_str_buf_char_40_();
    {
        VAL d2 = pop_value();
        push_i64(16LL);
        mw__2A_();
        push_value(d2);
    }
    mw_hexdigit_value();
    mw__2B_();
}

static void mb_str_buf_hex_int_3F__9 (void) {
    mw_prim_drop();
    push_i64(16LL);
    mw__2A_();
}

static void mb_hexdigit_value_1 (void) {
    mw_prim_drop();
    mw_Char__3E_Int();
    push_i64(48LL);
    mw__();
}

static void mb_hexdigit_value_2 (void) {
    mw_prim_drop();
    mw_is_upper_hexdigit_3F_();
    if (pop_u64()) {
        mw_Char__3E_Int();
        push_i64(55LL);
        mw__();
    } else {
        mw_Char__3E_Int();
        push_i64(87LL);
        mw__();
    }
}

static void mb_hexdigit_value_3 (void) {
    mw_prim_drop();
    mw_Char__3E_Int();
    push_i64(55LL);
    mw__();
}

static void mb_hexdigit_value_4 (void) {
    mw_prim_drop();
    mw_Char__3E_Int();
    push_i64(87LL);
    mw__();
}

static void mb_lexer_push_string_char_21__1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_lexer_move_21_();
    mw_lexer_peek();
    mw_is_newline_3F_();
    if (pop_u64()) {
        mw_drop();
    } else {
        mw_is_n_3F_();
        if (pop_u64()) {
            mw_drop();
            push_i64(10LL);
            mw_Int__3E_Char();
            mw_str_buf_push_char_21_();
        } else {
            mw_is_r_3F_();
            if (pop_u64()) {
                mw_drop();
                push_i64(13LL);
                mw_Int__3E_Char();
                mw_str_buf_push_char_21_();
            } else {
                mw_is_t_3F_();
                if (pop_u64()) {
                    mw_drop();
                    push_i64(9LL);
                    mw_Int__3E_Char();
                    mw_str_buf_push_char_21_();
                } else {
                    mw_is_quote_3F_();
                    if (pop_u64()) {
                        mw_str_buf_push_char_21_();
                    } else {
                        mw_is_backslash_3F_();
                        if (pop_u64()) {
                            mw_str_buf_push_char_21_();
                        } else {
                            mw_str_buf_push_char_21_();
                            push_ptr("Unknown character escape sequence.\0\0\0\0");
                            mw_lexer_emit_warning_21_();
                        }
                    }
                }
            }
        }
    }
}

static void mb_lexer_push_string_char_21__14 (void) {
    mw_prim_drop();
    mw_str_buf_push_char_21_();
}

static void mb_lexer_push_string_char_21__2 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_lexer_push_string_char_21__3 (void) {
    mw_prim_drop();
    mw_is_n_3F_();
    if (pop_u64()) {
        mw_drop();
        push_i64(10LL);
        mw_Int__3E_Char();
        mw_str_buf_push_char_21_();
    } else {
        mw_is_r_3F_();
        if (pop_u64()) {
            mw_drop();
            push_i64(13LL);
            mw_Int__3E_Char();
            mw_str_buf_push_char_21_();
        } else {
            mw_is_t_3F_();
            if (pop_u64()) {
                mw_drop();
                push_i64(9LL);
                mw_Int__3E_Char();
                mw_str_buf_push_char_21_();
            } else {
                mw_is_quote_3F_();
                if (pop_u64()) {
                    mw_str_buf_push_char_21_();
                } else {
                    mw_is_backslash_3F_();
                    if (pop_u64()) {
                        mw_str_buf_push_char_21_();
                    } else {
                        mw_str_buf_push_char_21_();
                        push_ptr("Unknown character escape sequence.\0\0\0\0");
                        mw_lexer_emit_warning_21_();
                    }
                }
            }
        }
    }
}

static void mb_lexer_push_string_char_21__4 (void) {
    mw_prim_drop();
    mw_drop();
    push_i64(10LL);
    mw_Int__3E_Char();
    mw_str_buf_push_char_21_();
}

static void mb_lexer_push_string_char_21__5 (void) {
    mw_prim_drop();
    mw_is_r_3F_();
    if (pop_u64()) {
        mw_drop();
        push_i64(13LL);
        mw_Int__3E_Char();
        mw_str_buf_push_char_21_();
    } else {
        mw_is_t_3F_();
        if (pop_u64()) {
            mw_drop();
            push_i64(9LL);
            mw_Int__3E_Char();
            mw_str_buf_push_char_21_();
        } else {
            mw_is_quote_3F_();
            if (pop_u64()) {
                mw_str_buf_push_char_21_();
            } else {
                mw_is_backslash_3F_();
                if (pop_u64()) {
                    mw_str_buf_push_char_21_();
                } else {
                    mw_str_buf_push_char_21_();
                    push_ptr("Unknown character escape sequence.\0\0\0\0");
                    mw_lexer_emit_warning_21_();
                }
            }
        }
    }
}

static void mb_lexer_push_string_char_21__6 (void) {
    mw_prim_drop();
    mw_drop();
    push_i64(13LL);
    mw_Int__3E_Char();
    mw_str_buf_push_char_21_();
}

static void mb_lexer_push_string_char_21__7 (void) {
    mw_prim_drop();
    mw_is_t_3F_();
    if (pop_u64()) {
        mw_drop();
        push_i64(9LL);
        mw_Int__3E_Char();
        mw_str_buf_push_char_21_();
    } else {
        mw_is_quote_3F_();
        if (pop_u64()) {
            mw_str_buf_push_char_21_();
        } else {
            mw_is_backslash_3F_();
            if (pop_u64()) {
                mw_str_buf_push_char_21_();
            } else {
                mw_str_buf_push_char_21_();
                push_ptr("Unknown character escape sequence.\0\0\0\0");
                mw_lexer_emit_warning_21_();
            }
        }
    }
}

static void mb_lexer_push_string_char_21__8 (void) {
    mw_prim_drop();
    mw_drop();
    push_i64(9LL);
    mw_Int__3E_Char();
    mw_str_buf_push_char_21_();
}

static void mb_lexer_push_string_char_21__9 (void) {
    mw_prim_drop();
    mw_is_quote_3F_();
    if (pop_u64()) {
        mw_str_buf_push_char_21_();
    } else {
        mw_is_backslash_3F_();
        if (pop_u64()) {
            mw_str_buf_push_char_21_();
        } else {
            mw_str_buf_push_char_21_();
            push_ptr("Unknown character escape sequence.\0\0\0\0");
            mw_lexer_emit_warning_21_();
        }
    }
}

static void mb_lexer_push_string_char_21__10 (void) {
    mw_prim_drop();
    mw_str_buf_push_char_21_();
}

static void mb_lexer_push_string_char_21__11 (void) {
    mw_prim_drop();
    mw_is_backslash_3F_();
    if (pop_u64()) {
        mw_str_buf_push_char_21_();
    } else {
        mw_str_buf_push_char_21_();
        push_ptr("Unknown character escape sequence.\0\0\0\0");
        mw_lexer_emit_warning_21_();
    }
}

static void mb_lexer_push_string_char_21__12 (void) {
    mw_prim_drop();
    mw_str_buf_push_char_21_();
}

static void mb_lexer_push_string_char_21__13 (void) {
    mw_prim_drop();
    mw_str_buf_push_char_21_();
    push_ptr("Unknown character escape sequence.\0\0\0\0");
    mw_lexer_emit_warning_21_();
}

static void mb_lexer_emit_warning_21__1 (void) {
    mw_prim_drop();
    mw_lexer_location();
}

static void mb_lexer_comment_end_3F__1 (void) {
    mw_prim_drop();
    mw_true();
}

static void mb_lexer_comment_end_3F__2 (void) {
    mw_prim_drop();
    mw_lexer_peek();
    mw_is_newline_3F_();
    mw_nip();
}

static void mb_emit_warning_at_21__1 (void) {
    mw_prim_drop();
    mw_location_trace_21_();
}

static void mb_emit_warning_at_21__2 (void) {
    mw_prim_drop();
    mw_1_2B_();
}

static void mb_lexer_emit_error_21__1 (void) {
    mw_prim_drop();
    mw_lexer_location();
}

static void mb_emit_error_at_21__1 (void) {
    mw_prim_drop();
    mw_location_trace_21_();
}

static void mb_emit_error_at_21__2 (void) {
    mw_prim_drop();
    mw_1_2B_();
}

static void mb_stack_uncons_3 (void) {
    mw_prim_drop();
    mw_SOME();
}

static void mb_hash_1 (void) {
    mw_prim_drop();
    mw_Char__3E_Int();
    push_i64(5LL);
    mw__2A_();
    mw_swap();
    push_i64(18LL);
    mw__2A_();
    mw__5E_();
}

static void mb_name_keep_going_3F__1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_false();
}

static void mb_name_keep_going_3F__2 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_over();
        push_value(d2);
    }
    mw_name_str();
    mw__40_();
    mw_str_eq();
    mw_not();
}

static void mb_name_keep_going_3F__3 (void) {
    mw_prim_drop();
    mw_over();
}

static void mb_name_mangle_compute_21__1 (void) {
    mw_prim_drop();
    mw_is_alpha_3F_();
    if (pop_u64()) {
        mw_TS_CHAR();
    } else {
        mw_is_digit_3F_();
        if (pop_u64()) {
            mw_TS_CHAR();
        } else {
            mw_is_underscore_3F_();
            if (pop_u64()) {
                mw_TS_CHAR();
            } else {
                mw_is_dash_3F_();
                if (pop_u64()) {
                    mw_drop();
                    mw_underscore();
                    mw_TS_CHAR();
                } else {
                    mw_char_hexdigits();
                    mw_underscore();
                    mw_swap();
                    mw_cons();
                    mw_underscore();
                    mw_snoc();
                    mw_TS_PUSH();
                }
            }
        }
    }
}

static void mb_name_mangle_compute_21__2 (void) {
    mw_prim_drop();
    mw_TS_CHAR();
}

static void mb_name_mangle_compute_21__3 (void) {
    mw_prim_drop();
    mw_is_digit_3F_();
    if (pop_u64()) {
        mw_TS_CHAR();
    } else {
        mw_is_underscore_3F_();
        if (pop_u64()) {
            mw_TS_CHAR();
        } else {
            mw_is_dash_3F_();
            if (pop_u64()) {
                mw_drop();
                mw_underscore();
                mw_TS_CHAR();
            } else {
                mw_char_hexdigits();
                mw_underscore();
                mw_swap();
                mw_cons();
                mw_underscore();
                mw_snoc();
                mw_TS_PUSH();
            }
        }
    }
}

static void mb_name_mangle_compute_21__4 (void) {
    mw_prim_drop();
    mw_TS_CHAR();
}

static void mb_name_mangle_compute_21__5 (void) {
    mw_prim_drop();
    mw_is_underscore_3F_();
    if (pop_u64()) {
        mw_TS_CHAR();
    } else {
        mw_is_dash_3F_();
        if (pop_u64()) {
            mw_drop();
            mw_underscore();
            mw_TS_CHAR();
        } else {
            mw_char_hexdigits();
            mw_underscore();
            mw_swap();
            mw_cons();
            mw_underscore();
            mw_snoc();
            mw_TS_PUSH();
        }
    }
}

static void mb_name_mangle_compute_21__6 (void) {
    mw_prim_drop();
    mw_TS_CHAR();
}

static void mb_name_mangle_compute_21__7 (void) {
    mw_prim_drop();
    mw_is_dash_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_underscore();
        mw_TS_CHAR();
    } else {
        mw_char_hexdigits();
        mw_underscore();
        mw_swap();
        mw_cons();
        mw_underscore();
        mw_snoc();
        mw_TS_PUSH();
    }
}

static void mb_name_mangle_compute_21__8 (void) {
    mw_prim_drop();
    mw_drop();
    mw_underscore();
    mw_TS_CHAR();
}

static void mb_name_mangle_compute_21__9 (void) {
    mw_prim_drop();
    mw_char_hexdigits();
    mw_underscore();
    mw_swap();
    mw_cons();
    mw_underscore();
    mw_snoc();
    mw_TS_PUSH();
}

static void mb_name_cat_21__1 (void) {
    mw_prim_drop();
    mw_name_str();
    mw__40_();
}

static void mb_name_is_type_hole_1 (void) {
    mw_prim_drop();
    mw_str_tail();
    mw_str_is_empty_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_true();
    } else {
        mw_str_could_be_type_var();
    }
}

static void mb_name_is_type_hole_4 (void) {
    mw_prim_drop();
    mw_drop();
    mw_false();
}

static void mb_name_is_type_hole_2 (void) {
    mw_prim_drop();
    mw_drop();
    mw_true();
}

static void mb_name_is_type_hole_3 (void) {
    mw_prim_drop();
    mw_str_could_be_type_var();
}

static void mb_name_is_underscore_1 (void) {
    mw_prim_drop();
    mw_str_tail();
    mw_str_is_empty();
}

static void mb_name_is_underscore_2 (void) {
    mw_prim_drop();
    mw_drop();
    mw_false();
}

static void mb_name_could_be_stack_var_1 (void) {
    mw_prim_drop();
    mw_str_tail();
    mw_str_could_be_type_var();
}

static void mb_name_could_be_stack_var_2 (void) {
    mw_prim_drop();
    mw_drop();
    mw_false();
}

static void mb_name_could_be_effect_con_1 (void) {
    mw_prim_drop();
    mw_str_tail();
    mw_str_head();
    mw_is_upper_3F_();
    mw_nip();
}

static void mb_name_could_be_effect_con_2 (void) {
    mw_prim_drop();
    mw_drop();
    mw_false();
}

static void mb_force_21__3 (void) {
    mw_prim_drop();
    mw_run();
    mw_dup();
    mw_LAZY_READY();
}

static void mb_char_hexdigits_1 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(0LL);
    mw__3E_();
}

static void mb_char_hexdigits_2 (void) {
    mw_prim_drop();
    mw_char_hexdigits_next();
}

static void mb_char_hexdigits_first_1 (void) {
    mw_prim_drop();
    mw_L0();
}

static void mb_char_hexdigits_next_1 (void) {
    mw_prim_drop();
    push_i64(15LL);
    mw__26_();
    mw_hexdigit();
}

static void mb_char_hexdigits_next_2 (void) {
    mw_prim_drop();
    push_i64(15LL);
    mw__26_();
    mw_hexdigit();
}

static void mb_char_hexdigits_next_3 (void) {
    mw_prim_drop();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_snoc();
        push_value(d2);
    }
    mw_snoc();
}

static void mb_char_hexdigits_next_4 (void) {
    mw_prim_drop();
    mw_snoc();
}

static void mb_hexdigit_1 (void) {
    mw_prim_drop();
    push_i64(55LL);
    mw__2B_();
    mw_Int__3E_Char();
}

static void mb_hexdigit_2 (void) {
    mw_prim_drop();
    push_i64(48LL);
    mw__2B_();
    mw_Int__3E_Char();
}

static void mb_token_prim_3D__3F__1 (void) {
    mw_prim_drop();
    mw_dup();
}

static void mb_token_prev_4 (void) {
    mw_prim_drop();
    mw_nip();
}

static void mb_token_prev_5 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_token_next_arg_end_1 (void) {
    mw_prim_drop();
    mw_token_is_arg_end_3F_();
    mw_not();
}

static void mb_token_next_arg_end_2 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_token_has_args_3F__1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_token_succ();
    mw_token_is_lparen_3F_();
    mw_nip();
}

static void mb_token_has_args_3F__2 (void) {
    mw_prim_drop();
    mw_token_is_lparen_3F_();
}

static void mb_token_num_args_1 (void) {
    mw_prim_drop();
    mw_token_has_args_3F_();
    if (pop_u64()) {
        mw_token_succ();
    } else {
        mw_id();
    }
}

static void mb_token_num_args_4 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_token_num_args_2 (void) {
    mw_prim_drop();
    mw_token_succ();
}

static void mb_token_num_args_3 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_token_num_args_5 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_i64(0LL);
        push_value(d2);
    }
    while(1) {
        mw_token_is_right_enclosure_3F_();
        mw_not();
        if (!pop_u64()) break;
        {
            VAL d3 = pop_value();
            mw_1_2B_();
            push_value(d3);
        }
        mw_token_succ();
        mw_token_next_arg_end();
    }
    mw_drop();
}

static void mb_token_num_args_10 (void) {
    mw_prim_drop();
    mw_drop();
    push_i64(0LL);
}

static void mb_token_num_args_6 (void) {
    mw_prim_drop();
    push_i64(0LL);
}

static void mb_token_num_args_7 (void) {
    mw_prim_drop();
    mw_token_is_right_enclosure_3F_();
    mw_not();
}

static void mb_token_num_args_8 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_1_2B_();
        push_value(d2);
    }
    mw_token_succ();
    mw_token_next_arg_end();
}

static void mb_token_num_args_9 (void) {
    mw_prim_drop();
    mw_1_2B_();
}

static void mb_token_args_0_1 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_token_args_0_2 (void) {
    mw_prim_drop();
    push_ptr("expected no args\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_token_args_1_1 (void) {
    mw_prim_drop();
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_token_succ();
    } else {
        mw_id();
    }
    mw_token_succ();
}

static void mb_token_args_1_4 (void) {
    mw_prim_drop();
    mw_token_num_args_3F_();
    push_i64(1LL);
    mw__3C_();
    if (pop_u64()) {
        push_ptr("expected 1 arg, got none\0\0\0\0");
        mw_emit_fatal_error_21_();
    } else {
        push_ptr("expected 1 arg, got too many\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mb_token_args_1_2 (void) {
    mw_prim_drop();
    mw_token_succ();
}

static void mb_token_args_1_3 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_token_args_1_5 (void) {
    mw_prim_drop();
    push_ptr("expected 1 arg, got none\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_token_args_1_6 (void) {
    mw_prim_drop();
    push_ptr("expected 1 arg, got too many\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_token_args_2_1 (void) {
    mw_prim_drop();
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_token_succ();
    } else {
        mw_id();
    }
    mw_token_succ();
    mw_dup();
    mw_token_next_arg_end();
    mw_token_succ();
}

static void mb_token_args_2_4 (void) {
    mw_prim_drop();
    mw_token_num_args_3F_();
    push_i64(2LL);
    mw__3C_();
    if (pop_u64()) {
        push_ptr("expected 2 args, got too few\0\0\0\0");
        mw_emit_fatal_error_21_();
    } else {
        push_ptr("expected 2 args, got too many\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mb_token_args_2_2 (void) {
    mw_prim_drop();
    mw_token_succ();
}

static void mb_token_args_2_3 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_token_args_2_5 (void) {
    mw_prim_drop();
    push_ptr("expected 2 args, got too few\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_token_args_2_6 (void) {
    mw_prim_drop();
    push_ptr("expected 2 args, got too many\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_token_args_3_1 (void) {
    mw_prim_drop();
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_token_succ();
    } else {
        mw_id();
    }
    mw_token_succ();
    mw_dup();
    mw_token_next_arg_end();
    mw_token_succ();
    mw_dup();
    mw_token_next_arg_end();
    mw_token_succ();
}

static void mb_token_args_3_4 (void) {
    mw_prim_drop();
    mw_token_num_args_3F_();
    push_i64(3LL);
    mw__3C_();
    if (pop_u64()) {
        push_ptr("expected 3 args, got too few\0\0\0\0");
        mw_emit_fatal_error_21_();
    } else {
        push_ptr("expected 3 args, got too many\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mb_token_args_3_2 (void) {
    mw_prim_drop();
    mw_token_succ();
}

static void mb_token_args_3_3 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_token_args_3_5 (void) {
    mw_prim_drop();
    push_ptr("expected 3 args, got too few\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_token_args_3_6 (void) {
    mw_prim_drop();
    push_ptr("expected 3 args, got too many\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_token_args_1 (void) {
    mw_prim_drop();
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_token_succ();
    } else {
        mw_id();
    }
    mw_L0();
    mw_swap();
    while(1) {
        mw_token_is_args_end_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_token_succ();
        push_u64(0);
        push_fnptr(&mb_token_args_6);
        mw_prim_pack_cons();
        mw_sip();
        mw_token_next_arg_end();
    }
    mw_drop();
}

static void mb_token_args_7 (void) {
    mw_prim_drop();
    mw_drop();
    mw_L0();
}

static void mb_token_args_2 (void) {
    mw_prim_drop();
    mw_token_succ();
}

static void mb_token_args_3 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_token_args_4 (void) {
    mw_prim_drop();
    mw_token_is_args_end_3F_();
    mw_not();
}

static void mb_token_args_5 (void) {
    mw_prim_drop();
    mw_token_succ();
    push_u64(0);
    push_fnptr(&mb_token_args_6);
    mw_prim_pack_cons();
    mw_sip();
    mw_token_next_arg_end();
}

static void mb_token_args_6 (void) {
    mw_prim_drop();
    mw_snoc();
}

static void mb_token_is_args_end_3F__1 (void) {
    mw_prim_drop();
    mw_token_succ();
}

static void mb_token_is_args_end_3F__2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_token_args_2_2B__1 (void) {
    mw_prim_drop();
    mw_nip();
}

static void mb_token_args_2_2B__2 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("expected 2+ args\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_emit_warning_21__1 (void) {
    mw_prim_drop();
    mw_token_location();
}

static void mb_emit_error_21__1 (void) {
    mw_prim_drop();
    mw_token_location();
}

static void mb_token_run_1 (void) {
    mw_prim_drop();
    mw_token_run_end_3F_();
    mw_not();
}

static void mb_token_run_2 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_token_run_3);
    mw_prim_pack_cons();
    mw_sip();
}

static void mb_token_run_3 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_token_run_has_arrow_1 (void) {
    mw_prim_drop();
    mw_token_is_arrow_3F_();
}

static void mb_token_run_has_dashes_1 (void) {
    mw_prim_drop();
    mw_token_is_dashes_3F_();
}

static void mb_sig_is_stack_end_3F__1 (void) {
    mw_prim_drop();
    mw_true();
}

static void mb_sig_is_stack_end_3F__2 (void) {
    mw_prim_drop();
    mw_token_run_end_3F_();
}

static void mb_sig_is_stack_end2_3F__1 (void) {
    mw_prim_drop();
    mw_true();
}

static void mb_sig_is_stack_end2_3F__2 (void) {
    mw_prim_drop();
    mw_sig_token_is_effect_con_3F_();
}

static void mb_sig_next_stack_end_1 (void) {
    mw_prim_drop();
    mw_sig_is_stack_end_3F_();
    mw_not();
}

static void mb_sig_next_stack_end_2 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_sig_arity_1 (void) {
    mw_prim_drop();
    mw_sig_count_types();
    mw_token_next();
    mw_sig_count_types();
    mw_drop();
}

static void mb_sig_arity_2 (void) {
    mw_prim_drop();
    mw_sig_count_types();
    mw_drop();
    push_i64(0LL);
    mw_swap();
}

static void mb_sig_count_types_1 (void) {
    mw_prim_drop();
    mw_sig_is_stack_end_3F_();
    mw_not();
}

static void mb_sig_count_types_2 (void) {
    mw_prim_drop();
    mw_sig_token_is_type_3F_();
    if (pop_u64()) {
        {
            VAL d3 = pop_value();
            mw_1_2B_();
            push_value(d3);
        }
    } else {
        mw_id();
    }
    mw_token_next();
}

static void mb_sig_count_types_3 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_1_2B_();
        push_value(d2);
    }
}

static void mb_sig_count_types_5 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_sig_count_types_4 (void) {
    mw_prim_drop();
    mw_1_2B_();
}

static void mb_sig_skip_dashes_1 (void) {
    mw_prim_drop();
    mw_sig_next_stack_end();
    mw_token_next();
}

static void mb_sig_skip_dashes_2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_module_add_import_21__1 (void) {
    mw_prim_drop();
    mw_set_cons();
}

static void mb_module_path_from_name_1 (void) {
    mw_prim_drop();
    mw_is_dot_3F_();
    if (pop_u64()) {
        mw_drop();
        push_ptr("/\0\0\0\0");
        mw_TS_COPY();
    } else {
        mw_is_upper_3F_();
        if (pop_u64()) {
            mw_Char__3E_Int();
            push_i64(32LL);
            mw__7C_();
            mw_Int__3E_Char();
            mw_TS_CHAR();
        } else {
            mw_TS_CHAR();
        }
    }
}

static void mb_module_path_from_name_2 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("/\0\0\0\0");
    mw_TS_COPY();
}

static void mb_module_path_from_name_3 (void) {
    mw_prim_drop();
    mw_is_upper_3F_();
    if (pop_u64()) {
        mw_Char__3E_Int();
        push_i64(32LL);
        mw__7C_();
        mw_Int__3E_Char();
        mw_TS_CHAR();
    } else {
        mw_TS_CHAR();
    }
}

static void mb_module_path_from_name_4 (void) {
    mw_prim_drop();
    mw_Char__3E_Int();
    push_i64(32LL);
    mw__7C_();
    mw_Int__3E_Char();
    mw_TS_CHAR();
}

static void mb_module_path_from_name_5 (void) {
    mw_prim_drop();
    mw_TS_CHAR();
}

static void mb_set_snoc_1 (void) {
    mw_prim_drop();
    mw_unSET();
}

static void mb_bag_replace_1 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_bag_replace_2 (void) {
    mw_prim_drop();
    mw_bag_insert();
}

static void mb_codegen_flush_21__1 (void) {
    mw_prim_drop();
    mw_codegen_file();
    mw__40_();
    mw_File__3E_Int();
    mw_CODEGEN_BUF();
    mw_codegen_length();
    mw__40_();
    mw_posix_write_21_();
    mw_dup();
    push_i64(0LL);
    mw__3C_();
    if (pop_u64()) {
        push_ptr("error: codegen write failed\0\0\0\0");
        mw_panic_21_();
    } else {
        mw_codegen_length();
        mw__40_();
        mw__3C_();
        if (pop_u64()) {
            push_ptr("error: codegen write wrote fewer bytes than expected\0\0\0\0");
            mw_panic_21_();
        } else {
            push_i64(0LL);
            mw_codegen_length();
            mw__21_();
        }
    }
}

static void mb_codegen_flush_21__6 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_codegen_flush_21__2 (void) {
    mw_prim_drop();
    push_ptr("error: codegen write failed\0\0\0\0");
    mw_panic_21_();
}

static void mb_codegen_flush_21__3 (void) {
    mw_prim_drop();
    mw_codegen_length();
    mw__40_();
    mw__3C_();
    if (pop_u64()) {
        push_ptr("error: codegen write wrote fewer bytes than expected\0\0\0\0");
        mw_panic_21_();
    } else {
        push_i64(0LL);
        mw_codegen_length();
        mw__21_();
    }
}

static void mb_codegen_flush_21__4 (void) {
    mw_prim_drop();
    push_ptr("error: codegen write wrote fewer bytes than expected\0\0\0\0");
    mw_panic_21_();
}

static void mb_codegen_flush_21__5 (void) {
    mw_prim_drop();
    push_i64(0LL);
    mw_codegen_length();
    mw__21_();
}

static void mb__2E_b_1 (void) {
    mw_prim_drop();
    mw_codegen_flush_21_();
}

static void mb__2E_b_2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb__2E_b_3 (void) {
    mw_prim_drop();
    mw_dup();
    mw_1_2B_();
}

static void mb__2E_c_1 (void) {
    mw_prim_drop();
    mw_codegen_flush_21_();
}

static void mb__2E_c_2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb__2E_c_3 (void) {
    mw_prim_drop();
    mw_char_21_();
}

static void mb__2E_c_4 (void) {
    mw_prim_drop();
    mw__2B_();
}

static void mb__2E__1 (void) {
    mw_prim_drop();
    mw_codegen_flush_21_();
    while(1) {
        mw_dup();
        mw_CODEGEN_BUF_SIZE();
        mw__3E_();
        if (!pop_u64()) break;
        mw_over();
        mw_CODEGEN_BUF_SIZE();
        mw_CODEGEN_BUF();
        mw_prim_ptr_copy();
        mw_CODEGEN_BUF_SIZE();
        mw_codegen_length();
        mw__21_();
        mw_codegen_flush_21_();
        {
            VAL d3 = pop_value();
            mw_CODEGEN_BUF_SIZE();
            mw_swap();
            mw_ptr_2B_();
            push_value(d3);
        }
        mw_CODEGEN_BUF_SIZE();
        mw__();
    }
    mw_dup();
    mw_codegen_length();
    mw__21_();
    mw_CODEGEN_BUF();
    mw_prim_ptr_copy();
}

static void mb__2E__5 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb__2E__6);
    mw_prim_pack_cons();
    mw_sip();
    mw_codegen_length();
    push_u64(0);
    push_fnptr(&mb__2E__7);
    mw_prim_pack_cons();
    mw_modify();
}

static void mb__2E__2 (void) {
    mw_prim_drop();
    mw_dup();
    mw_CODEGEN_BUF_SIZE();
    mw__3E_();
}

static void mb__2E__3 (void) {
    mw_prim_drop();
    mw_over();
    mw_CODEGEN_BUF_SIZE();
    mw_CODEGEN_BUF();
    mw_prim_ptr_copy();
    mw_CODEGEN_BUF_SIZE();
    mw_codegen_length();
    mw__21_();
    mw_codegen_flush_21_();
    {
        VAL d2 = pop_value();
        mw_CODEGEN_BUF_SIZE();
        mw_swap();
        mw_ptr_2B_();
        push_value(d2);
    }
    mw_CODEGEN_BUF_SIZE();
    mw__();
}

static void mb__2E__4 (void) {
    mw_prim_drop();
    mw_CODEGEN_BUF_SIZE();
    mw_swap();
    mw_ptr_2B_();
}

static void mb__2E__6 (void) {
    mw_prim_drop();
    mw_codegen_length();
    mw__40_();
    mw_CODEGEN_BUF();
    mw_ptr_2B_();
    mw_prim_ptr_copy();
}

static void mb__2E__7 (void) {
    mw_prim_drop();
    mw__2B_();
}

static void mb_run_output_c99_21__1 (void) {
    mw_prim_drop();
    mw_drop2();
}

static void mb_run_output_c99_21__2 (void) {
    mw_prim_drop();
    mw_make_output_path();
    mw_Path__3E_Str();
    mw_create_file_21_();
    mw_codegen_start_21_();
    mw_c99_header_21_();
    mw_c99_tags_21_();
    mw_c99_buffers_21_();
    mw_c99_variables_21_();
    mw_c99_externals_21_();
    mw_c99_word_sigs_21_();
    mw_c99_block_sigs_21_();
    mw_c99_field_sigs_21_();
    mw_c99_main_21_();
    mw_c99_word_defs_21_();
    mw_c99_block_defs_21_();
    mw_c99_field_defs_21_();
    mw_codegen_end_21_();
}

static void mb_c99_tags_21__1 (void) {
    mw_prim_drop();
    mw_c99_tag_21_();
}

static void mb_c99_buffers_21__1 (void) {
    mw_prim_drop();
    mw_c99_buffer_21_();
}

static void mb_c99_variables_21__1 (void) {
    mw_prim_drop();
    mw_c99_variable_21_();
}

static void mb_c99_externals_21__1 (void) {
    mw_prim_drop();
    mw_c99_external_21_();
}

static void mb_c99_word_sigs_21__1 (void) {
    mw_prim_drop();
    mw_c99_word_sig_21_();
}

static void mb_c99_block_sigs_21__1 (void) {
    mw_prim_drop();
    mw_c99_block_sig_21_();
}

static void mb_c99_field_sigs_21__1 (void) {
    mw_prim_drop();
    mw_c99_field_sig_21_();
}

static void mb_c99_main_21__1 (void) {
    mw_prim_drop();
    mw_c99_arrow_21_();
}

static void mb_c99_word_defs_21__1 (void) {
    mw_prim_drop();
    mw_c99_word_def_21_();
}

static void mb_c99_block_defs_21__1 (void) {
    mw_prim_drop();
    mw_c99_block_def_21_();
}

static void mb_c99_field_defs_21__1 (void) {
    mw_prim_drop();
    mw_c99_field_def_21_();
}

static void mb_c99_tag_21__1 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_c99_tag_21__2 (void) {
    mw_prim_drop();
    mw_tag_num_inputs_3F_();
    push_i64(0LL);
    mw__3D__3D_();
    if (pop_u64()) {
        push_ptr("    push_u64(\0\0\0\0");
        mw__2E_();
        mw_tag_value();
        mw__40_();
        mw__2E_n();
        push_ptr("LL);\0\0\0\0");
        mw__3B_();
    } else {
        push_ptr("    VAL car = pop_value();\0\0\0\0");
        mw__3B_();
        mw_tag_num_inputs_3F_();
        mw_1_();
        push_u64(0);
        push_fnptr(&mb_c99_tag_21__5);
        mw_prim_pack_cons();
        mw_repeat();
        push_ptr("    VAL tag = mku64(\0\0\0\0");
        mw__2E_();
        mw_tag_value();
        mw__40_();
        mw__2E_n();
        push_ptr("LL);\0\0\0\0");
        mw__3B_();
        push_ptr("    car = mkcell(car, tag);\0\0\0\0");
        mw__3B_();
        push_ptr("    push_value(car);\0\0\0\0");
        mw__3B_();
    }
}

static void mb_c99_tag_21__3 (void) {
    mw_prim_drop();
    push_ptr("    push_u64(\0\0\0\0");
    mw__2E_();
    mw_tag_value();
    mw__40_();
    mw__2E_n();
    push_ptr("LL);\0\0\0\0");
    mw__3B_();
}

static void mb_c99_tag_21__4 (void) {
    mw_prim_drop();
    push_ptr("    VAL car = pop_value();\0\0\0\0");
    mw__3B_();
    mw_tag_num_inputs_3F_();
    mw_1_();
    push_u64(0);
    push_fnptr(&mb_c99_tag_21__5);
    mw_prim_pack_cons();
    mw_repeat();
    push_ptr("    VAL tag = mku64(\0\0\0\0");
    mw__2E_();
    mw_tag_value();
    mw__40_();
    mw__2E_n();
    push_ptr("LL);\0\0\0\0");
    mw__3B_();
    push_ptr("    car = mkcell(car, tag);\0\0\0\0");
    mw__3B_();
    push_ptr("    push_value(car);\0\0\0\0");
    mw__3B_();
}

static void mb_c99_tag_21__5 (void) {
    mw_prim_drop();
    push_ptr("    car = mkcell(car, pop_value());\0\0\0\0");
    mw__3B_();
}

static void mb_tag_num_inputs_3F__3 (void) {
    mw_prim_drop();
    mw_token_run_end_3F_();
    mw_not();
}

static void mb_tag_num_inputs_3F__4 (void) {
    mw_prim_drop();
    mw_token_next();
    {
        VAL d2 = pop_value();
        mw_1_2B_();
        push_value(d2);
    }
}

static void mb_tag_num_inputs_3F__5 (void) {
    mw_prim_drop();
    mw_1_2B_();
}

static void mb_c99_external_21__1 (void) {
    mw_prim_drop();
    push_ptr("can't declare external with multiple return values\0\0\0\0");
    mw_panic_21_();
}

static void mb_c99_external_21__2 (void) {
    mw_prim_drop();
    mw_dup();
    push_i64(1LL);
    mw__3E__3D_();
    if (pop_u64()) {
        push_ptr("int64_t \0\0\0\0");
        mw__2E_();
    } else {
        push_ptr("void \0\0\0\0");
        mw__2E_();
    }
}

static void mb_c99_external_21__3 (void) {
    mw_prim_drop();
    push_ptr("int64_t \0\0\0\0");
    mw__2E_();
}

static void mb_c99_external_21__4 (void) {
    mw_prim_drop();
    push_ptr("void \0\0\0\0");
    mw__2E_();
}

static void mb_c99_external_21__5 (void) {
    mw_prim_drop();
    mw_dup();
    mw_external_name();
    mw__40_();
    mw__2E_name();
}

static void mb_c99_external_21__6 (void) {
    mw_prim_drop();
    push_ptr("int64_t\0\0\0\0");
    mw__2E_();
    mw_1_();
    push_u64(0);
    push_fnptr(&mb_c99_external_21__7);
    mw_prim_pack_cons();
    mw_repeat();
}

static void mb_c99_external_21__8 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("void\0\0\0\0");
    mw__2E_();
}

static void mb_c99_external_21__7 (void) {
    mw_prim_drop();
    push_ptr(", int64_t\0\0\0\0");
    mw__2E_();
}

static void mb_c99_external_21__9 (void) {
    mw_prim_drop();
    mw_dup();
    mw_external_name();
    mw__40_();
    mw__2E_name();
}

static void mb_c99_external_21__10 (void) {
    mw_prim_drop();
    push_ptr("    int64_t x\0\0\0\0");
    mw__2E_();
    mw__2E_n();
    push_ptr(" = pop_i64();\0\0\0\0");
    mw__3B_();
}

static void mb_c99_external_21__11 (void) {
    mw_prim_drop();
    push_ptr("    push_i64(\0\0\0\0");
}

static void mb_c99_external_21__12 (void) {
    mw_prim_drop();
    push_ptr("    \0\0\0\0");
}

static void mb_c99_external_21__13 (void) {
    mw_prim_drop();
    mw_dup();
    mw_external_name();
    mw__40_();
    mw__2E_name();
}

static void mb_c99_external_21__14 (void) {
    mw_prim_drop();
    mw_dup();
    mw_0_3E_();
    if (pop_u64()) {
        mw_dup();
        mw_1_();
        mw_dup();
        push_u64(0);
        push_fnptr(&mb_c99_external_21__16);
        mw_prim_pack_cons();
        mw_count();
        push_ptr("x\0\0\0\0");
        mw__2E_();
        mw__2E_n();
    } else {
        mw_id();
    }
}

static void mb_c99_external_21__15 (void) {
    mw_prim_drop();
    mw_dup();
    mw_1_();
    mw_dup();
    push_u64(0);
    push_fnptr(&mb_c99_external_21__16);
    mw_prim_pack_cons();
    mw_count();
    push_ptr("x\0\0\0\0");
    mw__2E_();
    mw__2E_n();
}

static void mb_c99_external_21__17 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_c99_external_21__16 (void) {
    mw_prim_drop();
    push_ptr("x\0\0\0\0");
    mw__2E_();
    mw__2E_n();
    push_ptr(", \0\0\0\0");
    mw__2E_();
}

static void mb_c99_external_21__18 (void) {
    mw_prim_drop();
    push_ptr(");\0\0\0\0");
}

static void mb_c99_external_21__19 (void) {
    mw_prim_drop();
    push_ptr(";\0\0\0\0");
}

static void mb_c99_nest_2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_1_2B_();
    decref(var_f);
}

static void mb_c99_nest_3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_1_();
    decref(var_f);
}

static void mb_c99_indent_1 (void) {
    mw_prim_drop();
    push_ptr("    \0\0\0\0");
    mw__2E_();
}

static void mb_c99_call_21__1 (void) {
    mw_prim_drop();
    mw_c99_args_push_21_();
}

static void mb_c99_call_21__2 (void) {
    mw_prim_drop();
    push_ptr("mw_\0\0\0\0");
    mw__2E_();
    mw__2E_name();
    push_ptr("();\0\0\0\0");
    mw__2E_();
}

static void mb_c99_args_push_21__1 (void) {
    mw_prim_drop();
    mw_c99_arg_push_21_();
}

static void mb_c99_arrow_21__1 (void) {
    mw_prim_drop();
    mw_c99_atom_21_();
}

static void mb_c99_atom_21__1 (void) {
    mw_prim_drop();
    mw_atom_args();
    mw__40_();
}

static void mb_c99_int_21__1 (void) {
    mw_prim_drop();
    push_ptr("push_i64(\0\0\0\0");
    mw__2E_();
    mw__2E_n();
    push_ptr("LL);\0\0\0\0");
    mw__2E_();
}

static void mb_c99_str_21__1 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_str_21__2);
    mw_prim_pack_cons();
    mw_c99_line();
    push_u64(0);
    push_fnptr(&mb_c99_str_21__3);
    mw_prim_pack_cons();
    mw_c99_nest();
    push_u64(0);
    push_fnptr(&mb_c99_str_21__13);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_str_21__14 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_str_21__15);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_str_21__2 (void) {
    mw_prim_drop();
    push_ptr("{\0\0\0\0");
    mw__2E_();
}

static void mb_c99_str_21__3 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_str_21__4);
    mw_prim_pack_cons();
    mw_c99_line();
    push_u64(0);
    push_fnptr(&mb_c99_str_21__5);
    mw_prim_pack_cons();
    mw_c99_nest();
    push_u64(0);
    push_fnptr(&mb_c99_str_21__11);
    mw_prim_pack_cons();
    mw_c99_line();
    push_u64(0);
    push_fnptr(&mb_c99_str_21__12);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_str_21__4 (void) {
    mw_prim_drop();
    push_ptr("static uint8_t b[\0\0\0\0");
    mw__2E_();
    mw_dup();
    mw_str_size();
    push_i64(4LL);
    mw__2B_();
    mw__2E_n();
    push_ptr("] = {\0\0\0\0");
    mw__2E_();
}

static void mb_c99_str_21__5 (void) {
    mw_prim_drop();
    mw_c99_indent();
    push_u64(0);
    push_fnptr(&mb_c99_str_21__6);
    mw_prim_pack_cons();
    mw_str_for();
    mw__2E_lf();
    push_u64(0);
    push_fnptr(&mb_c99_str_21__10);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_str_21__6 (void) {
    mw_prim_drop();
    mw_char_bytes();
    push_u64(0);
    push_fnptr(&mb_c99_str_21__7);
    mw_prim_pack_cons();
    mw_for();
}

static void mb_c99_str_21__7 (void) {
    mw_prim_drop();
    mw_U8__3E_Int();
    mw_dup();
    mw__2E_n();
    push_ptr(",\0\0\0\0");
    mw__2E_();
    push_i64(10LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw__2E_lf();
        mw_c99_indent();
    } else {
        mw_id();
    }
}

static void mb_c99_str_21__8 (void) {
    mw_prim_drop();
    mw__2E_lf();
    mw_c99_indent();
}

static void mb_c99_str_21__9 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_c99_str_21__10 (void) {
    mw_prim_drop();
    push_ptr("0,0,0,0,\0\0\0\0");
    mw__2E_();
}

static void mb_c99_str_21__11 (void) {
    mw_prim_drop();
    push_ptr("};\0\0\0\0");
    mw__2E_();
}

static void mb_c99_str_21__12 (void) {
    mw_prim_drop();
    push_ptr("push_ptr((void*)b);\0\0\0\0");
    mw__2E_();
}

static void mb_c99_str_21__13 (void) {
    mw_prim_drop();
    push_ptr("}\0\0\0\0");
    mw__2E_();
}

static void mb_c99_str_21__15 (void) {
    mw_prim_drop();
    push_ptr("push_ptr(\"\0\0\0\0");
    mw__2E_();
    push_u64(0);
    push_fnptr(&mb_c99_str_21__16);
    mw_prim_pack_cons();
    mw_str_for();
    push_ptr("\\0\\0\\0\\0\");\0\0\0\0");
    mw__2E_();
}

static void mb_c99_str_21__16 (void) {
    mw_prim_drop();
    mw_c99_string_char_21_();
}

static void mb_c99_prim_21__3 (void) {
    mw_prim_drop();
    push_ptr("{\0\0\0\0");
    mw__2E_();
}

static void mb_c99_prim_21__4 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_prim_21__5);
    mw_prim_pack_cons();
    mw_c99_line();
    mw_c99_arg_run_21_();
    push_u64(0);
    push_fnptr(&mb_c99_prim_21__6);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_prim_21__5 (void) {
    mw_prim_drop();
    push_ptr("VAL d\0\0\0\0");
    mw__2E_();
    mw_c99_depth();
    mw__40_();
    mw__2E_n();
    push_ptr(" = pop_value();\0\0\0\0");
    mw__2E_();
}

static void mb_c99_prim_21__6 (void) {
    mw_prim_drop();
    push_ptr("push_value(d\0\0\0\0");
    mw__2E_();
    mw_c99_depth();
    mw__40_();
    mw__2E_n();
    push_ptr(");\0\0\0\0");
    mw__2E_();
}

static void mb_c99_prim_21__7 (void) {
    mw_prim_drop();
    push_ptr("}\0\0\0\0");
    mw__2E_();
}

static void mb_c99_prim_21__11 (void) {
    mw_prim_drop();
    push_ptr("if (pop_u64()) {\0\0\0\0");
    mw__2E_();
}

static void mb_c99_prim_21__12 (void) {
    mw_prim_drop();
    mw_swap();
    mw_c99_arg_run_21_();
}

static void mb_c99_prim_21__13 (void) {
    mw_prim_drop();
    push_ptr("} else {\0\0\0\0");
    mw__2E_();
}

static void mb_c99_prim_21__14 (void) {
    mw_prim_drop();
    mw_c99_arg_run_21_();
}

static void mb_c99_prim_21__15 (void) {
    mw_prim_drop();
    push_ptr("}\0\0\0\0");
    mw__2E_();
}

static void mb_c99_prim_21__19 (void) {
    mw_prim_drop();
    push_ptr("while(1) {\0\0\0\0");
    mw__2E_();
}

static void mb_c99_prim_21__20 (void) {
    mw_prim_drop();
    mw_swap();
    mw_c99_arg_run_21_();
    push_u64(0);
    push_fnptr(&mb_c99_prim_21__21);
    mw_prim_pack_cons();
    mw_c99_line();
    mw_c99_arg_run_21_();
}

static void mb_c99_prim_21__21 (void) {
    mw_prim_drop();
    push_ptr("if (!pop_u64()) break;\0\0\0\0");
    mw__2E_();
}

static void mb_c99_prim_21__22 (void) {
    mw_prim_drop();
    push_ptr("}\0\0\0\0");
    mw__2E_();
}

static void mb_c99_match_21__1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_match_cases();
    mw__40_();
    mw_first();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_match_token();
            mw__40_();
            push_ptr("codegen: unexpected number of cases in transparent match\0\0\0\0");
            mw_emit_fatal_error_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_case_body();
            mw__40_();
            mw_c99_arrow_21_();
            mw_drop();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mb_c99_match_21__4 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_match_21__5);
    mw_prim_pack_cons();
    mw_c99_line();
    push_u64(0);
    push_fnptr(&mb_c99_match_21__6);
    mw_prim_pack_cons();
    mw_c99_nest();
    push_u64(0);
    push_fnptr(&mb_c99_match_21__11);
    mw_prim_pack_cons();
    mw_c99_line();
    mw__2E_();
}

static void mb_c99_match_21__5 (void) {
    mw_prim_drop();
    push_ptr("switch (get_top_data_tag()) {\0\0\0\0");
    mw__2E_();
}

static void mb_c99_match_21__6 (void) {
    mw_prim_drop();
    mw_dup();
    mw_match_cases();
    mw__40_();
    push_u64(0);
    push_fnptr(&mb_c99_match_21__7);
    mw_prim_pack_cons();
    mw_for();
    mw_match_has_default_case_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_u64(0);
        push_fnptr(&mb_c99_match_21__10);
        mw_prim_pack_cons();
        mw_c99_line();
    }
    mw_drop();
}

static void mb_c99_match_21__7 (void) {
    mw_prim_drop();
    mw_c99_case_21_();
}

static void mb_c99_match_21__8 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_c99_match_21__9 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_match_21__10);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_match_21__10 (void) {
    mw_prim_drop();
    push_ptr("default: write(2, \"unexpected fallthrough in match\\n\", 32); mw_prim_debug(); exit(99);\0\0\0\0");
    mw__2E_();
}

static void mb_c99_match_21__11 (void) {
    mw_prim_drop();
    push_ptr("}\0\0\0\0");
}

static void mb_c99_lambda_21__1 (void) {
    mw_prim_drop();
    push_ptr("{\0\0\0\0");
    mw__2E_();
}

static void mb_c99_lambda_21__2 (void) {
    mw_prim_drop();
    mw_dup();
    mw_lambda_params();
    mw__40_();
    push_u64(0);
    push_fnptr(&mb_c99_lambda_21__3);
    mw_prim_pack_cons();
    mw_reverse_for();
    mw_dup();
    mw_lambda_body();
    mw__40_();
    mw_c99_arrow_21_();
    mw_lambda_params();
    mw__40_();
    push_u64(0);
    push_fnptr(&mb_c99_lambda_21__5);
    mw_prim_pack_cons();
    mw_reverse_for();
}

static void mb_c99_lambda_21__3 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_lambda_21__4);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_lambda_21__4 (void) {
    mw_prim_drop();
    push_ptr("VAL \0\0\0\0");
    mw__2E_();
    mw__2E_param();
    push_ptr(" = pop_value();\0\0\0\0");
    mw__2E_();
}

static void mb_c99_lambda_21__5 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_lambda_21__6);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_lambda_21__6 (void) {
    mw_prim_drop();
    push_ptr("decref(\0\0\0\0");
    mw__2E_();
    mw__2E_param();
    push_ptr(");\0\0\0\0");
    mw__2E_();
}

static void mb_c99_lambda_21__7 (void) {
    mw_prim_drop();
    push_ptr("}\0\0\0\0");
    mw__2E_();
}

static void mb_c99_var_21__1 (void) {
    mw_prim_drop();
    mw_c99_var_push_21_();
}

static void mb_c99_var_21__2 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_var_21__3);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_var_21__4 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_c99_var_21__3 (void) {
    mw_prim_drop();
    push_ptr("mw_prim_run();\0\0\0\0");
    mw__2E_();
}

static void mb_c99_block_push_21__1 (void) {
    mw_prim_drop();
    push_ptr("push_fnptr(&\0\0\0\0");
    mw__2E_();
    mw__2E_block();
    push_ptr(");\0\0\0\0");
    mw__2E_();
}

static void mb_c99_block_push_21__2 (void) {
    mw_prim_drop();
    push_ptr("mw_prim_pack_cons();\0\0\0\0");
    mw__2E_();
}

static void mb_c99_string_char_21__1 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("\\\\\0\0\0\0");
    mw__2E_();
}

static void mb_c99_string_char_21__2 (void) {
    mw_prim_drop();
    mw_is_quote_3F_();
    if (pop_u64()) {
        mw_drop();
        push_ptr("\\\"\0\0\0\0");
        mw__2E_();
    } else {
        mw_dup();
        mw_Char__3E_Int();
        push_i64(32LL);
        push_i64(127LL);
        mw_in_range();
        if (pop_u64()) {
            mw__2E_c();
        } else {
            mw_dup();
            mw_Char__3E_Int();
            push_i64(9LL);
            mw__3D__3D_();
            if (pop_u64()) {
                mw_drop();
                push_ptr("\\t\0\0\0\0");
                mw__2E_();
            } else {
                mw_dup();
                mw_Char__3E_Int();
                push_i64(10LL);
                mw__3D__3D_();
                if (pop_u64()) {
                    mw_drop();
                    push_ptr("\\n\0\0\0\0");
                    mw__2E_();
                } else {
                    mw_dup();
                    mw_Char__3E_Int();
                    push_i64(13LL);
                    mw__3D__3D_();
                    if (pop_u64()) {
                        mw_drop();
                        push_ptr("\\r\0\0\0\0");
                        mw__2E_();
                    } else {
                        mw_char_bytes();
                        push_u64(0);
                        push_fnptr(&mb_c99_string_char_21__13);
                        mw_prim_pack_cons();
                        mw_for();
                    }
                }
            }
        }
    }
}

static void mb_c99_string_char_21__3 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("\\\"\0\0\0\0");
    mw__2E_();
}

static void mb_c99_string_char_21__4 (void) {
    mw_prim_drop();
    mw_dup();
    mw_Char__3E_Int();
    push_i64(32LL);
    push_i64(127LL);
    mw_in_range();
    if (pop_u64()) {
        mw__2E_c();
    } else {
        mw_dup();
        mw_Char__3E_Int();
        push_i64(9LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            push_ptr("\\t\0\0\0\0");
            mw__2E_();
        } else {
            mw_dup();
            mw_Char__3E_Int();
            push_i64(10LL);
            mw__3D__3D_();
            if (pop_u64()) {
                mw_drop();
                push_ptr("\\n\0\0\0\0");
                mw__2E_();
            } else {
                mw_dup();
                mw_Char__3E_Int();
                push_i64(13LL);
                mw__3D__3D_();
                if (pop_u64()) {
                    mw_drop();
                    push_ptr("\\r\0\0\0\0");
                    mw__2E_();
                } else {
                    mw_char_bytes();
                    push_u64(0);
                    push_fnptr(&mb_c99_string_char_21__13);
                    mw_prim_pack_cons();
                    mw_for();
                }
            }
        }
    }
}

static void mb_c99_string_char_21__5 (void) {
    mw_prim_drop();
    mw__2E_c();
}

static void mb_c99_string_char_21__6 (void) {
    mw_prim_drop();
    mw_dup();
    mw_Char__3E_Int();
    push_i64(9LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        push_ptr("\\t\0\0\0\0");
        mw__2E_();
    } else {
        mw_dup();
        mw_Char__3E_Int();
        push_i64(10LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            push_ptr("\\n\0\0\0\0");
            mw__2E_();
        } else {
            mw_dup();
            mw_Char__3E_Int();
            push_i64(13LL);
            mw__3D__3D_();
            if (pop_u64()) {
                mw_drop();
                push_ptr("\\r\0\0\0\0");
                mw__2E_();
            } else {
                mw_char_bytes();
                push_u64(0);
                push_fnptr(&mb_c99_string_char_21__13);
                mw_prim_pack_cons();
                mw_for();
            }
        }
    }
}

static void mb_c99_string_char_21__7 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("\\t\0\0\0\0");
    mw__2E_();
}

static void mb_c99_string_char_21__8 (void) {
    mw_prim_drop();
    mw_dup();
    mw_Char__3E_Int();
    push_i64(10LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        push_ptr("\\n\0\0\0\0");
        mw__2E_();
    } else {
        mw_dup();
        mw_Char__3E_Int();
        push_i64(13LL);
        mw__3D__3D_();
        if (pop_u64()) {
            mw_drop();
            push_ptr("\\r\0\0\0\0");
            mw__2E_();
        } else {
            mw_char_bytes();
            push_u64(0);
            push_fnptr(&mb_c99_string_char_21__13);
            mw_prim_pack_cons();
            mw_for();
        }
    }
}

static void mb_c99_string_char_21__9 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("\\n\0\0\0\0");
    mw__2E_();
}

static void mb_c99_string_char_21__10 (void) {
    mw_prim_drop();
    mw_dup();
    mw_Char__3E_Int();
    push_i64(13LL);
    mw__3D__3D_();
    if (pop_u64()) {
        mw_drop();
        push_ptr("\\r\0\0\0\0");
        mw__2E_();
    } else {
        mw_char_bytes();
        push_u64(0);
        push_fnptr(&mb_c99_string_char_21__13);
        mw_prim_pack_cons();
        mw_for();
    }
}

static void mb_c99_string_char_21__11 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("\\r\0\0\0\0");
    mw__2E_();
}

static void mb_c99_string_char_21__12 (void) {
    mw_prim_drop();
    mw_char_bytes();
    push_u64(0);
    push_fnptr(&mb_c99_string_char_21__13);
    mw_prim_pack_cons();
    mw_for();
}

static void mb_c99_string_char_21__13 (void) {
    mw_prim_drop();
    push_ptr("\\x\0\0\0\0");
    mw__2E_();
    mw_dup();
    mw_U8__3E_Int();
    push_i64(4LL);
    mw__3E__3E_();
    mw_hexdigit();
    mw__2E_c();
    mw_U8__3E_Int();
    push_i64(15LL);
    mw__26_();
    mw_hexdigit();
    mw__2E_c();
}

static void mb_c99_var_push_21__1 (void) {
    mw_prim_drop();
    push_ptr("push_value(\0\0\0\0");
    mw__2E_();
    mw_dup();
    mw__2E_var();
    push_ptr(");\0\0\0\0");
    mw__2E_();
}

static void mb_c99_var_push_21__2 (void) {
    mw_prim_drop();
    push_ptr("incref(\0\0\0\0");
    mw__2E_();
    mw__2E_var();
    push_ptr(");\0\0\0\0");
    mw__2E_();
}

static void mb_c99_pack_ctx_21__1 (void) {
    mw_prim_drop();
    push_ptr("push_u64(0);\0\0\0\0");
    mw__2E_();
}

static void mb_c99_pack_ctx_21__2 (void) {
    mw_prim_drop();
    mw_c99_var_push_21_();
    push_u64(0);
    push_fnptr(&mb_c99_pack_ctx_21__3);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_pack_ctx_21__3 (void) {
    mw_prim_drop();
    push_ptr("mw_prim_pack_cons();\0\0\0\0");
    mw__2E_();
}

static void mb_ctx_physical_vars_1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_var_is_physical();
}

static void mb_c99_unpack_ctx_21__1 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_unpack_ctx_21__2);
    mw_prim_pack_cons();
    mw_c99_line();
    push_u64(0);
    push_fnptr(&mb_c99_unpack_ctx_21__3);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_unpack_ctx_21__2 (void) {
    mw_prim_drop();
    push_ptr("mw_prim_pack_uncons();\0\0\0\0");
    mw__2E_();
}

static void mb_c99_unpack_ctx_21__3 (void) {
    mw_prim_drop();
    push_ptr("VAL \0\0\0\0");
    mw__2E_();
    mw__2E_var();
    push_ptr(" = pop_value();\0\0\0\0");
    mw__2E_();
}

static void mb_c99_unpack_ctx_21__4 (void) {
    mw_prim_drop();
    push_ptr("mw_prim_drop();\0\0\0\0");
    mw__2E_();
}

static void mb_c99_decref_ctx_21__1 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_decref_ctx_21__2);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_decref_ctx_21__2 (void) {
    mw_prim_drop();
    push_ptr("decref(\0\0\0\0");
    mw__2E_();
    mw__2E_var();
    push_ptr(");\0\0\0\0");
    mw__2E_();
}

static void mb__2E_block_1 (void) {
    mw_prim_drop();
    mw_drop2();
    mw_Block_2E_id();
    mw__2E_n();
}

static void mb__2E_block_2 (void) {
    mw_prim_drop();
    mw_word_name();
    mw__40_();
    mw__2E_name();
    push_ptr("_\0\0\0\0");
    mw__2E_();
    mw_arrow_homeidx();
    mw__40_();
    mw__2E_n();
    mw_drop();
}

static void mb_c99_case_21__1 (void) {
    mw_prim_drop();
    mw_case_body();
    mw__40_();
    mw_c99_arrow_21_();
    push_u64(0);
    push_fnptr(&mb_c99_case_21__2);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_case_21__2 (void) {
    mw_prim_drop();
    push_ptr("break;\0\0\0\0");
    mw__2E_();
}

static void mb_c99_pattern_21__2 (void) {
    mw_prim_drop();
    push_ptr("default:\0\0\0\0");
    mw__2E_();
}

static void mb_c99_pattern_21__4 (void) {
    mw_prim_drop();
    push_ptr("case \0\0\0\0");
    mw__2E_();
    mw_dup();
    mw_tag_value();
    mw__40_();
    mw__2E_n();
    push_ptr("LL:\0\0\0\0");
    mw__2E_();
}

static void mb_c99_pattern_21__5 (void) {
    mw_prim_drop();
    mw_tag_num_inputs_3F_();
    mw_nip();
    mw_dup();
    push_i64(0LL);
    mw__3E_();
    if (pop_u64()) {
        push_u64(0);
        push_fnptr(&mb_c99_pattern_21__7);
        mw_prim_pack_cons();
        mw_c99_line();
        mw_1_();
        push_u64(0);
        push_fnptr(&mb_c99_pattern_21__8);
        mw_prim_pack_cons();
        mw_repeat();
    } else {
        mw_drop();
        push_u64(0);
        push_fnptr(&mb_c99_pattern_21__11);
        mw_prim_pack_cons();
        mw_c99_line();
    }
}

static void mb_c99_pattern_21__6 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_pattern_21__7);
    mw_prim_pack_cons();
    mw_c99_line();
    mw_1_();
    push_u64(0);
    push_fnptr(&mb_c99_pattern_21__8);
    mw_prim_pack_cons();
    mw_repeat();
}

static void mb_c99_pattern_21__10 (void) {
    mw_prim_drop();
    mw_drop();
    push_u64(0);
    push_fnptr(&mb_c99_pattern_21__11);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_pattern_21__7 (void) {
    mw_prim_drop();
    push_ptr("mw_prim_pack_uncons(); mw_prim_drop();\0\0\0\0");
    mw__2E_();
}

static void mb_c99_pattern_21__8 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_c99_pattern_21__9);
    mw_prim_pack_cons();
    mw_c99_line();
}

static void mb_c99_pattern_21__9 (void) {
    mw_prim_drop();
    push_ptr("mw_prim_pack_uncons(); mw_prim_swap();\0\0\0\0");
    mw__2E_();
}

static void mb_c99_pattern_21__11 (void) {
    mw_prim_drop();
    push_ptr("mw_prim_drop();\0\0\0\0");
    mw__2E_();
}

static void mb_c99_word_sig_21__1 (void) {
    mw_prim_drop();
    push_ptr("static void mw_\0\0\0\0");
    mw__2E_();
    mw_word_name();
    mw__40_();
    mw__2E_name();
    push_ptr(" (void);\0\0\0\0");
    mw__2E_();
}

static void mb_c99_block_sig_21__1 (void) {
    mw_prim_drop();
    push_ptr("static void \0\0\0\0");
    mw__2E_();
    mw__2E_block();
    push_ptr(" (void);\0\0\0\0");
    mw__2E_();
}

static void mb_c99_field_sig_21__1 (void) {
    mw_prim_drop();
    push_ptr("static void mw_\0\0\0\0");
    mw__2E_();
    mw_field_name();
    mw__40_();
    mw__2E_name();
    push_ptr(" (void);\0\0\0\0");
    mw__2E_();
}

static void mb_c99_block_def_21__1 (void) {
    mw_prim_drop();
    push_ptr("static void \0\0\0\0");
    mw__2E_();
    mw_dup();
    mw__2E_block();
    push_ptr(" (void) {\0\0\0\0");
    mw__2E_();
}

static void mb_c99_block_def_21__2 (void) {
    mw_prim_drop();
    mw_block_arrow();
    mw_force_21_();
    mw_dup();
    mw_arrow_ctx();
    mw__40_();
    mw_c99_unpack_ctx_21_();
    mw_dup();
    mw_c99_arrow_21_();
    mw_arrow_ctx();
    mw__40_();
    mw_c99_decref_ctx_21_();
}

static void mb_c99_block_def_21__3 (void) {
    mw_prim_drop();
    push_ptr("}\0\0\0\0");
    mw__2E_();
}

static void mb_c99_word_def_21__1 (void) {
    mw_prim_drop();
    mw_c99_arrow_21_();
}

static void mb_atom_arg_add_left_21__1 (void) {
    mw_prim_drop();
    mw_cons();
}

static void mb_arrow_atom_add_21__1 (void) {
    mw_prim_drop();
    mw_swap();
    mw_snoc();
}

static void mb_block_new_deferred_21__1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_block_ctx();
    mw__40_();
    mw_swap();
    mw_dup();
    mw_block_dom();
    mw__40_();
    mw_swap();
    mw_dup();
    mw_block_cod();
    mw__40_();
    mw_swap();
    mw_block_token();
    mw__40_();
    mw_elab_arrow_hom_21_();
}

static void mb_elab_arrow_hom_21__1 (void) {
    mw_prim_drop();
    mw_elab_arrow_fwd_21_();
    mw_dup();
    mw_arrow_token_end();
    mw__40_();
    mw_GAMMA();
    mw_over();
    mw_arrow_cod();
    mw__40_();
}

static void mb_block_unify_type_21__1 (void) {
    mw_prim_drop();
    mw_swap();
    push_u64(0);
    push_fnptr(&mb_block_unify_type_21__2);
    mw_prim_pack_cons();
    mw_sip();
}

static void mb_block_unify_type_21__2 (void) {
    mw_prim_drop();
    mw_block_dom();
    mw__40_();
    mw_type_unify_21_();
    mw_drop();
}

static void mb_block_unify_type_21__3 (void) {
    mw_prim_drop();
    mw_block_cod();
    mw__40_();
    mw_type_unify_21_();
    mw_drop();
}

static void mb_elab_expand_morphism_21__2 (void) {
    mw_prim_drop();
    mw_TYPE_ERROR();
    mw_TYPE_ERROR();
}

static void mb_elab_expand_morphism_21__5 (void) {
    mw_prim_drop();
    mw_meta_alloc_21_();
    mw_TMeta();
    mw_meta_alloc_21_();
    mw_TMeta();
    mw_dup2();
    mw_T__3E_();
    mw_SOME();
}

static void mb_elab_expand_morphism_21__7 (void) {
    mw_prim_drop();
    mw_TYPE_ERROR();
    mw_TYPE_ERROR();
}

static void mb_type_unify_21__8 (void) {
    mw_prim_drop();
    mw_TMeta();
}

static void mb_type_unify_21__14 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_type_unify_21__16 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_type_unify_21__19 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_type_unify_21__24 (void) {
    mw_prim_drop();
    mw_TPrim();
}

static void mb_type_unify_21__26 (void) {
    mw_prim_drop();
    mw_TPrim();
}

static void mb_type_unify_21__28 (void) {
    mw_prim_drop();
    mw_TPrim();
}

static void mb_type_unify_21__31 (void) {
    mw_prim_drop();
    mw_TPrim();
}

static void mb_type_unify_21__36 (void) {
    mw_prim_drop();
    mw_TData();
}

static void mb_type_unify_21__38 (void) {
    mw_prim_drop();
    mw_TData();
}

static void mb_type_unify_21__40 (void) {
    mw_prim_drop();
    mw_TData();
}

static void mb_type_unify_21__43 (void) {
    mw_prim_drop();
    mw_TData();
}

static void mb_type_unify_21__48 (void) {
    mw_prim_drop();
    mw_TTable();
}

static void mb_type_unify_21__50 (void) {
    mw_prim_drop();
    mw_TTable();
}

static void mb_type_unify_21__52 (void) {
    mw_prim_drop();
    mw_TTable();
}

static void mb_type_unify_21__55 (void) {
    mw_prim_drop();
    mw_TTable();
}

static void mb_type_unify_21__60 (void) {
    mw_prim_drop();
    mw_TTensor();
}

static void mb_type_unify_21__62 (void) {
    mw_prim_drop();
    mw_TTensor();
}

static void mb_type_unify_21__64 (void) {
    mw_prim_drop();
    mw_TTensor();
}

static void mb_type_unify_21__67 (void) {
    mw_prim_drop();
    mw_TTensor();
}

static void mb_type_unify_21__72 (void) {
    mw_prim_drop();
    mw_TMorphism();
}

static void mb_type_unify_21__74 (void) {
    mw_prim_drop();
    mw_TMorphism();
}

static void mb_type_unify_21__76 (void) {
    mw_prim_drop();
    mw_TMorphism();
}

static void mb_type_unify_21__79 (void) {
    mw_prim_drop();
    mw_TMorphism();
}

static void mb_type_unify_21__84 (void) {
    mw_prim_drop();
    mw_TApp();
}

static void mb_type_unify_21__86 (void) {
    mw_prim_drop();
    mw_TApp();
}

static void mb_type_unify_21__88 (void) {
    mw_prim_drop();
    mw_TApp();
}

static void mb_type_unify_21__91 (void) {
    mw_prim_drop();
    mw_TApp();
}

static void mb_type_unify_21__96 (void) {
    mw_prim_drop();
    mw_TValue();
}

static void mb_type_unify_21__98 (void) {
    mw_prim_drop();
    mw_TValue();
}

static void mb_match_add_case_21__1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_match_cases();
    mw__40_();
}

static void mb_match_add_case_21__2 (void) {
    mw_prim_drop();
    mw_case_token();
    mw__40_();
    push_ptr("Case is unreachable.\0\0\0\0");
    mw_emit_error_21_();
    mw_drop();
}

static void mb_match_add_case_21__3 (void) {
    mw_prim_drop();
    mw_snoc();
    mw_over();
    mw_match_cases();
    mw__21_();
}

static void mb_match_is_exhaustive_3F__1 (void) {
    mw_prim_drop();
    mw_true();
}

static void mb_match_is_exhaustive_3F__2 (void) {
    mw_prim_drop();
    mw_match_scrutinee_data_3F_();
    switch (get_top_data_tag()) {
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_data_num_tags();
            mw_over();
            mw_match_cases();
            mw__40_();
            mw_len();
            mw__3D__3D_();
            break;
        case 0LL:
            mw_prim_drop();
            mw_true();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mb_match_scrutinee_data_3F__1 (void) {
    mw_prim_drop();
    mw_type_head();
    switch (get_top_data_tag()) {
        case 7LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_SOME();
            break;
        default:
            mw_drop();
            mw_NONE();
            break;
    
}}

static void mb_cases_have_default_case_1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_case_is_default_case();
}

static void mb_type_head_2 (void) {
    mw_prim_drop();
    mw_type_head();
}

static void mb_type_head_3 (void) {
    mw_prim_drop();
    mw_TMeta();
}

static void mb_cases_cover_case_1 (void) {
    mw_prim_drop();
    mw_dup2();
    mw_case_is_covered();
}

static void mb_case_is_covered_1 (void) {
    mw_prim_drop();
    mw_case_pattern();
    mw__40_();
}

static void mb_pattern_is_covered_1 (void) {
    mw_prim_drop();
    mw_drop2();
    mw_true();
}

static void mb_pattern_is_covered_2 (void) {
    mw_prim_drop();
    mw__3D__3D_();
}

static void mb_def_type_21__1 (void) {
    mw_prim_drop();
    mw_DEF_TYPE();
}

static void mb_T1_1 (void) {
    mw_prim_drop();
    mw_T0();
}

static void mb_T2_1 (void) {
    mw_prim_drop();
    mw_T1();
}

static void mb_T3_1 (void) {
    mw_prim_drop();
    mw_T2();
}

static void mb_T4_1 (void) {
    mw_prim_drop();
    mw_T3();
}

static void mb_T5_1 (void) {
    mw_prim_drop();
    mw_T4();
}

static void mb_T6_1 (void) {
    mw_prim_drop();
    mw_T5();
}

static void mb_type_is_physical_2 (void) {
    mw_prim_drop();
    mw_type_is_physical();
}

static void mb_type_is_physical_3 (void) {
    mw_prim_drop();
    push_ptr("unbound meta at type-is-physical\0\0\0\0");
    mw_panic_21_();
}

static void mb_meta_expand_1 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_meta_expand_2 (void) {
    mw_prim_drop();
    mw_TMeta();
}

static void mb_type_unify_failed_21__1 (void) {
    mw_prim_drop();
    mw_gamma_token_3F_();
    mw_token_location();
    mw_location_trace_21_();
}

static void mb_type_unify_failed_21__2 (void) {
    mw_prim_drop();
    mw_type_trace_21_();
}

static void mb_type_unify_failed_21__3 (void) {
    mw_prim_drop();
    mw_1_2B_();
}

static void mb_type_hole_unify_21__1 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_type_hole_unify_21__2 (void) {
    mw_prim_drop();
    mw_THole();
    mw_type_trace_21_();
    push_ptr(" ~ \0\0\0\0");
    mw_str_trace_21_();
    mw_dup();
    mw_type_trace_21_();
    mw_trace_ln_21_();
}

static void mb_meta_unify_21__3 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_meta_unify_21__4 (void) {
    mw_prim_drop();
    mw_swap();
    mw_type_has_meta_3F_();
    if (pop_u64()) {
        mw_swap();
        mw_TMeta();
        mw_type_unify_failed_21_();
    } else {
        mw_tuck();
        mw_SOME();
        mw_swap();
        mw_meta_type();
        mw__21_();
    }
}

static void mb_meta_unify_21__5 (void) {
    mw_prim_drop();
    mw_swap();
    mw_TMeta();
    mw_type_unify_failed_21_();
}

static void mb_meta_unify_21__6 (void) {
    mw_prim_drop();
    mw_tuck();
    mw_SOME();
    mw_swap();
    mw_meta_type();
    mw__21_();
}

static void mb_type_var_unify_21__1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_TVar();
}

static void mb_type_var_unify_21__2 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_TVar();
        push_value(d2);
    }
    mw_TVar();
    mw_type_unify_failed_21_();
}

static void mb_type_var_unify_21__3 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_type_prim_unify_21__1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_TPrim();
}

static void mb_type_prim_unify_21__2 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_TPrim();
        push_value(d2);
    }
    mw_TPrim();
    mw_type_unify_failed_21_();
}

static void mb_type_prim_unify_21__3 (void) {
    mw_prim_drop();
    mw_TPrim();
}

static void mb_type_data_unify_21__1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_TData();
}

static void mb_type_data_unify_21__2 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_TData();
        push_value(d2);
    }
    mw_TData();
    mw_type_unify_failed_21_();
}

static void mb_type_data_unify_21__3 (void) {
    mw_prim_drop();
    mw_TData();
}

static void mb_type_table_unify_21__1 (void) {
    mw_prim_drop();
    mw_drop();
    mw_TTable();
}

static void mb_type_table_unify_21__2 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_TTable();
        push_value(d2);
    }
    mw_TTable();
    mw_type_unify_failed_21_();
}

static void mb_type_table_unify_21__3 (void) {
    mw_prim_drop();
    mw_TTable();
}

static void mb_type_unify_pair_21__1 (void) {
    mw_prim_drop();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_type_unify_21_();
        mw_swap();
        push_value(d2);
    }
}

static void mb_type_unify_pair_21__2 (void) {
    mw_prim_drop();
    mw_type_unify_21_();
    mw_swap();
}

static void mb_type_unify_pair_21__3 (void) {
    mw_prim_drop();
    mw_swap();
}

static void mb_value_unify_21__3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_VALUE_INT();
    mw_TValue();
}

static void mb_value_unify_21__4 (void) {
    mw_prim_drop();
    mw_drop2();
    mw_TYPE_INT();
}

static void mb_value_unify_21__9 (void) {
    mw_prim_drop();
    mw_drop();
    mw_VALUE_STR();
    mw_TValue();
}

static void mb_value_unify_21__10 (void) {
    mw_prim_drop();
    mw_drop2();
    mw_TYPE_STR();
}

static void mb_value_unify_21__15 (void) {
    mw_prim_drop();
    mw_drop();
    mw_VALUE_BLOCK();
    mw_TValue();
}

static void mb_value_unify_21__16 (void) {
    mw_prim_drop();
    mw_block_infer_type_21_();
    mw_block_unify_type_21_();
}

static void mb_type2_has_meta_1 (void) {
    mw_prim_drop();
    mw_drop2();
    mw_true();
}

static void mb_type2_has_meta_2 (void) {
    mw_prim_drop();
    mw_type_has_meta();
}

static void mb_value_type_has_meta_4 (void) {
    mw_prim_drop();
    mw_block_dom();
    mw__40_();
}

static void mb_type_trace_sig_21__2 (void) {
    mw_prim_drop();
    mw_type_trace_sig_21_();
}

static void mb_type_trace_sig_21__3 (void) {
    mw_prim_drop();
    mw_meta_trace_21_();
}

static void mb_type_trace_stack_dom_21__1 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_type_trace_stack_dom_21__2 (void) {
    mw_prim_drop();
    mw_type_trace_stack_21_();
    push_ptr(" \0\0\0\0");
    mw_str_trace_21_();
}

static void mb_type_trace_stack_cod_21__1 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_type_trace_stack_cod_21__2 (void) {
    mw_prim_drop();
    push_ptr(" \0\0\0\0");
    mw_str_trace_21_();
    mw_type_trace_stack_21_();
}

static void mb_type_trace_stack_21__2 (void) {
    mw_prim_drop();
    mw_type_trace_stack_21_();
}

static void mb_type_trace_stack_21__3 (void) {
    mw_prim_drop();
    mw_meta_trace_21_();
}

static void mb_type_trace_stack_21__6 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_type_trace_stack_21__7 (void) {
    mw_prim_drop();
    push_ptr(" .\0\0\0\0");
    mw_str_trace_21_();
}

static void mb_value_as_type_4 (void) {
    mw_prim_drop();
    mw_block_dom();
    mw__40_();
}

static void mb_type_semifreshen_sig_1 (void) {
    mw_prim_drop();
    mw_type_semifreshen_sig_aux();
}

static void mb_type_semifreshen_sig_2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_type_sig_needs_fresh_stack_rest_2 (void) {
    mw_prim_drop();
    mw_type_sig_needs_fresh_stack_rest();
}

static void mb_type_sig_needs_fresh_stack_rest_3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_false();
}

static void mb_type_sig_needs_fresh_stack_rest_5 (void) {
    mw_prim_drop();
    mw_type_stack_rest();
    mw_TYPE_UNIT();
    mw__3D__3D_();
}

static void mb_type_sig_needs_fresh_stack_rest_6 (void) {
    mw_prim_drop();
    mw_drop();
    mw_false();
}

static void mb_type_semifreshen_sig_aux_2 (void) {
    mw_prim_drop();
    mw_type_semifreshen_sig_aux();
}

static void mb_type_semifreshen_sig_aux_3 (void) {
    mw_prim_drop();
    mw_TMeta();
}

static void mb_type_semifreshen_sig_aux_5 (void) {
    mw_prim_drop();
    mw_type_semifreshen_sig_stack();
}

static void mb_type_semifreshen_sig_aux_6 (void) {
    mw_prim_drop();
    mw_type_semifreshen_sig_stack();
}

static void mb_type_semifreshen_sig_stack_2 (void) {
    mw_prim_drop();
    mw_type_semifreshen_sig_stack();
}

static void mb_type_semifreshen_sig_stack_3 (void) {
    mw_prim_drop();
    mw_TMeta();
}

static void mb_type_semifreshen_sig_stack_5 (void) {
    mw_prim_drop();
    mw_type_semifreshen_sig_stack();
}

static void mb_type_freshen_sig_1 (void) {
    mw_prim_drop();
    mw_type_freshen_sig_aux();
}

static void mb_type_freshen_sig_2 (void) {
    mw_prim_drop();
    mw_type_freshen();
}

static void mb_type_freshen_sig_aux_2 (void) {
    mw_prim_drop();
    mw_type_freshen_sig_aux();
}

static void mb_type_freshen_sig_aux_3 (void) {
    mw_prim_drop();
    mw_TMeta();
}

static void mb_type_freshen_sig_aux_5 (void) {
    mw_prim_drop();
    mw_type_freshen_sig_stack();
}

static void mb_type_freshen_sig_aux_6 (void) {
    mw_prim_drop();
    mw_type_freshen_sig_stack();
}

static void mb_type_freshen_sig_aux_7 (void) {
    mw_prim_drop();
    mw_nip();
}

static void mb_type_stack_rest_2 (void) {
    mw_prim_drop();
    mw_type_stack_rest();
}

static void mb_type_stack_rest_3 (void) {
    mw_prim_drop();
    mw_TMeta();
}

static void mb_type_freshen_sig_stack_2 (void) {
    mw_prim_drop();
    mw_type_freshen_sig_stack();
}

static void mb_type_freshen_sig_stack_3 (void) {
    mw_prim_drop();
    mw_meta_freshen();
}

static void mb_type_freshen_sig_stack_5 (void) {
    mw_prim_drop();
    mw_type_freshen_sig_stack();
}

static void mb_type_freshen_sig_stack_6 (void) {
    mw_prim_drop();
    mw_type_freshen();
}

static void mb_meta_freshen_1 (void) {
    mw_prim_drop();
    mw_type_freshen();
}

static void mb_meta_freshen_2 (void) {
    mw_prim_drop();
    mw_drop();
    mw_meta_alloc_21_();
    mw_TMeta();
}

static void mb_type_var_freshen_1 (void) {
    mw_prim_drop();
    mw_tuck();
    mw_subst_get_var();
}

static void mb_type_var_freshen_2 (void) {
    mw_prim_drop();
    mw_meta_alloc_21_();
    mw_TMeta();
    mw_dup();
    {
        VAL d2 = pop_value();
        mw_rotr();
        mw_subst_new_21_();
        push_value(d2);
    }
}

static void mb_type_var_freshen_3 (void) {
    mw_prim_drop();
    mw_rotr();
    mw_subst_new_21_();
}

static void mb_type_pair_freshen_1 (void) {
    mw_prim_drop();
    mw_type_freshen();
    mw_swap();
}

static void mb_type_pair_freshen_2 (void) {
    mw_prim_drop();
    mw_swap();
}

static void mb_type_rigidify_sig_21__2 (void) {
    mw_prim_drop();
    mw_meta_alloc_21_();
    mw_TMeta();
    mw_meta_alloc_21_();
    mw_TMeta();
    mw_TMorphism();
}

static void mb_meta_expand_or_update_21__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    mw_dup();
    mw_SOME();
    decref(var_f);
}

static void mb_type_rigidify_21__2 (void) {
    mw_prim_drop();
    mw_ctx_make_fresh_type_var_21_();
    mw_TVar();
}

static void mb_type_rigidify_21__11 (void) {
    mw_prim_drop();
    mw_type_rigidify_21_();
}

static void mb_type_rigidify_21__12 (void) {
    mw_prim_drop();
    mw_type_rigidify_21_();
}

static void mb_type_rigidify_21__14 (void) {
    mw_prim_drop();
    mw_type_rigidify_stack_21_();
}

static void mb_type_rigidify_21__15 (void) {
    mw_prim_drop();
    mw_type_rigidify_21_();
}

static void mb_type_rigidify_21__17 (void) {
    mw_prim_drop();
    mw_type_rigidify_stack_21_();
}

static void mb_type_rigidify_21__18 (void) {
    mw_prim_drop();
    mw_type_rigidify_stack_21_();
}

static void mb_type_rigidify_stack_21__2 (void) {
    mw_prim_drop();
    mw_ctx_make_fresh_stack_type_var_21_();
    mw_TVar();
}

static void mb_type_rigidify_stack_21__4 (void) {
    mw_prim_drop();
    mw_type_rigidify_stack_21_();
}

static void mb_type_rigidify_stack_21__5 (void) {
    mw_prim_drop();
    mw_type_rigidify_21_();
}

static void mb_type_arity_2 (void) {
    mw_prim_drop();
    mw_type_arity();
}

static void mb_type_arity_3 (void) {
    mw_prim_drop();
    mw_drop();
    push_i64(0LL);
}

static void mb_type_max_count_2 (void) {
    mw_prim_drop();
    mw_type_max_count();
}

static void mb_type_max_count_3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_NONE();
}

static void mb_type_max_count_7 (void) {
    mw_prim_drop();
    mw_data_tags();
    mw__40_();
    mw_len();
    mw_SOME();
}

static void mb_type_max_count_8 (void) {
    mw_prim_drop();
    mw_drop();
    mw_NONE();
}

static void mb_data_is_enum_3F__1 (void) {
    mw_prim_drop();
    mw_tag_num_inputs_3F_();
    push_i64(0LL);
    mw__3D__3D_();
}

static void mb_type_max_num_params_2 (void) {
    mw_prim_drop();
    mw_type_max_num_params();
}

static void mb_type_max_num_params_3 (void) {
    mw_prim_drop();
    mw_drop();
    push_i64(0LL);
}

static void mb_type_num_morphisms_on_top_2 (void) {
    mw_prim_drop();
    mw_type_num_morphisms_on_top();
}

static void mb_type_num_morphisms_on_top_3 (void) {
    mw_prim_drop();
    mw_drop();
    push_i64(0LL);
}

static void mb_type_num_morphisms_on_top_5 (void) {
    mw_prim_drop();
    mw_type_num_morphisms_on_top();
    mw_1_2B_();
}

static void mb_type_num_morphisms_on_top_6 (void) {
    mw_prim_drop();
    mw_drop();
    push_i64(0LL);
}

static void mb_map_insert_1 (void) {
    mw_prim_drop();
    mw_unMAP();
}

static void mb_map_lookup_1 (void) {
    mw_prim_drop();
    mw_unMAP();
}

static void mb_subst_match_var_1 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_subst_match_var_2);
    mw_prim_pack_cons();
    mw_sip();
}

static void mb_subst_match_var_3 (void) {
    mw_prim_drop();
    mw_subst_new_21_();
}

static void mb_subst_match_var_2 (void) {
    mw_prim_drop();
    mw_subst_get_var();
    mw_type_unify_21_();
    mw_drop();
}

static void mb_data_add_tag_21__1 (void) {
    mw_prim_drop();
    mw_snoc();
}

static void mb_map_keys_1 (void) {
    mw_prim_drop();
    mw_unpack2();
    mw_nip();
}

static void mb_map_values_1 (void) {
    mw_prim_drop();
    mw_unpack2();
    mw_drop();
}

static void mb_order2_1 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_order2_2 (void) {
    mw_prim_drop();
    mw_swap();
}

static void mb_order3_1 (void) {
    mw_prim_drop();
    mw_order2();
}

static void mb_order3_2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_order3_3 (void) {
    mw_prim_drop();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_order2();
        push_value(d2);
    }
}

static void mb_order3_4 (void) {
    mw_prim_drop();
    mw_order2();
}

static void mb_Bag__3E_Bag_2B__1 (void) {
    mw_prim_drop();
    mw_BAG_2B_();
}

static void mb_bag_split_half_left_1 (void) {
    mw_prim_drop();
    mw_BAG_2B_();
}

static void mb_bag_split_half_right_1 (void) {
    mw_prim_drop();
    mw_BAG();
}

static void mb_bag_split_half_1 (void) {
    mw_prim_drop();
    mw_BAG();
}

static void mb_bag_unsnoc_1 (void) {
    mw_prim_drop();
    mw_BAG();
}

static void mb_bag_insert_2B__2B__1 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_bag_first_2B_();
        push_value(d2);
    }
    mw_B2_2B_();
}

static void mb_bag_insert_2B__2B__3 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_bag_split_half_right();
        push_value(d2);
    }
    mw_over();
    mw_bag_first_2B_();
    mw_dup2();
    mw__3E__3D_();
    if (pop_u64()) {
        mw_drop();
        mw_bag_insert_2B__2B_();
        mw_bag_cat_unsafe__2B_();
    } else {
        mw_drop();
        mw_swap();
        {
            VAL d3 = pop_value();
            mw_bag_insert_2B_();
            push_value(d3);
        }
        mw_bag_cat_unsafe_2B_();
    }
}

static void mb_bag_insert_2B__2B__2 (void) {
    mw_prim_drop();
    mw_bag_first_2B_();
}

static void mb_bag_insert_2B__2B__4 (void) {
    mw_prim_drop();
    mw_bag_split_half_right();
}

static void mb_bag_insert_2B__2B__5 (void) {
    mw_prim_drop();
    mw_drop();
    mw_bag_insert_2B__2B_();
    mw_bag_cat_unsafe__2B_();
}

static void mb_bag_insert_2B__2B__6 (void) {
    mw_prim_drop();
    mw_drop();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_bag_insert_2B_();
        push_value(d2);
    }
    mw_bag_cat_unsafe_2B_();
}

static void mb_bag_insert_2B__2B__7 (void) {
    mw_prim_drop();
    mw_bag_insert_2B_();
}

static void mb_bag_has_2B__1 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_bag_first_2B_();
        push_value(d2);
    }
    mw__3D__3D_();
}

static void mb_bag_has_2B__3 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_bag_split_half_right();
        push_value(d2);
    }
    mw_over();
    mw_bag_first_2B_();
    mw_cmp_3F_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_drop4();
            mw_true();
            break;
        case 1LL:
            mw_prim_drop();
            mw_drop();
            mw_nip();
            mw_bag_has();
            break;
        case 2LL:
            mw_prim_drop();
            mw_drop();
            {
                VAL d4 = pop_value();
                mw_nip();
                push_value(d4);
            }
            mw_bag_has_2B_();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mb_bag_has_2B__2 (void) {
    mw_prim_drop();
    mw_bag_first_2B_();
}

static void mb_bag_has_2B__4 (void) {
    mw_prim_drop();
    mw_bag_split_half_right();
}

static void mb_bag_has_2B__8 (void) {
    mw_prim_drop();
    mw_nip();
}

static void mb_bag_cat_unsafe__2B__1 (void) {
    mw_prim_drop();
    mw_unBAG();
}

static void mb_bag_cat_unsafe_2B__1 (void) {
    mw_prim_drop();
    mw_unBAG_2B_();
}

static void mb_bag_cat_unsafe_1 (void) {
    mw_prim_drop();
    mw_unBAG();
}

static void mb_bag_lookup_key_2B__1 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_bag_first_2B_();
        mw_unpack2();
        push_value(d2);
    }
    mw__3D__3D_();
    if (pop_u64()) {
        mw_SOME();
    } else {
        mw_drop();
        mw_NONE();
    }
}

static void mb_bag_lookup_key_2B__5 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_bag_split_half_right();
        mw_dup();
        mw_bag_first_2B_();
        mw_unpack2();
        push_value(d2);
    }
    mw_cmp_3F_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_drop2();
            mw_SOME();
            {
                VAL d4 = pop_value();
                mw_drop2();
                push_value(d4);
            }
            break;
        case 1LL:
            mw_prim_drop();
            {
                VAL d4 = pop_value();
                mw_drop2();
                mw_nip();
                push_value(d4);
            }
            mw_bag_lookup_key_2B_();
            break;
        case 2LL:
            mw_prim_drop();
            {
                VAL d4 = pop_value();
                mw_drop3();
                push_value(d4);
            }
            mw_bag_lookup_key();
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mb_bag_lookup_key_2B__2 (void) {
    mw_prim_drop();
    mw_bag_first_2B_();
    mw_unpack2();
}

static void mb_bag_lookup_key_2B__3 (void) {
    mw_prim_drop();
    mw_SOME();
}

static void mb_bag_lookup_key_2B__4 (void) {
    mw_prim_drop();
    mw_drop();
    mw_NONE();
}

static void mb_bag_lookup_key_2B__6 (void) {
    mw_prim_drop();
    mw_bag_split_half_right();
    mw_dup();
    mw_bag_first_2B_();
    mw_unpack2();
}

static void mb_bag_lookup_key_2B__8 (void) {
    mw_prim_drop();
    mw_drop2();
}

static void mb_bag_lookup_key_2B__10 (void) {
    mw_prim_drop();
    mw_drop2();
    mw_nip();
}

static void mb_bag_lookup_key_2B__12 (void) {
    mw_prim_drop();
    mw_drop3();
}

static void mb_bag_replace_key_2B__2B__1 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_bag_first_2B_();
        push_value(d2);
    }
    mw_dup2();
    mw__3D__3D_key();
    if (pop_u64()) {
        mw_nip();
        mw_B1_2B_();
    } else {
        mw_B2_2B_();
    }
}

static void mb_bag_replace_key_2B__2B__5 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_bag_split_half_right();
        mw_dup();
        mw_bag_first_2B_();
        push_value(d2);
    }
    mw_dup2();
    mw__3C__3D_key();
    if (pop_u64()) {
        mw_nip();
        mw_bag_replace_key_2B__2B_();
        mw_bag_cat_unsafe__2B_();
    } else {
        mw_nip();
        mw_swap();
        {
            VAL d3 = pop_value();
            mw_bag_replace_key_2B_();
            push_value(d3);
        }
        mw_bag_cat_unsafe_2B_();
    }
}

static void mb_bag_replace_key_2B__2B__2 (void) {
    mw_prim_drop();
    mw_bag_first_2B_();
}

static void mb_bag_replace_key_2B__2B__3 (void) {
    mw_prim_drop();
    mw_nip();
    mw_B1_2B_();
}

static void mb_bag_replace_key_2B__2B__4 (void) {
    mw_prim_drop();
    mw_B2_2B_();
}

static void mb_bag_replace_key_2B__2B__6 (void) {
    mw_prim_drop();
    mw_bag_split_half_right();
    mw_dup();
    mw_bag_first_2B_();
}

static void mb_bag_replace_key_2B__2B__7 (void) {
    mw_prim_drop();
    mw_nip();
    mw_bag_replace_key_2B__2B_();
    mw_bag_cat_unsafe__2B_();
}

static void mb_bag_replace_key_2B__2B__8 (void) {
    mw_prim_drop();
    mw_nip();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_bag_replace_key_2B_();
        push_value(d2);
    }
    mw_bag_cat_unsafe_2B_();
}

static void mb_bag_replace_key_2B__2B__9 (void) {
    mw_prim_drop();
    mw_bag_replace_key_2B_();
}

static void mb__3D__3D_key_1 (void) {
    mw_prim_drop();
    mw_unpack2();
    mw_nip();
}

static void mb__3C__3D_key_1 (void) {
    mw_prim_drop();
    mw_unpack2();
    mw_nip();
}

static void mb_delay0_1 (void) {
    mw_prim_drop();
    mw_run();
}

static void mb_delay2_1 (void) {
    mw_prim_drop();
    mw_unpack3();
    mw_run();
}

static void mb_delay3_1 (void) {
    mw_prim_drop();
    mw_unpack4();
    mw_run();
}

static void mb_delay4_1 (void) {
    mw_prim_drop();
    mw_unpack5();
    mw_run();
}

static void mb_force_or2_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    mw_pack2();
    decref(var_f);
}

static void mb_type_elab_stack_assertion_1 (void) {
    mw_prim_drop();
    mw_true();
}

static void mb_elab_type_sig_21__1 (void) {
    mw_prim_drop();
    mw_dup();
    push_ptr("expected type signature\0\0\0\0");
    mw_emit_error_21_();
}

static void mb_elab_type_sig_21__2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_type_sig_21__3 (void) {
    mw_prim_drop();
    mw_swap();
}

static void mb_elab_type_sig_21__4 (void) {
    mw_prim_drop();
    mw_swap();
}

static void mb_elab_type_sig_21__5 (void) {
    mw_prim_drop();
    mw_token_next();
    mw_elab_type_stack_21_();
    {
        VAL d2 = pop_value();
        mw_swap();
        push_value(d2);
    }
}

static void mb_elab_type_sig_21__7 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_T0();
        mw_rotr();
        push_value(d2);
    }
}

static void mb_elab_type_sig_21__6 (void) {
    mw_prim_drop();
    mw_swap();
}

static void mb_elab_type_sig_21__8 (void) {
    mw_prim_drop();
    mw_T0();
    mw_rotr();
}

static void mb_elab_type_sig_21__9 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_type_sig_21__10 (void) {
    mw_prim_drop();
    mw_dup();
    push_ptr("expected right paren or comma\0\0\0\0");
    mw_emit_error_21_();
}

static void mb_elab_type_sig_21__11 (void) {
    mw_prim_drop();
    mw_rot4r();
    {
        VAL d2 = pop_value();
        mw_swap();
        push_u64(0);
        push_fnptr(&mb_elab_type_sig_21__13);
        mw_prim_pack_cons();
        mw_for();
        push_value(d2);
    }
    mw_T__3E_();
}

static void mb_elab_type_sig_21__12 (void) {
    mw_prim_drop();
    mw_swap();
    push_u64(0);
    push_fnptr(&mb_elab_type_sig_21__13);
    mw_prim_pack_cons();
    mw_for();
}

static void mb_elab_type_sig_21__13 (void) {
    mw_prim_drop();
    mw_T_2A_();
}

static void mb_elab_type_sig_params_21__1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_token_next();
    {
        VAL d2 = pop_value();
        mw_L0();
        mw_rotr();
        mw_token_args();
        push_u64(0);
        push_fnptr(&mb_elab_type_sig_params_21__3);
        mw_prim_pack_cons();
        mw_for();
        mw_swap();
        push_value(d2);
    }
}

static void mb_elab_type_sig_params_21__5 (void) {
    mw_prim_drop();
    mw_L0();
    mw_swap();
}

static void mb_elab_type_sig_params_21__2 (void) {
    mw_prim_drop();
    mw_L0();
    mw_rotr();
    mw_token_args();
    push_u64(0);
    push_fnptr(&mb_elab_type_sig_params_21__3);
    mw_prim_pack_cons();
    mw_for();
    mw_swap();
}

static void mb_elab_type_sig_params_21__3 (void) {
    mw_prim_drop();
    mw_elab_type_sig_21_();
    mw_drop();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_snoc();
        push_value(d2);
    }
}

static void mb_elab_type_sig_params_21__4 (void) {
    mw_prim_drop();
    mw_snoc();
}

static void mb_elab_type_stack_21__1 (void) {
    mw_prim_drop();
    mw_elab_stack_var_21_();
    {
        VAL d2 = pop_value();
        mw_TVar();
        push_value(d2);
    }
}

static void mb_elab_type_stack_21__3 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_TYPE_UNIT();
        push_value(d2);
    }
}

static void mb_elab_type_stack_21__2 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_elab_type_stack_21__4 (void) {
    mw_prim_drop();
    mw_TYPE_UNIT();
}

static void mb_elab_type_stack_rest_21__1 (void) {
    mw_prim_drop();
    mw_sig_is_stack_end2_3F_();
    mw_not();
}

static void mb_elab_type_stack_rest_21__2 (void) {
    mw_prim_drop();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_elab_type_atom_21_();
        push_value(d2);
    }
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_swap();
        mw_TTensor();
        push_value(d2);
    }
}

static void mb_elab_type_stack_rest_21__3 (void) {
    mw_prim_drop();
    mw_elab_type_atom_21_();
}

static void mb_elab_type_stack_rest_21__4 (void) {
    mw_prim_drop();
    mw_swap();
    mw_TTensor();
}

static void mb_elab_type_atom_21__1 (void) {
    mw_prim_drop();
    mw_elab_type_var_21_();
    {
        VAL d2 = pop_value();
        mw_TVar();
        push_value(d2);
    }
}

static void mb_elab_type_atom_21__3 (void) {
    mw_prim_drop();
    mw_sig_token_is_type_con_3F_();
    if (pop_u64()) {
        mw_elab_type_con_21_();
    } else {
        mw_token_is_underscore_3F_();
        if (pop_u64()) {
            mw_elab_type_dont_care_21_();
        } else {
            mw_sig_token_is_type_hole_3F_();
            if (pop_u64()) {
                mw_elab_type_hole_21_();
            } else {
                mw_token_is_lsquare_3F_();
                if (pop_u64()) {
                    mw_elab_type_quote_21_();
                } else {
                    mw_dup();
                    push_ptr("Expected type, got unknown token.\0\0\0\0");
                    mw_emit_error_21_();
                    {
                        VAL d6 = pop_value();
                        mw_TYPE_ERROR();
                        push_value(d6);
                    }
                    mw_token_next();
                }
            }
        }
    }
}

static void mb_elab_type_atom_21__2 (void) {
    mw_prim_drop();
    mw_TVar();
}

static void mb_elab_type_atom_21__4 (void) {
    mw_prim_drop();
    mw_elab_type_con_21_();
}

static void mb_elab_type_atom_21__5 (void) {
    mw_prim_drop();
    mw_token_is_underscore_3F_();
    if (pop_u64()) {
        mw_elab_type_dont_care_21_();
    } else {
        mw_sig_token_is_type_hole_3F_();
        if (pop_u64()) {
            mw_elab_type_hole_21_();
        } else {
            mw_token_is_lsquare_3F_();
            if (pop_u64()) {
                mw_elab_type_quote_21_();
            } else {
                mw_dup();
                push_ptr("Expected type, got unknown token.\0\0\0\0");
                mw_emit_error_21_();
                {
                    VAL d5 = pop_value();
                    mw_TYPE_ERROR();
                    push_value(d5);
                }
                mw_token_next();
            }
        }
    }
}

static void mb_elab_type_atom_21__6 (void) {
    mw_prim_drop();
    mw_elab_type_dont_care_21_();
}

static void mb_elab_type_atom_21__7 (void) {
    mw_prim_drop();
    mw_sig_token_is_type_hole_3F_();
    if (pop_u64()) {
        mw_elab_type_hole_21_();
    } else {
        mw_token_is_lsquare_3F_();
        if (pop_u64()) {
            mw_elab_type_quote_21_();
        } else {
            mw_dup();
            push_ptr("Expected type, got unknown token.\0\0\0\0");
            mw_emit_error_21_();
            {
                VAL d4 = pop_value();
                mw_TYPE_ERROR();
                push_value(d4);
            }
            mw_token_next();
        }
    }
}

static void mb_elab_type_atom_21__8 (void) {
    mw_prim_drop();
    mw_elab_type_hole_21_();
}

static void mb_elab_type_atom_21__9 (void) {
    mw_prim_drop();
    mw_token_is_lsquare_3F_();
    if (pop_u64()) {
        mw_elab_type_quote_21_();
    } else {
        mw_dup();
        push_ptr("Expected type, got unknown token.\0\0\0\0");
        mw_emit_error_21_();
        {
            VAL d3 = pop_value();
            mw_TYPE_ERROR();
            push_value(d3);
        }
        mw_token_next();
    }
}

static void mb_elab_type_atom_21__10 (void) {
    mw_prim_drop();
    mw_elab_type_quote_21_();
}

static void mb_elab_type_atom_21__11 (void) {
    mw_prim_drop();
    mw_dup();
    push_ptr("Expected type, got unknown token.\0\0\0\0");
    mw_emit_error_21_();
    {
        VAL d2 = pop_value();
        mw_TYPE_ERROR();
        push_value(d2);
    }
    mw_token_next();
}

static void mb_elab_type_atom_21__12 (void) {
    mw_prim_drop();
    mw_TYPE_ERROR();
}

static void mb_elab_type_arg_21__1 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_type_arg_21__2 (void) {
    mw_prim_drop();
    push_ptr("Unexpected token after type.\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_type_con_21__2 (void) {
    mw_prim_drop();
    mw_elab_type_args_21_();
}

static void mb_elab_type_con_21__3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_dup();
    push_ptr("Wrong number of arguments for type.\0\0\0\0");
    mw_emit_error_21_();
    mw_TYPE_ERROR();
}

static void mb_elab_type_dont_care_21__1 (void) {
    mw_prim_drop();
    mw_token_has_args_3F_();
    if (pop_u64()) {
        mw_dup();
        push_ptr("Types with args not yet supported.\0\0\0\0");
        mw_emit_error_21_();
        mw_TYPE_ERROR();
    } else {
        mw_TYPE_DONT_CARE();
    }
    mw_swap();
    mw_token_next();
}

static void mb_elab_type_dont_care_21__4 (void) {
    mw_prim_drop();
    push_ptr("type don't care is not allowed here\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_type_dont_care_21__2 (void) {
    mw_prim_drop();
    mw_dup();
    push_ptr("Types with args not yet supported.\0\0\0\0");
    mw_emit_error_21_();
    mw_TYPE_ERROR();
}

static void mb_elab_type_dont_care_21__3 (void) {
    mw_prim_drop();
    mw_TYPE_DONT_CARE();
}

static void mb_elab_type_hole_21__1 (void) {
    mw_prim_drop();
    mw_token_has_args_3F_();
    if (pop_u64()) {
        mw_dup();
        push_ptr("Types with args not yet supported.\0\0\0\0");
        mw_emit_error_21_();
        mw_TYPE_ERROR();
    } else {
        mw_token_name_3F_();
        mw_THole();
    }
    mw_swap();
    mw_token_next();
}

static void mb_elab_type_hole_21__4 (void) {
    mw_prim_drop();
    push_ptr("type holes are not allowed here\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_type_hole_21__2 (void) {
    mw_prim_drop();
    mw_dup();
    push_ptr("Types with args not yet supported.\0\0\0\0");
    mw_emit_error_21_();
    mw_TYPE_ERROR();
}

static void mb_elab_type_hole_21__3 (void) {
    mw_prim_drop();
    mw_token_name_3F_();
    mw_THole();
}

static void mb_elab_type_quote_21__1 (void) {
    mw_prim_drop();
    mw_elab_type_sig_21_();
}

static void mb_elab_type_quote_21__2 (void) {
    mw_prim_drop();
    mw_elab_type_stack_21_();
}

static void mb_elab_implicit_var_21__1 (void) {
    mw_prim_drop();
    mw_type_elab_ctx_3F_();
}

static void mb_elab_implicit_var_21__2 (void) {
    mw_prim_drop();
    mw_token_name_40_();
    mw_dup2();
    mw_swap();
    mw_ctx_lookup();
}

static void mb_elab_implicit_var_21__4 (void) {
    mw_prim_drop();
    mw_nip();
    mw_dup();
    mw_var_type();
    mw__40_();
}

static void mb_elab_implicit_var_21__6 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_var_new_implicit_21_();
        push_value(d2);
    }
    mw_over();
    mw_var_type();
    mw__21_();
    push_u64(0);
    push_fnptr(&mb_elab_implicit_var_21__8);
    mw_prim_pack_cons();
    mw_sip();
}

static void mb_elab_implicit_var_21__7 (void) {
    mw_prim_drop();
    mw_var_new_implicit_21_();
}

static void mb_elab_implicit_var_21__8 (void) {
    mw_prim_drop();
    mw_ctx_new_21_();
}

static void mb_elab_implicit_var_21__9 (void) {
    mw_prim_drop();
    mw_type_elab_ctx_replace();
}

static void mb_ctx_lookup_1 (void) {
    mw_prim_drop();
    mw_dup2();
    mw_var_name();
    mw__40_();
    mw__3D__3D_();
}

static void mb_elab_type_unify_21__1 (void) {
    mw_prim_drop();
    mw_GAMMA();
    mw_rotr();
    mw_type_unify_21_();
    mw_nip();
}

static void mb_ctx_new_21__1 (void) {
    mw_prim_drop();
    mw_unCTX();
}

static void mb_elab_type_args_21__1 (void) {
    mw_prim_drop();
    mw_token_has_args_3F_();
}

static void mb_elab_type_args_21__2 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_tuck();
        push_value(d2);
    }
    mw_swap();
    mw_token_succ();
    while(1) {
        mw_token_is_right_enclosure_3F_();
        mw_not();
        if (!pop_u64()) break;
        mw_token_succ();
        mw_swap();
        {
            VAL d3 = pop_value();
            mw_elab_type_arg_21_();
            push_value(d3);
        }
        mw_swap();
        {
            VAL d3 = pop_value();
            mw_swap();
            mw_TApp();
            push_value(d3);
        }
    }
    mw_drop();
    {
        VAL d2 = pop_value();
        mw_swap();
        push_value(d2);
    }
}

static void mb_elab_type_args_21__9 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_type_args_21__3 (void) {
    mw_prim_drop();
    mw_tuck();
}

static void mb_elab_type_args_21__4 (void) {
    mw_prim_drop();
    mw_token_is_right_enclosure_3F_();
    mw_not();
}

static void mb_elab_type_args_21__5 (void) {
    mw_prim_drop();
    mw_token_succ();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_elab_type_arg_21_();
        push_value(d2);
    }
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_swap();
        mw_TApp();
        push_value(d2);
    }
}

static void mb_elab_type_args_21__6 (void) {
    mw_prim_drop();
    mw_elab_type_arg_21_();
}

static void mb_elab_type_args_21__7 (void) {
    mw_prim_drop();
    mw_swap();
    mw_TApp();
}

static void mb_elab_type_args_21__8 (void) {
    mw_prim_drop();
    mw_swap();
}

static void mb_elab_simple_type_arg_21__1 (void) {
    mw_prim_drop();
    mw_type_elab_default();
}

static void mb_ab_save_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_ab_build_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_Arrow_2E_alloc_21_();
    mw_ab_home();
    mw__40_();
    mw_over();
    mw_arrow_home();
    mw__21_();
    mw_ab_homeidx();
    mw__40_();
    mw_over();
    mw_arrow_homeidx();
    mw__21_();
    mw_ab_homeidx();
    push_u64(0);
    push_value(var_f);
    incref(var_f);
    mw_prim_pack_cons();
    push_fnptr(&mb_ab_build_21__3);
    mw_prim_pack_cons();
    mw_modify();
    mw_tuck();
    mw_dup2();
    mw_arrow_token_start();
    mw__21_();
    mw_arrow_token_end();
    mw__21_();
    mw_tuck();
    mw_dup2();
    mw_arrow_dom();
    mw__21_();
    mw_arrow_cod();
    mw__21_();
    mw_tuck();
    mw_arrow_ctx();
    mw__21_();
    mw_ab_arrow();
    mw__21_();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    mw_ab_arrow();
    mw__40_();
    decref(var_f);
}

static void mb_ab_build_21__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_1_2B_();
    decref(var_f);
}

static void mb_ab_build_hom_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_rotr();
    decref(var_f);
}

static void mb_ab_build_hom_21__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    mw_ab_unify_type_21_();
    decref(var_f);
}

static void mb_ab_unify_type_21__1 (void) {
    mw_prim_drop();
    mw_ab_token();
    mw__40_();
    mw_GAMMA();
    mw_ab_type();
    mw__40_();
}

static void mb_ab_build_word_arrow_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_elab_word_ctx_sig_weak_21_();
    decref(var_f);
}

static void mb_ab_build_word_arrow_21__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_ab_build_word_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_u64(0);
    push_value(var_f);
    incref(var_f);
    mw_prim_pack_cons();
    push_fnptr(&mb_ab_build_word_21__3);
    mw_prim_pack_cons();
    mw_ab_build_word_arrow_21_();
    mw_ready();
    decref(var_f);
}

static void mb_ab_build_word_21__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    decref(var_f);
}

static void mb_ab_build_word_21__4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_ab_atom_21__1 (void) {
    mw_prim_drop();
    mw_ab_arrow();
    mw__40_();
    mw_arrow_atoms();
    mw__40_();
}

static void mb_ab_optimized_snoc_21__1 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_ab_optimized_snoc_21__2);
    mw_prim_pack_cons();
    mw_dip_3F_();
    push_u64(0);
    push_fnptr(&mb_ab_optimized_snoc_21__3);
    mw_prim_pack_cons();
    mw_and();
}

static void mb_ab_optimized_snoc_21__4 (void) {
    mw_prim_drop();
    mw_swap();
    mw_atoms_turn_last_block_to_arg();
    mw_swap();
}

static void mb_ab_optimized_snoc_21__2 (void) {
    mw_prim_drop();
    mw_atoms_has_last_block_3F_();
}

static void mb_ab_optimized_snoc_21__3 (void) {
    mw_prim_drop();
    mw_atom_accepts_args_3F_();
}

static void mb_atom_accepts_args_3F__2 (void) {
    mw_prim_drop();
    mw_dup();
    mw_atom_args();
    mw__40_();
    mw_len();
}

static void mb_atoms_turn_last_block_to_arg_4 (void) {
    mw_prim_drop();
    mw_atom_cod();
    mw__40_();
    mw_rotl();
    mw_tuck();
    mw_atom_dom();
    mw__21_();
}

static void mb_atom_to_run_var_2 (void) {
    mw_prim_drop();
    mw_SOME();
}

static void mb_atom_to_run_var_3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_NONE();
}

static void mb_ab_op_21__1 (void) {
    mw_prim_drop();
    mw_over();
    mw_atom_subst();
    mw__21_();
}

static void mb_ab_op_21__2 (void) {
    mw_prim_drop();
    mw_over();
    mw_atom_dom();
    mw__21_();
}

static void mb_ab_expand_opsig_21__3 (void) {
    mw_prim_drop();
    mw_ab_type();
    mw__40_();
    mw_dup();
}

static void mb_ab_expand_opsig_21__5 (void) {
    mw_prim_drop();
    mw_ab_type();
    mw__40_();
}

static void mb_ab_expand_opsig_21__6 (void) {
    mw_prim_drop();
    mw_elab_type_unify_21_();
    mw_drop();
}

static void mb_ab_prim_21__1 (void) {
    mw_prim_drop();
    mw_ab_token();
    mw__40_();
    push_ptr("compiler error: prim type missing\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_ab_prim_21__2 (void) {
    mw_prim_drop();
    mw_OP_PRIM();
    mw_ab_op_21_();
}

static void mb_ab_block_at_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_ab_block_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_ab_dip_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_ab_if_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
    decref(var_g);
}

static void mb_ab_if_21__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    push_value(var_g);
    incref(var_g);
    mw_prim_run();
    decref(var_f);
    decref(var_g);
}

static void mb_ab_while_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
    decref(var_g);
}

static void mb_ab_while_21__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_pack_uncons();
    VAL var_g = pop_value();
    mw_prim_drop();
    push_value(var_g);
    incref(var_g);
    mw_prim_run();
    decref(var_f);
    decref(var_g);
}

static void mb_ab_lambda_21__2 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_ab_ctx();
    mw__40_();
    mw_ab_type();
    mw__40_();
    mw_rotl();
    push_u64(0);
    push_value(var_f);
    incref(var_f);
    mw_prim_pack_cons();
    push_fnptr(&mb_ab_lambda_21__3);
    mw_prim_pack_cons();
    mw_reverse_for();
    decref(var_f);
}

static void mb_ab_lambda_21__3 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_swap();
    {
        VAL d2 = pop_value();
        mw_Param__3E_Var();
        mw_dup();
        {
            VAL d3 = pop_value();
            mw_ctx_new_21_();
            push_value(d3);
        }
        push_value(d2);
    }
    mw_ab_token();
    mw__40_();
    mw_elab_expand_tensor_21_();
    {
        VAL d2 = pop_value();
        mw_rotl();
        mw_var_type();
        mw__40_();
        push_value(d2);
    }
    mw_elab_type_unify_21_();
    mw_drop2();
    decref(var_f);
}

static void mb_ab_lambda_21__4 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_Param__3E_Var();
    mw_dup();
    {
        VAL d2 = pop_value();
        mw_ctx_new_21_();
        push_value(d2);
    }
    decref(var_f);
}

static void mb_ab_lambda_21__5 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_ctx_new_21_();
    decref(var_f);
}

static void mb_ab_lambda_21__6 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    mw_rotl();
    mw_var_type();
    mw__40_();
    decref(var_f);
}

static void mb_ab_lambda_21__7 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        push_value(var_f);
        incref(var_f);
        mw_prim_run();
        push_value(d2);
    }
    mw_ab_type();
    mw__40_();
    mw_over();
    mw_lambda_cod();
    mw__21_();
    decref(var_f);
}

static void mb_ab_lambda_21__8 (void) {
    mw_prim_pack_uncons();
    VAL var_f = pop_value();
    mw_prim_drop();
    push_value(var_f);
    incref(var_f);
    mw_prim_run();
    decref(var_f);
}

static void mb_elab_expand_tensor_21__2 (void) {
    mw_prim_drop();
    mw_TYPE_ERROR();
    mw_TYPE_ERROR();
}

static void mb_elab_expand_tensor_21__5 (void) {
    mw_prim_drop();
    mw_meta_alloc_21_();
    mw_TMeta();
    mw_meta_alloc_21_();
    mw_TMeta();
    mw_dup2();
    mw_T_2A_();
    mw_SOME();
}

static void mb_elab_expand_tensor_21__7 (void) {
    mw_prim_drop();
    mw_TYPE_ERROR();
    mw_TYPE_ERROR();
}

static void mb_elab_field_type_21__1 (void) {
    mw_prim_drop();
    mw_field_index_type();
    mw_force_21_();
    mw_T1();
}

static void mb_elab_var_sig_21__1 (void) {
    mw_prim_drop();
    mw_var_type();
    mw__40_();
    mw_type_semifreshen_sig();
    mw_OPSIG_APPLY();
}

static void mb_elab_var_sig_21__2 (void) {
    mw_prim_drop();
    mw_var_type();
    mw__40_();
    mw_OPSIG_PUSH();
}

static void mb_elab_match_sig_21__1 (void) {
    mw_prim_drop();
    mw_match_dom();
    mw__40_();
}

static void mb_elab_lambda_sig_21__1 (void) {
    mw_prim_drop();
    mw_lambda_dom();
    mw__40_();
}

static void mb_elab_word_ctx_sig_21__1 (void) {
    mw_prim_drop();
    mw_emit_recursive_word_fatal_error_21_();
}

static void mb_elab_arrow_fwd_21__1 (void) {
    mw_prim_drop();
    mw_elab_atoms_21_();
}

static void mb_elab_atoms_21__1 (void) {
    mw_prim_drop();
    mw_elab_atoms_done_3F_();
    mw_not();
}

static void mb_elab_atoms_21__2 (void) {
    mw_prim_drop();
    mw_elab_atom_21_();
    mw_ab_token();
    push_u64(0);
    push_fnptr(&mb_elab_atoms_21__3);
    mw_prim_pack_cons();
    mw_modify();
}

static void mb_elab_atoms_21__3 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_args_21__1 (void) {
    mw_prim_drop();
    mw_elab_block_at_21_();
}

static void mb_elab_match_cases_21__1 (void) {
    mw_prim_drop();
    mw_token_is_rparen_3F_();
    mw_not();
}

static void mb_elab_match_cases_21__2 (void) {
    mw_prim_drop();
    mw_elab_match_case_21_();
}

static void mb_elab_match_exhaustive_21__1 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_match_exhaustive_21__2 (void) {
    mw_prim_drop();
    mw_dup();
    mw_match_token();
    mw__40_();
    push_ptr("Pattern match not exhaustive.\0\0\0\0");
    mw_emit_error_21_();
}

static void mb_elab_lambda_params_21__1 (void) {
    mw_prim_drop();
    mw_token_is_lambda_param_3F_();
}

static void mb_elab_lambda_params_21__2 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_lambda_params_21__3 (void) {
    mw_prim_drop();
    mw_token_is_lambda_param_3F_();
}

static void mb_elab_lambda_params_21__4 (void) {
    mw_prim_drop();
    mw_elab_lambda_pop_from_mid_21_();
    push_u64(0);
    push_fnptr(&mb_elab_lambda_params_21__5);
    mw_prim_pack_cons();
    mw_sip();
    mw_token_prev();
}

static void mb_elab_lambda_params_21__5 (void) {
    mw_prim_drop();
    mw_sig_token_is_type_var_3F_();
    if (pop_u64()) {
        mw_token_name_40_();
        mw_var_new_21_();
        mw_tuck();
        mw_var_type();
        mw__21_();
    } else {
        mw_token_succ();
        {
            VAL d3 = pop_value();
            mw_type_expand();
            mw_type_is_morphism_3F_();
            push_value(d3);
        }
        mw_swap();
        if (pop_u64()) {
            mw_token_name_40_();
            mw_var_new_21_();
            mw_true();
            mw_over();
            mw_var_auto_run();
            mw__21_();
            mw_tuck();
            mw_var_type();
            mw__21_();
        } else {
            push_ptr("block pattern on non-block argument\0\0\0\0");
            mw_emit_fatal_error_21_();
        }
    }
    {
        VAL d2 = pop_value();
        mw_dup();
        mw_lambda_params();
        mw__40_();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_elab_lambda_params_21__12);
    mw_prim_pack_cons();
    mw_sip();
    {
        VAL d2 = pop_value();
        mw_dup();
        mw_lambda_inner_ctx();
        mw__40_();
        push_value(d2);
    }
    mw_ctx_new_21_();
    mw_over();
    mw_lambda_inner_ctx();
    mw__21_();
}

static void mb_elab_lambda_params_21__6 (void) {
    mw_prim_drop();
    mw_token_name_40_();
    mw_var_new_21_();
    mw_tuck();
    mw_var_type();
    mw__21_();
}

static void mb_elab_lambda_params_21__7 (void) {
    mw_prim_drop();
    mw_token_succ();
    {
        VAL d2 = pop_value();
        mw_type_expand();
        mw_type_is_morphism_3F_();
        push_value(d2);
    }
    mw_swap();
    if (pop_u64()) {
        mw_token_name_40_();
        mw_var_new_21_();
        mw_true();
        mw_over();
        mw_var_auto_run();
        mw__21_();
        mw_tuck();
        mw_var_type();
        mw__21_();
    } else {
        push_ptr("block pattern on non-block argument\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mb_elab_lambda_params_21__8 (void) {
    mw_prim_drop();
    mw_type_expand();
    mw_type_is_morphism_3F_();
}

static void mb_elab_lambda_params_21__9 (void) {
    mw_prim_drop();
    mw_token_name_40_();
    mw_var_new_21_();
    mw_true();
    mw_over();
    mw_var_auto_run();
    mw__21_();
    mw_tuck();
    mw_var_type();
    mw__21_();
}

static void mb_elab_lambda_params_21__10 (void) {
    mw_prim_drop();
    push_ptr("block pattern on non-block argument\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_lambda_params_21__11 (void) {
    mw_prim_drop();
    mw_dup();
    mw_lambda_params();
    mw__40_();
}

static void mb_elab_lambda_params_21__12 (void) {
    mw_prim_drop();
    mw_Var__3E_Param();
    mw_swap();
    mw_cons();
    mw_over();
    mw_lambda_params();
    mw__21_();
}

static void mb_elab_lambda_params_21__13 (void) {
    mw_prim_drop();
    mw_dup();
    mw_lambda_inner_ctx();
    mw__40_();
}

static void mb_elab_lambda_body_21__1 (void) {
    mw_prim_drop();
    mw_token_is_lambda_param_3F_();
}

static void mb_elab_lambda_body_21__2 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_lambda_body_21__3 (void) {
    mw_prim_drop();
    mw_dup();
    mw_lambda_mid();
    mw__40_();
    {
        VAL d2 = pop_value();
        mw_dup();
        mw_lambda_inner_ctx();
        mw__40_();
        push_value(d2);
    }
}

static void mb_elab_lambda_body_21__4 (void) {
    mw_prim_drop();
    mw_dup();
    mw_lambda_inner_ctx();
    mw__40_();
}

static void mb_elab_lambda_pop_from_mid_21__1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_lambda_mid();
    mw__40_();
}

static void mb_elab_lambda_pop_from_mid_21__2 (void) {
    mw_prim_drop();
    mw_over();
    mw_lambda_mid();
    mw__21_();
}

static void mb_token_is_lambda_param_3F__1 (void) {
    mw_prim_drop();
    mw_token_has_args_3F_();
    mw_not();
}

static void mb_token_is_lambda_param_3F__2 (void) {
    mw_prim_drop();
    mw_token_is_lsquare_3F_();
    if (pop_u64()) {
        mw_dup();
        mw_true();
        {
            VAL d3 = pop_value();
            mw_token_succ();
            mw_sig_token_is_type_var_3F_();
            push_value(d3);
        }
        mw__26__26_();
        {
            VAL d3 = pop_value();
            mw_token_succ();
            mw_token_is_rsquare_3F_();
            push_value(d3);
        }
        mw__26__26_();
        mw_nip();
    } else {
        mw_false();
    }
}

static void mb_token_is_lambda_param_3F__3 (void) {
    mw_prim_drop();
    mw_dup();
    mw_true();
    {
        VAL d2 = pop_value();
        mw_token_succ();
        mw_sig_token_is_type_var_3F_();
        push_value(d2);
    }
    mw__26__26_();
    {
        VAL d2 = pop_value();
        mw_token_succ();
        mw_token_is_rsquare_3F_();
        push_value(d2);
    }
    mw__26__26_();
    mw_nip();
}

static void mb_token_is_lambda_param_3F__6 (void) {
    mw_prim_drop();
    mw_false();
}

static void mb_token_is_lambda_param_3F__4 (void) {
    mw_prim_drop();
    mw_token_succ();
    mw_sig_token_is_type_var_3F_();
}

static void mb_token_is_lambda_param_3F__5 (void) {
    mw_prim_drop();
    mw_token_succ();
    mw_token_is_rsquare_3F_();
}

static void mb_expect_token_arrow_1 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_expect_token_arrow_2 (void) {
    mw_prim_drop();
    push_ptr("Expected arrow.\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_match_case_21__1 (void) {
    mw_prim_drop();
    mw_dup2();
    mw_case_match();
    mw__21_();
}

static void mb_elab_match_case_21__2 (void) {
    mw_prim_drop();
    mw_match_add_case_21_();
}

static void mb_elab_match_case_21__3 (void) {
    mw_prim_drop();
    mw_token_succ();
}

static void mb_elab_match_case_21__4 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_case_pattern_21__1 (void) {
    mw_prim_drop();
    {
        VAL d2 = pop_value();
        mw_PATTERN_UNDERSCORE();
        mw_over();
        mw_case_pattern();
        mw__21_();
        push_value(d2);
    }
    {
        VAL d2 = pop_value();
        mw_dup();
        mw_case_match();
        mw__40_();
        mw_match_dom();
        mw__40_();
        mw_TYPE_DONT_CARE();
        mw_TYPE_DONT_CARE();
        mw_T_2A_();
        push_value(d2);
    }
    mw_elab_type_unify_21_();
    {
        VAL d2 = pop_value();
        mw_over();
        mw_case_mid();
        mw__21_();
        push_value(d2);
    }
    mw_token_succ();
}

static void mb_elab_case_pattern_21__5 (void) {
    mw_prim_drop();
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_token_name_3F_();
        mw_name_def();
        mw__40_();
        switch (get_top_data_tag()) {
            case 3LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_dup();
                mw_PATTERN_TAG();
                mw_rotr();
                push_u64(0);
                push_fnptr(&mb_elab_case_pattern_21__8);
                mw_prim_pack_cons();
                mw_dip2();
                push_u64(0);
                push_fnptr(&mb_elab_case_pattern_21__9);
                mw_prim_pack_cons();
                mw_dip2();
                mw_elab_tag_sig_21_();
                mw_subst_nil();
                mw_swap();
                mw_type_freshen_sig();
                mw_rotr();
                {
                    VAL d5 = pop_value();
                    mw_elab_expand_morphism_21_();
                    push_u64(0);
                    push_fnptr(&mb_elab_case_pattern_21__11);
                    mw_prim_pack_cons();
                    mw_dip2();
                    mw_elab_type_unify_21_();
                    mw_nip();
                    {
                        VAL d6 = pop_value();
                        mw_over();
                        mw_case_mid();
                        mw__21_();
                        push_value(d6);
                    }
                    push_value(d5);
                }
                mw_swap();
                {
                    VAL d5 = pop_value();
                    mw_over();
                    mw_case_subst();
                    mw__21_();
                    push_value(d5);
                }
                mw_token_succ();
                break;
            case 0LL:
                mw_prim_drop();
                push_ptr("Unknown constructor.\0\0\0\0");
                mw_emit_fatal_error_21_();
                break;
            default:
                mw_drop();
                push_ptr("Not a constructor.\0\0\0\0");
                mw_emit_fatal_error_21_();
                break;
        
}    } else {
        push_ptr("Expected constructor name.\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mb_elab_case_pattern_21__2 (void) {
    mw_prim_drop();
    mw_PATTERN_UNDERSCORE();
    mw_over();
    mw_case_pattern();
    mw__21_();
}

static void mb_elab_case_pattern_21__3 (void) {
    mw_prim_drop();
    mw_dup();
    mw_case_match();
    mw__40_();
    mw_match_dom();
    mw__40_();
    mw_TYPE_DONT_CARE();
    mw_TYPE_DONT_CARE();
    mw_T_2A_();
}

static void mb_elab_case_pattern_21__4 (void) {
    mw_prim_drop();
    mw_over();
    mw_case_mid();
    mw__21_();
}

static void mb_elab_case_pattern_21__6 (void) {
    mw_prim_drop();
    mw_token_name_3F_();
    mw_name_def();
    mw__40_();
    switch (get_top_data_tag()) {
        case 3LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_dup();
            mw_PATTERN_TAG();
            mw_rotr();
            push_u64(0);
            push_fnptr(&mb_elab_case_pattern_21__8);
            mw_prim_pack_cons();
            mw_dip2();
            push_u64(0);
            push_fnptr(&mb_elab_case_pattern_21__9);
            mw_prim_pack_cons();
            mw_dip2();
            mw_elab_tag_sig_21_();
            mw_subst_nil();
            mw_swap();
            mw_type_freshen_sig();
            mw_rotr();
            {
                VAL d4 = pop_value();
                mw_elab_expand_morphism_21_();
                push_u64(0);
                push_fnptr(&mb_elab_case_pattern_21__11);
                mw_prim_pack_cons();
                mw_dip2();
                mw_elab_type_unify_21_();
                mw_nip();
                {
                    VAL d5 = pop_value();
                    mw_over();
                    mw_case_mid();
                    mw__21_();
                    push_value(d5);
                }
                push_value(d4);
            }
            mw_swap();
            {
                VAL d4 = pop_value();
                mw_over();
                mw_case_subst();
                mw__21_();
                push_value(d4);
            }
            mw_token_succ();
            break;
        case 0LL:
            mw_prim_drop();
            push_ptr("Unknown constructor.\0\0\0\0");
            mw_emit_fatal_error_21_();
            break;
        default:
            mw_drop();
            push_ptr("Not a constructor.\0\0\0\0");
            mw_emit_fatal_error_21_();
            break;
    
}}

static void mb_elab_case_pattern_21__16 (void) {
    mw_prim_drop();
    push_ptr("Expected constructor name.\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_case_pattern_21__8 (void) {
    mw_prim_drop();
    mw_over();
    mw_case_pattern();
    mw__21_();
}

static void mb_elab_case_pattern_21__9 (void) {
    mw_prim_drop();
    mw_dup();
    mw_case_match();
    mw__40_();
    mw_match_dom();
    mw__40_();
}

static void mb_elab_case_pattern_21__10 (void) {
    mw_prim_drop();
    mw_elab_expand_morphism_21_();
    push_u64(0);
    push_fnptr(&mb_elab_case_pattern_21__11);
    mw_prim_pack_cons();
    mw_dip2();
    mw_elab_type_unify_21_();
    mw_nip();
    {
        VAL d2 = pop_value();
        mw_over();
        mw_case_mid();
        mw__21_();
        push_value(d2);
    }
}

static void mb_elab_case_pattern_21__11 (void) {
    mw_prim_drop();
    mw_swap();
}

static void mb_elab_case_pattern_21__12 (void) {
    mw_prim_drop();
    mw_over();
    mw_case_mid();
    mw__21_();
}

static void mb_elab_case_pattern_21__13 (void) {
    mw_prim_drop();
    mw_over();
    mw_case_subst();
    mw__21_();
}

static void mb_elab_case_body_21__1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_case_mid();
    mw__40_();
    {
        VAL d2 = pop_value();
        mw_dup();
        mw_case_match();
        mw__40_();
        mw_match_ctx();
        mw__40_();
        push_value(d2);
    }
}

static void mb_elab_case_body_21__2 (void) {
    mw_prim_drop();
    mw_dup();
    mw_case_match();
    mw__40_();
    mw_match_ctx();
    mw__40_();
}

static void mb_elab_case_body_21__3 (void) {
    mw_prim_drop();
    mw_dup();
    mw_arrow_cod();
    mw__40_();
}

static void mb_elab_case_body_21__4 (void) {
    mw_prim_drop();
    mw_over();
    mw_case_body();
    mw__21_();
}

static void mb_elab_case_body_21__5 (void) {
    mw_prim_drop();
    mw_dup();
    mw_case_match();
    mw__40_();
    mw_match_cod();
    mw__40_();
}

static void mb_elab_module_header_21__1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_token_args_1();
    mw_token_is_name_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("Expected module name.\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_token_name_3F_();
    mw_name_defined_3F_();
    if (pop_u64()) {
        mw_drop();
        push_ptr("Module name already taken.\0\0\0\0");
        mw_emit_fatal_error_21_();
    } else {
        mw_id();
    }
    mw_over();
    mw_token_module();
    mw__40_();
    mw_dup2();
    mw_module_name();
    mw__21_();
    mw_dup2();
    mw_DEF_MODULE();
    mw_swap();
    mw_name_def();
    mw__21_();
    mw_module_path();
    mw__40_();
    mw_Path__3E_Str();
    mw_swap();
    mw_module_path_from_name();
    mw_Path__3E_Str();
    mw_str_eq();
    if (pop_u64()) {
        mw_drop();
    } else {
        push_ptr("Module name should match path.\0\0\0\0");
        mw_emit_error_21_();
    }
    mw_token_next();
}

static void mb_elab_module_header_21__8 (void) {
    mw_prim_drop();
    mw_dup();
    push_ptr("Expected module header.\0\0\0\0");
    mw_emit_error_21_();
}

static void mb_elab_module_header_21__2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_module_header_21__3 (void) {
    mw_prim_drop();
    push_ptr("Expected module name.\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_module_header_21__4 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("Module name already taken.\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_module_header_21__5 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_module_header_21__6 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_elab_module_header_21__7 (void) {
    mw_prim_drop();
    push_ptr("Module name should match path.\0\0\0\0");
    mw_emit_error_21_();
}

static void mb_elab_module_decl_21__3 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("unknown declaration\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_module_decl_21__4 (void) {
    mw_prim_drop();
    mw_run();
}

static void mb_elab_module_import_21__1 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_module_import_21__4 (void) {
    mw_prim_drop();
    mw_drop2();
    mw_dup();
    mw_token_module();
    mw__40_();
}

static void mb_elab_module_import_21__6 (void) {
    mw_prim_drop();
    mw_drop();
    mw_dup();
    mw_token_module();
    mw__40_();
}

static void mb_elab_data_21__1 (void) {
    mw_prim_drop();
    mw_Data_2E_alloc_21_();
    mw_swap();
    mw_token_args_2B_();
    mw_uncons();
    {
        VAL d2 = pop_value();
        mw_elab_data_header_21_();
        push_value(d2);
    }
    push_u64(0);
    push_fnptr(&mb_elab_data_21__3);
    mw_prim_pack_cons();
    mw_for();
    mw_drop();
}

static void mb_elab_data_21__2 (void) {
    mw_prim_drop();
    mw_elab_data_header_21_();
}

static void mb_elab_data_21__3 (void) {
    mw_prim_drop();
    mw_elab_data_tag_21_();
}

static void mb_elab_data_header_21__1 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_data_header_21__2 (void) {
    mw_prim_drop();
    push_ptr("Expected type name.\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_data_header_21__3 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_data_header_21__4 (void) {
    mw_prim_drop();
    mw_drop2();
    push_ptr("Name already defined.\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_data_tag_21__1 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_data_tag_21__2 (void) {
    mw_prim_drop();
    push_ptr("Expected constructor name.\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_data_tag_21__3 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_data_tag_21__4 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("Name already defined. (Overlapping tags not supported.)\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_data_tag_21__5 (void) {
    mw_prim_drop();
    mw_over();
}

static void mb_elab_data_tag_21__6 (void) {
    mw_prim_drop();
    mw_data_add_tag_21_();
}

static void mb_elab_data_tag_21__7 (void) {
    mw_prim_drop();
    mw_token_succ();
    mw_SOME();
    mw_over();
    mw_tag_sig();
    mw__21_();
}

static void mb_elab_data_tag_21__8 (void) {
    mw_prim_drop();
    mw_token_run_end_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_NONE();
        mw_over();
        mw_tag_sig();
        mw__21_();
    } else {
        push_ptr("Expected arrow, comma, or right paren.\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mb_elab_data_tag_21__9 (void) {
    mw_prim_drop();
    mw_drop();
    mw_NONE();
    mw_over();
    mw_tag_sig();
    mw__21_();
}

static void mb_elab_data_tag_21__10 (void) {
    mw_prim_drop();
    push_ptr("Expected arrow, comma, or right paren.\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_data_tag_21__11 (void) {
    mw_prim_drop();
    mw_type_elab_default();
    mw_over();
    mw_tag_data();
    mw__40_();
    mw_data_header();
    mw__40_();
    mw_elab_type_atom_21_();
    mw_drop();
    mw_T1();
    {
        VAL d2 = pop_value();
        mw_T0();
        mw_rotl();
        mw_tag_sig();
        mw__40_();
        switch (get_top_data_tag()) {
            case 0LL:
                mw_prim_drop();
                mw_id();
                break;
            case 1LL:
                mw_prim_pack_uncons(); mw_prim_drop();
                mw_elab_type_stack_rest_21_();
                mw_token_run_end_3F_();
                if (pop_u64()) {
                    mw_drop();
                } else {
                    push_ptr("syntax error\0\0\0\0");
                    mw_emit_fatal_error_21_();
                }
                break;
            default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
        
}        push_value(d2);
    }
    mw_T__3E_();
    {
        VAL d2 = pop_value();
        mw_type_elab_ctx();
        push_value(d2);
    }
    mw_pack2();
}

static void mb_elab_data_tag_21__12 (void) {
    mw_prim_drop();
    mw_T0();
    mw_rotl();
    mw_tag_sig();
    mw__40_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_id();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_type_stack_rest_21_();
            mw_token_run_end_3F_();
            if (pop_u64()) {
                mw_drop();
            } else {
                push_ptr("syntax error\0\0\0\0");
                mw_emit_fatal_error_21_();
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}}

static void mb_elab_data_tag_21__15 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_elab_data_tag_21__16 (void) {
    mw_prim_drop();
    push_ptr("syntax error\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_data_tag_21__17 (void) {
    mw_prim_drop();
    mw_type_elab_ctx();
}

static void mb_expect_token_comma_1 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_expect_token_comma_2 (void) {
    mw_prim_drop();
    push_ptr("Expected comma.\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_expect_token_rparen_1 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_expect_token_rparen_2 (void) {
    mw_prim_drop();
    push_ptr("Expected right parenthesis.\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_token_def_args_1 (void) {
    mw_prim_drop();
    mw_nip();
}

static void mb_token_def_args_2 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("def expects at least two arguments\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_token_def_args_3 (void) {
    mw_prim_drop();
    mw_drop();
    mw_L1_2B_();
    {
        VAL d2 = pop_value();
        mw_NONE();
        push_value(d2);
    }
}

static void mb_token_def_args_5 (void) {
    mw_prim_drop();
    mw_over();
    mw_token_run_has_arrow();
    if (pop_u64()) {
        mw_cons_2B_();
        {
            VAL d3 = pop_value();
            mw_NONE();
            push_value(d3);
        }
    } else {
        mw_List__3E_List_2B_();
        mw_unwrap();
        {
            VAL d3 = pop_value();
            mw_SOME();
            push_value(d3);
        }
    }
}

static void mb_token_def_args_4 (void) {
    mw_prim_drop();
    mw_NONE();
}

static void mb_token_def_args_6 (void) {
    mw_prim_drop();
    mw_cons_2B_();
    {
        VAL d2 = pop_value();
        mw_NONE();
        push_value(d2);
    }
}

static void mb_token_def_args_8 (void) {
    mw_prim_drop();
    mw_List__3E_List_2B_();
    mw_unwrap();
    {
        VAL d2 = pop_value();
        mw_SOME();
        push_value(d2);
    }
}

static void mb_token_def_args_7 (void) {
    mw_prim_drop();
    mw_NONE();
}

static void mb_token_def_args_9 (void) {
    mw_prim_drop();
    mw_SOME();
}

static void mb_elab_def_missing_21__1 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_def_missing_21__2 (void) {
    mw_prim_drop();
    mw_elab_def_21_();
}

static void mb_elab_def_21__1 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_def_21__2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_def_21__3 (void) {
    mw_prim_drop();
    mw_dup();
    mw_token_run_has_arrow();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("expected match case\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mb_elab_def_21__4 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_def_21__5 (void) {
    mw_prim_drop();
    push_ptr("expected match case\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_def_21__6 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_def_21__7 (void) {
    mw_prim_drop();
    push_ptr("expected word name\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_def_21__8 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_def_21__9 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("word already defined\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_def_21__10 (void) {
    mw_prim_drop();
    mw_type_elab_default();
    mw_over();
    mw_word_sig();
    mw__40_();
    switch (get_top_data_tag()) {
        case 0LL:
            mw_prim_drop();
            mw_type_elab_ctx();
            mw_over();
            mw_word_arrow();
            push_u64(0);
            push_fnptr(&mb_elab_def_21__12);
            mw_prim_pack_cons();
            mw_force_or_21_();
            mw_arrow_type();
            mw_type_rigidify_sig_21_();
            break;
        case 1LL:
            mw_prim_pack_uncons(); mw_prim_drop();
            mw_elab_type_sig_21_();
            mw_drop();
            {
                VAL d4 = pop_value();
                mw_type_elab_ctx();
                push_value(d4);
            }
            break;
        default: write(2, "unexpected fallthrough in match\n", 32); mw_prim_debug(); exit(99);
    
}    mw_pack2();
    mw_nip();
}

static void mb_elab_def_21__12 (void) {
    mw_prim_drop();
    mw_swap();
    mw_emit_recursive_word_fatal_error_21_();
}

static void mb_elab_def_21__14 (void) {
    mw_prim_drop();
    mw_type_elab_ctx();
}

static void mb_elab_def_21__15 (void) {
    mw_prim_drop();
    mw_elab_def_params_21_();
}

static void mb_elab_def_21__16 (void) {
    mw_prim_drop();
    mw_dup();
    push_u64(0);
    push_fnptr(&mb_elab_def_21__17);
    mw_prim_pack_cons();
    mw_ab_build_word_arrow_21_();
}

static void mb_elab_def_21__17 (void) {
    mw_prim_drop();
    mw_swap();
    mw_word_params();
    mw_force_21_();
    mw_is_nil_3F_();
    if (pop_u64()) {
        mw_drop();
        mw_elab_def_body_21_();
    } else {
        push_u64(0);
        push_fnptr(&mb_elab_def_21__20);
        mw_prim_pack_cons();
        mw_ab_lambda_21_();
    }
}

static void mb_elab_def_21__18 (void) {
    mw_prim_drop();
    mw_drop();
    mw_elab_def_body_21_();
}

static void mb_elab_def_21__19 (void) {
    mw_prim_drop();
    push_u64(0);
    push_fnptr(&mb_elab_def_21__20);
    mw_prim_pack_cons();
    mw_ab_lambda_21_();
}

static void mb_elab_def_21__20 (void) {
    mw_prim_drop();
    mw_elab_def_body_21_();
}

static void mb_elab_def_params_21__1 (void) {
    mw_prim_drop();
    mw_token_is_param_name_3F_();
    if (pop_u64()) {
        mw_id();
    } else {
        push_ptr("expected parameter name\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_dup();
    mw_token_succ();
    mw_token_run_end_3F_();
    if (pop_u64()) {
        mw_drop();
    } else {
        push_ptr("expected right paren or comma\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
    mw_elab_expand_tensor_21_();
    {
        VAL d2 = pop_value();
        mw_dup();
        push_value(d2);
    }
    mw_elab_expand_morphism_21_();
    {
        VAL d2 = pop_value();
        mw_drop2();
        push_value(d2);
    }
    mw_token_name_40_();
    mw_var_new_21_();
    mw_tuck();
    mw_var_type();
    mw__21_();
    mw_true();
    mw_over();
    mw_var_auto_run();
    mw__21_();
    mw_PARAM();
    mw_rotr();
    {
        VAL d2 = pop_value();
        mw_cons();
        push_value(d2);
    }
}

static void mb_elab_def_params_21__2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_def_params_21__3 (void) {
    mw_prim_drop();
    push_ptr("expected parameter name\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_def_params_21__4 (void) {
    mw_prim_drop();
    mw_drop();
}

static void mb_elab_def_params_21__5 (void) {
    mw_prim_drop();
    push_ptr("expected right paren or comma\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_def_params_21__6 (void) {
    mw_prim_drop();
    mw_dup();
}

static void mb_elab_def_params_21__7 (void) {
    mw_prim_drop();
    mw_drop2();
}

static void mb_elab_def_params_21__8 (void) {
    mw_prim_drop();
    mw_cons();
}

static void mb_elab_def_body_21__1 (void) {
    mw_prim_drop();
    mw_dup();
    mw_ab_token();
    mw__40_();
    mw_elab_match_at_21_();
}

static void mb_elab_def_body_21__2 (void) {
    mw_prim_drop();
    mw_elab_atoms_21_();
}

static void mb_elab_def_external_21__1 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_def_external_21__2 (void) {
    mw_prim_drop();
    mw_token_name_3F_();
    mw_name_undefined_3F_();
    if (pop_u64()) {
        mw_nip();
        mw_External_2E_alloc_21_();
        mw_dup2();
        mw_DEF_EXTERNAL();
        mw_swap();
        mw_name_def();
        mw__21_();
        mw_tuck();
        mw_external_name();
        mw__21_();
        mw_tuck();
        mw_external_sig();
        mw__21_();
        mw_dup();
        push_u64(0);
        push_fnptr(&mb_elab_def_external_21__4);
        mw_prim_pack_cons();
        mw_delay();
        mw_swap();
        mw_external_ctx_type();
        mw__21_();
    } else {
        mw_drop();
        push_ptr("word already defined\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mb_elab_def_external_21__7 (void) {
    mw_prim_drop();
    push_ptr("expected word name\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_def_external_21__3 (void) {
    mw_prim_drop();
    mw_nip();
    mw_External_2E_alloc_21_();
    mw_dup2();
    mw_DEF_EXTERNAL();
    mw_swap();
    mw_name_def();
    mw__21_();
    mw_tuck();
    mw_external_name();
    mw__21_();
    mw_tuck();
    mw_external_sig();
    mw__21_();
    mw_dup();
    push_u64(0);
    push_fnptr(&mb_elab_def_external_21__4);
    mw_prim_pack_cons();
    mw_delay();
    mw_swap();
    mw_external_ctx_type();
    mw__21_();
}

static void mb_elab_def_external_21__6 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("word already defined\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_def_external_21__4 (void) {
    mw_prim_drop();
    mw_type_elab_default();
    mw_swap();
    mw_external_sig();
    mw__40_();
    mw_elab_type_sig_21_();
    mw_drop();
    {
        VAL d2 = pop_value();
        mw_type_elab_ctx();
        push_value(d2);
    }
    mw_pack2();
}

static void mb_elab_def_external_21__5 (void) {
    mw_prim_drop();
    mw_type_elab_ctx();
}

static void mb_elab_def_type_21__1 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_def_type_21__2 (void) {
    mw_prim_drop();
    mw_token_name_3F_();
    mw_name_undefined_3F_();
    if (pop_u64()) {
        mw_nip();
        mw_swap();
        mw_elab_simple_type_arg_21_();
        mw_DEF_TYPE();
        mw_swap();
        mw_name_def();
        mw__21_();
    } else {
        mw_drop();
        push_ptr("type already defined\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mb_elab_def_type_21__5 (void) {
    mw_prim_drop();
    push_ptr("expected type constructor\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_def_type_21__3 (void) {
    mw_prim_drop();
    mw_nip();
    mw_swap();
    mw_elab_simple_type_arg_21_();
    mw_DEF_TYPE();
    mw_swap();
    mw_name_def();
    mw__21_();
}

static void mb_elab_def_type_21__4 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("type already defined\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_buffer_21__1 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_buffer_21__2 (void) {
    mw_prim_drop();
    mw_token_name_3F_();
    mw_name_undefined_3F_();
    if (pop_u64()) {
        mw_nip();
        mw_swap();
        mw_token_int_40_();
        mw_buffer_new_21_();
        mw_drop();
    } else {
        mw_drop();
        push_ptr("buffer already defined\0\0\0\0");
        mw_emit_fatal_error_21_();
    }
}

static void mb_elab_buffer_21__5 (void) {
    mw_prim_drop();
    push_ptr("expected buffer name\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_buffer_21__3 (void) {
    mw_prim_drop();
    mw_nip();
    mw_swap();
    mw_token_int_40_();
    mw_buffer_new_21_();
    mw_drop();
}

static void mb_elab_buffer_21__4 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("buffer already defined\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_variable_21__1 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_variable_21__2 (void) {
    mw_prim_drop();
    push_ptr("expected variable name\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_variable_21__3 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_variable_21__4 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("name already defined\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_variable_21__5 (void) {
    mw_prim_drop();
    mw_elab_simple_type_arg_21_();
}

static void mb_elab_table_21__1 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_table_21__2 (void) {
    mw_prim_drop();
    mw_token_name_40_();
    mw_table_new_21_();
    mw_drop();
}

static void mb_elab_table_21__3 (void) {
    mw_prim_drop();
    push_ptr("expected table name\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_table_new_21__1 (void) {
    mw_prim_drop();
    mw_TABLE_MAX_SIZE();
    mw_ab_int_21_();
}

static void mb_table_new_21__2 (void) {
    mw_prim_drop();
    mw_PRIM_UNSAFE_CAST();
    mw_ab_prim_21_();
}

static void mb_table_new_21__3 (void) {
    mw_prim_drop();
    mw_PRIM_UNSAFE_CAST();
    mw_ab_prim_21_();
    push_i64(1LL);
    mw_ab_int_21_();
    mw_PRIM_INT_ADD();
    mw_ab_prim_21_();
    mw_dup();
    mw_table_num_buffer();
    mw__40_();
    mw_ab_buffer_21_();
    mw_PRIM_INT_GET();
    mw_ab_prim_21_();
    push_i64(1LL);
    mw_ab_int_21_();
    mw_PRIM_INT_ADD();
    mw_ab_prim_21_();
    mw_PRIM_INT_MOD();
    mw_ab_prim_21_();
    mw_PRIM_UNSAFE_CAST();
    mw_ab_prim_21_();
}

static void mb_table_new_21__4 (void) {
    mw_prim_drop();
    mw_PRIM_UNSAFE_CAST();
    mw_ab_prim_21_();
    mw_PRIM_CORE_DUP();
    mw_ab_prim_21_();
    push_i64(0LL);
    mw_ab_int_21_();
    mw_PRIM_VALUE_EQ();
    mw_ab_prim_21_();
    push_u64(0);
    push_fnptr(&mb_table_new_21__5);
    mw_prim_pack_cons();
    push_u64(0);
    push_fnptr(&mb_table_new_21__6);
    mw_prim_pack_cons();
    mw_ab_if_21_();
    mw_PRIM_UNSAFE_CAST();
    mw_ab_prim_21_();
}

static void mb_table_new_21__5 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_table_new_21__6 (void) {
    mw_prim_drop();
    push_i64(1LL);
    mw_ab_int_21_();
    mw_PRIM_INT_SUB();
    mw_ab_prim_21_();
}

static void mb_table_new_21__9 (void) {
    mw_prim_pack_uncons();
    VAL var_t = pop_value();
    mw_prim_pack_uncons();
    VAL var_w = pop_value();
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    push_value(var_x);
    incref(var_x);
    mw_Var__3E_Param();
    mw_L1();
    push_u64(0);
    push_value(var_x);
    incref(var_x);
    mw_prim_pack_cons();
    push_value(var_w);
    incref(var_w);
    mw_prim_pack_cons();
    push_value(var_t);
    incref(var_t);
    mw_prim_pack_cons();
    push_fnptr(&mb_table_new_21__10);
    mw_prim_pack_cons();
    mw_ab_lambda_21_();
    decref(var_t);
    decref(var_w);
    decref(var_x);
}

static void mb_table_new_21__10 (void) {
    mw_prim_pack_uncons();
    VAL var_t = pop_value();
    mw_prim_pack_uncons();
    VAL var_w = pop_value();
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    push_i64(1LL);
    mw_ab_int_21_();
    push_u64(0);
    push_value(var_x);
    incref(var_x);
    mw_prim_pack_cons();
    push_value(var_w);
    incref(var_w);
    mw_prim_pack_cons();
    push_value(var_t);
    incref(var_t);
    mw_prim_pack_cons();
    push_fnptr(&mb_table_new_21__11);
    mw_prim_pack_cons();
    push_u64(0);
    push_value(var_x);
    incref(var_x);
    mw_prim_pack_cons();
    push_value(var_w);
    incref(var_w);
    mw_prim_pack_cons();
    push_value(var_t);
    incref(var_t);
    mw_prim_pack_cons();
    push_fnptr(&mb_table_new_21__12);
    mw_prim_pack_cons();
    mw_ab_while_21_();
    mw_PRIM_CORE_DROP();
    mw_ab_prim_21_();
    decref(var_t);
    decref(var_w);
    decref(var_x);
}

static void mb_table_new_21__11 (void) {
    mw_prim_pack_uncons();
    VAL var_t = pop_value();
    mw_prim_pack_uncons();
    VAL var_w = pop_value();
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_PRIM_CORE_DUP();
    mw_ab_prim_21_();
    push_value(var_t);
    incref(var_t);
    mw_table_num_buffer();
    mw__40_();
    mw_ab_buffer_21_();
    mw_PRIM_INT_GET();
    mw_ab_prim_21_();
    mw_PRIM_VALUE_LE();
    mw_ab_prim_21_();
    decref(var_t);
    decref(var_w);
    decref(var_x);
}

static void mb_table_new_21__12 (void) {
    mw_prim_pack_uncons();
    VAL var_t = pop_value();
    mw_prim_pack_uncons();
    VAL var_w = pop_value();
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_PRIM_CORE_DUP();
    mw_ab_prim_21_();
    push_u64(0);
    push_value(var_x);
    incref(var_x);
    mw_prim_pack_cons();
    push_value(var_w);
    incref(var_w);
    mw_prim_pack_cons();
    push_value(var_t);
    incref(var_t);
    mw_prim_pack_cons();
    push_fnptr(&mb_table_new_21__13);
    mw_prim_pack_cons();
    mw_ab_dip_21_();
    push_i64(1LL);
    mw_ab_int_21_();
    mw_PRIM_INT_ADD();
    mw_ab_prim_21_();
    decref(var_t);
    decref(var_w);
    decref(var_x);
}

static void mb_table_new_21__13 (void) {
    mw_prim_pack_uncons();
    VAL var_t = pop_value();
    mw_prim_pack_uncons();
    VAL var_w = pop_value();
    mw_prim_pack_uncons();
    VAL var_x = pop_value();
    mw_prim_drop();
    mw_PRIM_UNSAFE_CAST();
    mw_ab_prim_21_();
    push_value(var_x);
    incref(var_x);
    mw_ab_var_21_();
    decref(var_t);
    decref(var_w);
    decref(var_x);
}

static void mb_table_new_21__14 (void) {
    mw_prim_drop();
    mw_dup();
    mw_table_num_buffer();
    mw__40_();
    mw_ab_buffer_21_();
    mw_PRIM_INT_GET();
    mw_ab_prim_21_();
    push_i64(1LL);
    mw_ab_int_21_();
    mw_PRIM_INT_ADD();
    mw_ab_prim_21_();
    mw_PRIM_CORE_DUP();
    mw_ab_prim_21_();
    mw_dup();
    mw_table_num_buffer();
    mw__40_();
    mw_ab_buffer_21_();
    mw_PRIM_INT_SET();
    mw_ab_prim_21_();
    mw_PRIM_UNSAFE_CAST();
    mw_ab_prim_21_();
}

static void mb_elab_target_c99_21__1 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_target_c99_21__2 (void) {
    mw_prim_drop();
    mw_token_str_40_();
    mw_Str__3E_Path();
}

static void mb_elab_target_c99_21__3 (void) {
    mw_prim_drop();
    mw_ctx_empty();
    mw_T0();
    mw_T0();
    mw_T__3E_();
}

static void mb_elab_embed_str_21__1 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_embed_str_21__2 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_embed_str_21__3 (void) {
    mw_prim_drop();
    push_ptr("expected name\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_embed_str_21__4 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_embed_str_21__5 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("name already defined\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_embed_str_21__6 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_embed_str_21__7 (void) {
    mw_prim_drop();
    push_ptr("expected path\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_embed_str_21__8 (void) {
    mw_prim_drop();
    mw_read_file_21_();
    mw_nip();
}

static void mb_elab_embed_str_21__9 (void) {
    mw_prim_drop();
    push_ptr("could not open file\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_elab_field_21__1 (void) {
    mw_prim_drop();
    mw_token_next();
}

static void mb_elab_field_21__3 (void) {
    mw_prim_drop();
    mw_id();
}

static void mb_elab_field_21__4 (void) {
    mw_prim_drop();
    mw_drop();
    push_ptr("name already defined\0\0\0\0");
    mw_emit_fatal_error_21_();
}

static void mb_field_new_21__1 (void) {
    mw_prim_drop();
    mw_elab_simple_type_arg_21_();
}

static void mb_field_new_21__2 (void) {
    mw_prim_drop();
    mw_elab_simple_type_arg_21_();
}

static void mb_ctx_make_fresh_var_21__1 (void) {
    mw_prim_drop();
    mw_ctx_fresh_name_21_();
    mw_var_new_21_();
}

static void mb_ctx_make_fresh_var_21__2 (void) {
    mw_prim_drop();
    mw_ctx_new_21_();
}

static void mb_name_prim_3D__1 (void) {
    mw_prim_drop();
    mw_name_def();
    mw__40_();
}

static void mb_def_prim_21__1 (void) {
    mw_prim_drop();
    mw_DEF_PRIM();
}


static VAL* fieldptr_name_str (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_name_str (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_name_str(index);
    push_ptr(v);
}

static VAL* fieldptr_name_def (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_name_def (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_name_def(index);
    push_ptr(v);
}

static VAL* fieldptr_name_mangled (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_name_mangled (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_name_mangled(index);
    push_ptr(v);
}

static VAL* fieldptr_module_name (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_module_name (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_module_name(index);
    push_ptr(v);
}

static VAL* fieldptr_module_path (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_module_path (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_module_path(index);
    push_ptr(v);
}

static VAL* fieldptr_module_start (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_module_start (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_module_start(index);
    push_ptr(v);
}

static VAL* fieldptr_module_end (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_module_end (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_module_end(index);
    push_ptr(v);
}

static VAL* fieldptr_module_imports (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_module_imports (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_module_imports(index);
    push_ptr(v);
}

static VAL* fieldptr_token_value (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_token_value (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_token_value(index);
    push_ptr(v);
}

static VAL* fieldptr_token_module (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_token_module (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_token_module(index);
    push_ptr(v);
}

static VAL* fieldptr_token_row (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_token_row (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_token_row(index);
    push_ptr(v);
}

static VAL* fieldptr_token_col (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_token_col (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_token_col(index);
    push_ptr(v);
}

static VAL* fieldptr_buffer_size (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_buffer_size (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_buffer_size(index);
    push_ptr(v);
}

static VAL* fieldptr_buffer_name (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_buffer_name (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_buffer_name(index);
    push_ptr(v);
}

static VAL* fieldptr_meta_type (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_meta_type (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_meta_type(index);
    push_ptr(v);
}

static VAL* fieldptr_data_header (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_data_header (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_data_header(index);
    push_ptr(v);
}

static VAL* fieldptr_data_name (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_data_name (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_data_name(index);
    push_ptr(v);
}

static VAL* fieldptr_data_arity (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_data_arity (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_data_arity(index);
    push_ptr(v);
}

static VAL* fieldptr_data_tags (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_data_tags (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_data_tags(index);
    push_ptr(v);
}

static VAL* fieldptr_tag_data (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_tag_data (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_tag_data(index);
    push_ptr(v);
}

static VAL* fieldptr_tag_name (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_tag_name (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_tag_name(index);
    push_ptr(v);
}

static VAL* fieldptr_tag_value (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_tag_value (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_tag_value(index);
    push_ptr(v);
}

static VAL* fieldptr_tag_sig (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_tag_sig (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_tag_sig(index);
    push_ptr(v);
}

static VAL* fieldptr_tag_ctx_type (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_tag_ctx_type (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_tag_ctx_type(index);
    push_ptr(v);
}

static VAL* fieldptr_arrow_token_start (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_arrow_token_start (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_arrow_token_start(index);
    push_ptr(v);
}

static VAL* fieldptr_arrow_token_end (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_arrow_token_end (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_arrow_token_end(index);
    push_ptr(v);
}

static VAL* fieldptr_arrow_home (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_arrow_home (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_arrow_home(index);
    push_ptr(v);
}

static VAL* fieldptr_arrow_homeidx (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_arrow_homeidx (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_arrow_homeidx(index);
    push_ptr(v);
}

static VAL* fieldptr_arrow_ctx (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_arrow_ctx (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_arrow_ctx(index);
    push_ptr(v);
}

static VAL* fieldptr_arrow_dom (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_arrow_dom (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_arrow_dom(index);
    push_ptr(v);
}

static VAL* fieldptr_arrow_cod (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_arrow_cod (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_arrow_cod(index);
    push_ptr(v);
}

static VAL* fieldptr_arrow_atoms (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_arrow_atoms (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_arrow_atoms(index);
    push_ptr(v);
}

static VAL* fieldptr_atom_token (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_atom_token (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_atom_token(index);
    push_ptr(v);
}

static VAL* fieldptr_atom_ctx (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_atom_ctx (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_atom_ctx(index);
    push_ptr(v);
}

static VAL* fieldptr_atom_op (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_atom_op (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_atom_op(index);
    push_ptr(v);
}

static VAL* fieldptr_atom_args (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_atom_args (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_atom_args(index);
    push_ptr(v);
}

static VAL* fieldptr_atom_dom (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_atom_dom (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_atom_dom(index);
    push_ptr(v);
}

static VAL* fieldptr_atom_cod (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_atom_cod (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_atom_cod(index);
    push_ptr(v);
}

static VAL* fieldptr_atom_subst (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_atom_subst (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_atom_subst(index);
    push_ptr(v);
}

static VAL* fieldptr_lambda_token (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_lambda_token (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_lambda_token(index);
    push_ptr(v);
}

static VAL* fieldptr_lambda_outer_ctx (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_lambda_outer_ctx (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_lambda_outer_ctx(index);
    push_ptr(v);
}

static VAL* fieldptr_lambda_inner_ctx (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_lambda_inner_ctx (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_lambda_inner_ctx(index);
    push_ptr(v);
}

static VAL* fieldptr_lambda_dom (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_lambda_dom (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_lambda_dom(index);
    push_ptr(v);
}

static VAL* fieldptr_lambda_mid (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_lambda_mid (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_lambda_mid(index);
    push_ptr(v);
}

static VAL* fieldptr_lambda_cod (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_lambda_cod (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_lambda_cod(index);
    push_ptr(v);
}

static VAL* fieldptr_lambda_params (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_lambda_params (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_lambda_params(index);
    push_ptr(v);
}

static VAL* fieldptr_lambda_body (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_lambda_body (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_lambda_body(index);
    push_ptr(v);
}

static VAL* fieldptr_block_ctx (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_block_ctx (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_block_ctx(index);
    push_ptr(v);
}

static VAL* fieldptr_block_token (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_block_token (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_block_token(index);
    push_ptr(v);
}

static VAL* fieldptr_block_dom (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_block_dom (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_block_dom(index);
    push_ptr(v);
}

static VAL* fieldptr_block_cod (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_block_cod (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_block_cod(index);
    push_ptr(v);
}

static VAL* fieldptr_block_arrow (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_block_arrow (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_block_arrow(index);
    push_ptr(v);
}

static VAL* fieldptr_match_ctx (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_match_ctx (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_match_ctx(index);
    push_ptr(v);
}

static VAL* fieldptr_match_dom (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_match_dom (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_match_dom(index);
    push_ptr(v);
}

static VAL* fieldptr_match_cod (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_match_cod (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_match_cod(index);
    push_ptr(v);
}

static VAL* fieldptr_match_token (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_match_token (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_match_token(index);
    push_ptr(v);
}

static VAL* fieldptr_match_body (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_match_body (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_match_body(index);
    push_ptr(v);
}

static VAL* fieldptr_match_cases (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_match_cases (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_match_cases(index);
    push_ptr(v);
}

static VAL* fieldptr_case_match (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_case_match (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_case_match(index);
    push_ptr(v);
}

static VAL* fieldptr_case_token (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_case_token (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_case_token(index);
    push_ptr(v);
}

static VAL* fieldptr_case_pattern (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_case_pattern (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_case_pattern(index);
    push_ptr(v);
}

static VAL* fieldptr_case_subst (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_case_subst (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_case_subst(index);
    push_ptr(v);
}

static VAL* fieldptr_case_mid (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_case_mid (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_case_mid(index);
    push_ptr(v);
}

static VAL* fieldptr_case_body (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_case_body (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_case_body(index);
    push_ptr(v);
}

static VAL* fieldptr_var_is_implicit (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_var_is_implicit (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_var_is_implicit(index);
    push_ptr(v);
}

static VAL* fieldptr_var_name (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_var_name (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_var_name(index);
    push_ptr(v);
}

static VAL* fieldptr_var_type (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_var_type (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_var_type(index);
    push_ptr(v);
}

static VAL* fieldptr_var_auto_run (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_var_auto_run (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_var_auto_run(index);
    push_ptr(v);
}

static VAL* fieldptr_word_name (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_word_name (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_word_name(index);
    push_ptr(v);
}

static VAL* fieldptr_word_head (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_word_head (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_word_head(index);
    push_ptr(v);
}

static VAL* fieldptr_word_sig (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_word_sig (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_word_sig(index);
    push_ptr(v);
}

static VAL* fieldptr_word_body (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_word_body (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_word_body(index);
    push_ptr(v);
}

static VAL* fieldptr_word_ctx_type (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_word_ctx_type (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_word_ctx_type(index);
    push_ptr(v);
}

static VAL* fieldptr_word_params (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_word_params (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_word_params(index);
    push_ptr(v);
}

static VAL* fieldptr_word_arrow (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_word_arrow (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_word_arrow(index);
    push_ptr(v);
}

static VAL* fieldptr_table_name (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_table_name (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_table_name(index);
    push_ptr(v);
}

static VAL* fieldptr_table_num_buffer (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_table_num_buffer (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_table_num_buffer(index);
    push_ptr(v);
}

static VAL* fieldptr_table_max_count (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_table_max_count (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_table_max_count(index);
    push_ptr(v);
}

static VAL* fieldptr_field_name (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_field_name (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_field_name(index);
    push_ptr(v);
}

static VAL* fieldptr_field_index_type (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_field_index_type (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_field_index_type(index);
    push_ptr(v);
}

static VAL* fieldptr_field_value_type (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_field_value_type (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_field_value_type(index);
    push_ptr(v);
}

static VAL* fieldptr_external_name (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_external_name (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_external_name(index);
    push_ptr(v);
}

static VAL* fieldptr_external_sig (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_external_sig (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_external_sig(index);
    push_ptr(v);
}

static VAL* fieldptr_external_ctx_type (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_external_ctx_type (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_external_ctx_type(index);
    push_ptr(v);
}

static VAL* fieldptr_variable_name (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_variable_name (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_variable_name(index);
    push_ptr(v);
}

static VAL* fieldptr_variable_type (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_variable_type (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_variable_type(index);
    push_ptr(v);
}

static VAL* fieldptr_prim_name (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_prim_name (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_prim_name(index);
    push_ptr(v);
}

static VAL* fieldptr_prim_ctx (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_prim_ctx (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_prim_ctx(index);
    push_ptr(v);
}

static VAL* fieldptr_prim_type (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_prim_type (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_prim_type(index);
    push_ptr(v);
}

static VAL* fieldptr_prim_decl (size_t i) {
    static struct VAL * p = 0;
    size_t m = 65536;
    if (!p) { p = calloc(m, sizeof *p); }
    if (i>=m) { write(2,"table too big\n",14); exit(123); }
    return p+i;
}

static void mw_prim_decl (void){
    size_t index = (size_t)pop_u64();
    VAL *v = fieldptr_prim_decl(index);
    push_ptr(v);
}


