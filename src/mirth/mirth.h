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

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef uintptr_t usize;

extern void* mmap(void*, int, int, int, int, int);
extern void* malloc(usize);
extern void* calloc(usize, usize);
extern void* realloc(void*, usize);
extern void* memset(void*, int, usize);
extern void* memcpy(void*, const void*, usize);
extern void free(void*);
extern usize strlen(const char*);
extern int read(int, void*, usize);
extern int write(int, void*, usize);
extern int close(int);
extern int open(void*, int, int);
extern int strcmp(const char*, const char*);
extern void exit(int);

typedef enum value_tag_t {
    VT_U64 = 0x00,
    VT_U32 = 0x01,
    VT_U21 = 0x02,
    VT_U16 = 0x03,
    VT_C16 = 0x90,
    VT_C21 = 0x96,
    VT_C32 = 0xA0,
    VT_C64 = 0xC0,
} value_tag_t;

typedef void (*fnptr)(void);

typedef union value_payload_t {
    void* vp_ptr;
    u8 vp_u8;
    u16 vp_u16;
    u32 vp_u32;
    u64 vp_u64;
    i8 vp_i8;
    i16 vp_i16;
    i32 vp_i32;
    i64 vp_i64;
    bool vp_bool;
    fnptr vp_fnptr;
    struct value_t* vp_valueptr;
} value_payload_t;

typedef struct value_t {
    value_payload_t payload;
    value_tag_t tag;
} value_t;

typedef struct cell_t {
    u32 refs;
    bool freecdr;
    value_t car;
    value_t cdr;
} cell_t;

#define STACK_SIZE 0x1000
static usize stack_counter = STACK_SIZE;
static value_t stack [STACK_SIZE] = {0};

#define HEAP_SIZE 0x80000
#define HEAP_MASK 0x7FFFF
static usize heap_next = 1;
static usize heap_count = 0;
static cell_t heap [HEAP_SIZE] = {0};

static int global_argc;
static char** global_argv;

#define get_cell_index(v) ((usize)(((v).tag & 0x80) ? ((v).payload.vp_u64 >> (0xC0 - (u64)((v).tag))) : 0))

#define incref(v) do{ value_t w = (v); usize i = get_cell_index(w); if(i) heap[i].refs++; }while(0)

#define decref(v) do{ value_t w = (v); usize i = get_cell_index(w); if(i) { if(heap[i].refs) { heap[i].refs--; if (heap[i].refs == 0) heap_free(i); } }} while(0)

static void heap_free(usize i) {
    cell_t *cell = heap + i;
    cell_t contents = *cell;
    memset(cell, 0, sizeof(cell_t));
    cell->cdr.payload.vp_u64 = heap_next;
    heap_next = i;
    heap_count--;
    if (contents.freecdr) { free(contents.cdr.payload.vp_ptr); }
    else { decref(contents.cdr); }
    decref(contents.car);
}

#define decref_for_uncons(v) do{ value_t w = (v); usize i = get_cell_index(w); if(i) { if (heap[i].refs) { heap[i].refs--; if (heap[i].refs == 0) { memset(heap+i, 0, sizeof(cell_t)); heap[i].cdr.payload.vp_u64 = heap_next; heap_next = i; heap_count--; } else { cell_t cell = heap[i]; incref(cell.car); incref(cell.cdr); } } } } while(0)
static void value_uncons(value_t val, value_t* car, value_t* cdr) {
    switch (val.tag) {
        case VT_U64: {
            value_t nil = { 0 };
            *car = nil;
            *cdr = val;
        } break;
        case VT_U32: {
            u64 vv = val.payload.vp_u64;
            u64 lo = vv & 0xFFFFFFFF;
            u64 hi = vv >> 32;
            car->tag = VT_U64; car->payload.vp_u64 = hi;
            cdr->tag = VT_U64; cdr->payload.vp_u64 = lo;
        } break;
        case VT_C64: {
            cell_t* cell = heap + val.payload.vp_u64;
            *car = cell->car;
            *cdr = cell->cdr;
        } break;
        case VT_C32: {
            u64 vv = val.payload.vp_u64;
            u64 lo = vv & 0xFFFFFFFF;
            u64 hi = vv >> 32;
            car->tag = VT_C64; car->payload.vp_u64 = hi;
            cdr->tag = VT_U64; cdr->payload.vp_u64 = lo;
        } break;
        case VT_U21: {
            u64 vv = val.payload.vp_u64;
            u64 lo = vv & 0x1FFFFF;
            u64 md = (vv >> 21) & 0x1FFFFF;
            u64 hi = (vv >> 42) & 0x1FFFFF;
            car->tag = VT_U32;
            car->payload.vp_u64 = (hi << 32) | md;
            cdr->tag = VT_U64; cdr->payload.vp_u64 = lo;
        } break;
        case VT_C21: {
            u64 vv = val.payload.vp_u64;
            u64 lo = vv & 0x1FFFFF;
            u64 md = (vv >> 21) & 0x1FFFFF;
            u64 hi = (vv >> 42) & 0x1FFFFF;
            car->tag = VT_C32;
            car->payload.vp_u64 = (hi << 32) | md;
            cdr->tag = VT_U64; cdr->payload.vp_u64 = lo;
        } break;
        case VT_U16: {
            u64 vv = val.payload.vp_u64;
            u64 lo = vv & 0xFFFF;
            u64 y2 = (vv >> 16) & 0xFFFF;
            u64 y1 = (vv >> 32) & 0xFFFF;
            u64 y0 = (vv >> 48) & 0xFFFF;
            car->tag = VT_U21;
            car->payload.vp_u64 = (y0 << 42) | (y1 << 21) | y2;
            cdr->tag = VT_U64; cdr->payload.vp_u64 = lo;
        } break;
        case VT_C16: {
            u64 vv = val.payload.vp_u64;
            u64 lo = vv & 0xFFFF;
            u64 y2 = (vv >> 16) & 0xFFFF;
            u64 y1 = (vv >> 32) & 0xFFFF;
            u64 y0 = (vv >> 48) & 0xFFFF;
            car->tag = VT_C21;
            car->payload.vp_u64 = (y0 << 42) | (y1 << 21) | y2;
            cdr->tag = VT_U64; cdr->payload.vp_u64 = lo;
        } break;
    }
}

static bool value_has_ptr_offset (value_t v) {
    if (v.tag == VT_C64) {
        usize cell_index = (usize)v.payload.vp_u64;
        struct cell_t * cell = heap + cell_index;
        return !cell->freecdr;
    } else {
        return v.tag != VT_U64;
    }
}

static u64 value_ptr_size (value_t v) {
    if (v.payload.vp_u64 == 0) {
        return 0;
    } else if (v.tag == VT_U64) {
        return strlen(v.payload.vp_ptr);
    } else if (value_has_ptr_offset(v)) {
        value_t car, cdr;
        value_uncons(v, &car, &cdr);
        value_t car2, cdr2;
        value_uncons(car, &car2, &cdr2);
        u64 size = car2.payload.vp_u64;
        u64 offset = cdr.payload.vp_u64;
        if (size >= offset) {
            return offset - size;
        } else {
            return 0;
        }
    } else {
        value_t car, cdr;
        value_uncons(v, &car, &cdr);
        return car.payload.vp_u64;
    }
}

static void* value_ptr_base (value_t v) {
    if (value_has_ptr_offset(v)) {
        value_t car, cdr;
        value_uncons(v, &car, &cdr);
        value_t car2, cdr2;
        value_uncons(car, &car2, &cdr2);
        return cdr2.payload.vp_ptr;
    } else {
        value_t car, cdr;
        value_uncons(v, &car, &cdr);
        return cdr.payload.vp_ptr;
    }
}

static i64 value_ptr_offset (value_t v) {
    if (value_has_ptr_offset(v)) {
        value_t car, cdr;
        value_uncons(v, &car, &cdr);
        return cdr.payload.vp_i64;
    } else {
        return 0;
    }
}

static void* value_ptr (value_t v) {
    usize cell_index; cell_t* cell; usize offset;
    switch (v.tag) {
        case VT_U64: return v.payload.vp_ptr;
        case VT_C64:
            cell_index = (usize)v.payload.vp_u64;
            cell = heap + cell_index;
            if (cell->freecdr) {
                return cell->cdr.payload.vp_ptr;
            } else {
                offset = (usize)cell->cdr.payload.vp_u64;
                cell_index = (usize)cell->car.payload.vp_u64;
                break;
            }
        case VT_C32:
            offset = (usize)v.payload.vp_u32;
            cell_index = (usize)(v.payload.vp_u64 >> 32);
            break;
        default:
            return (void*)0;
    }
    cell = heap + cell_index;
    if (cell->freecdr) {
        char* base = cell->cdr.payload.vp_ptr;
        return (void*)(base + offset);
    }
    return (void*)0;
}

#define pop_fnptr() (pop_value().payload.vp_fnptr)
#define pop_u8() (pop_value().payload.vp_u8)
#define pop_u16() (pop_value().payload.vp_u16)
#define pop_u32() (pop_value().payload.vp_u32)
#define pop_u64() (pop_value().payload.vp_u64)
#define pop_i8() (pop_value().payload.vp_i8)
#define pop_i16() (pop_value().payload.vp_i16)
#define pop_i32() (pop_value().payload.vp_i32)
#define pop_i64() (pop_value().payload.vp_i64)
#define pop_bool() (pop_value().payload.vp_bool)
#define pop_rawptr() (pop_value().payload.vp_ptr)

#define push_u64(v) push_value(mku64(v))
#define push_i64(v) push_value(mki64(v))
#define push_ptr(v) push_u64((u64)(v))
#define push_fnptr(v) push_u64((u64)(v))
#define push_bool(b) push_u64((u64)((bool)(b)))
#define push_u8(b) push_u64((u64)(b))
#define push_u16(b) push_u64((u64)(b))
#define push_u32(b) push_u64((u64)(b))
#define push_i8(b) push_i64((i64)(b))
#define push_i16(b) push_i64((i64)(b))
#define push_i32(b) push_i64((i64)(b))
#define push_rawptr(v) push_u64((u64)(void*)(v))

static void push_value(value_t x) {
    stack[--stack_counter] = x;
}

static value_t pop_value(void) {
    return stack[stack_counter++];
}

static value_t mku64 (u64 x) {
    value_t v = {.tag = VT_U64, .payload = {.vp_u64 = x}};
    return v;
}

static value_t mki64 (i64 x) {
    value_t v = {.tag = VT_U64, .payload = {.vp_i64 = x}};
    return v;
}

static value_t mkcell (value_t car, value_t cdr) {
    if ((car.payload.vp_u64 == 0) && (cdr.tag == VT_U64))
        return cdr;
    if (cdr.tag == VT_U64) {
        switch (car.tag) {
            case VT_U64: {
                u64 x0 = car.payload.vp_u64;
                u64 x1 = cdr.payload.vp_u64;
                u64 y0 = x0 & 0xFFFFFFFFLL;
                u64 y1 = x1 & 0xFFFFFFFFLL;
                if ((x0 == y0) && (x1 == y1)) {
                    value_t r;
                    r.tag = VT_U32;
                    r.payload.vp_u64 = (y0 << 32) | y1;
                    return r;
                }
            } break;
            case VT_C64: {
                u64 x0 = car.payload.vp_u64;
                u64 x1 = cdr.payload.vp_u64;
                u64 y0 = x0 & 0xFFFFFFFFLL;
                u64 y1 = x1 & 0xFFFFFFFFLL;
                if ((x0 == y0) && (x1 == y1)) {
                    value_t r;
                    r.tag = VT_C32;
                    r.payload.vp_u64 = (y0 << 32) | y1;
                    return r;
                }
            } break;
            case VT_U32: {
                u64 x0 = car.payload.vp_u64 >> 32;
                u64 x1 = car.payload.vp_u64 & 0xFFFFFFFFLL;
                u64 x2 = cdr.payload.vp_u64;
                u64 y0 = x0 & 0x1FFFFFLL;
                u64 y1 = x1 & 0x1FFFFFLL;
                u64 y2 = x2 & 0x1FFFFFLL;
                if ((x0 == y0) && (x1 == y1) && (x2 == y2)) {
                    value_t r;
                    r.tag = VT_U21;
                    r.payload.vp_u64 = (y0 << 42) | (y1 << 21) | y2;
                    return r;
                }
            } break;
            case VT_C32: {
                u64 x0 = car.payload.vp_u64 >> 32;
                u64 x1 = car.payload.vp_u64 & 0xFFFFFFFFLL;
                u64 x2 = cdr.payload.vp_u64;
                u64 y0 = x0 & 0x1FFFFFLL;
                u64 y1 = x1 & 0x1FFFFFLL;
                u64 y2 = x2 & 0x1FFFFFLL;
                if ((x0 == y0) && (x1 == y1) && (x2 == y2)) {
                    value_t r;
                    r.tag = VT_C21;
                    r.payload.vp_u64 = (y0 << 42) | (y1 << 21) | y2;
                    return r;
                }
            } break;
            case VT_U21: {
                u64 x0 = car.payload.vp_u64 >> 42;
                u64 x1 = (car.payload.vp_u64 >> 21) & 0x1FFFFFLL;
                u64 x2 = car.payload.vp_u64 & 0x1FFFFFLL;
                u64 x3 = cdr.payload.vp_u64;
                u64 y0 = x0 & 0xFFFFLL;
                u64 y1 = x1 & 0xFFFFLL;
                u64 y2 = x2 & 0xFFFFLL;
                u64 y3 = x3 & 0xFFFFLL;
                if ((x0 == y0) && (x1 == y1) && (x2 == y2) && (x3 == y3)) {
                    value_t r;
                    r.tag = VT_U16;
                    r.payload.vp_u64 = (y0 << 48) | (y1 << 32) | (y2 << 16) | y3;
                    return r;
                }
            } break;
            case VT_C21: {
                u64 x0 = car.payload.vp_u64 >> 42;
                u64 x1 = (car.payload.vp_u64 >> 21) & 0x1FFFFFLL;
                u64 x2 = car.payload.vp_u64 & 0x1FFFFFLL;
                u64 x3 = cdr.payload.vp_u64;
                u64 y0 = x0 & 0xFFFFLL;
                u64 y1 = x1 & 0xFFFFLL;
                u64 y2 = x2 & 0xFFFFLL;
                u64 y3 = x3 & 0xFFFFLL;
                if ((x0 == y0) && (x1 == y1) && (x2 == y2) && (x3 == y3)) {
                    value_t r;
                    r.tag = VT_C16;
                    r.payload.vp_u64 = (y0 << 48) | (y1 << 32) | (y2 << 16) | y3;
                    return r;
                }
            } break;
            default: break;
        }
    }
    if (heap_count >= HEAP_SIZE - 1) {
        write(2, "HEAP OVERFLOW\n", 14);
        exit(1);
    }
    u64 cell_index = heap_next;
    cell_t *cell = heap + cell_index;
    while ((cell->refs > 0) && (cell_index < HEAP_SIZE)) { cell++; cell_index++; }
    if (cell_index >= HEAP_SIZE - 1) {
        write(2, "HEAP OVERFLOW\n", 14);
        exit(1);
    }
    u64 saved_index = cell->cdr.payload.vp_u64;
    heap_next = (usize)(saved_index ? saved_index : cell_index+1);
    heap_count++;
    cell->refs = 1;
    cell->freecdr = false;
    cell->car = car;
    cell->cdr = cdr;
    value_t v = {0};
    v.tag = VT_C64;
    v.payload.vp_u64 = cell_index;
    return v;
}

static value_t mkcell_raw (value_t car, value_t cdr) {
    u64 cell_index = heap_next;
    cell_t *cell = heap + cell_index;
    while ((cell->refs > 0) && (cell_index < HEAP_SIZE)) { cell++; cell_index++; }
    if (cell_index >= HEAP_SIZE - 1) {
        write(2, "HEAP OVERFLOW\n", 14);
        exit(1);
    }
    u64 saved_index = cell->cdr.payload.vp_u64;
    heap_next = (usize)(saved_index ? saved_index : cell_index+1);
    heap_count++;
    cell->refs = 1;
    cell->freecdr = false;
    cell->car = car;
    cell->cdr = cdr;
    value_t v = {0};
    v.tag = VT_C64;
    v.payload.vp_u64 = cell_index;
    return v;
}

static value_t mkcell_freecdr (value_t car, value_t cdr) {
    if (heap_count >= HEAP_SIZE - 1) {
        write(2, "HEAP OVERFLOW\n", 14);
        exit(1);
    }
    u64 cell_index = heap_next;
    cell_t *cell = heap + cell_index;
    while ((cell->refs > 0) && (cell_index < HEAP_SIZE)) { cell++; cell_index++; }
    if (cell_index >= HEAP_SIZE - 1) {
        write(2, "HEAP OVERFLOW\n", 14);
        exit(1);
    }
    u64 saved_index = cell->cdr.payload.vp_u64;
    heap_next = (usize)(saved_index ? saved_index : cell_index+1);
    heap_count++;
    cell->refs = 1;
    cell->freecdr = true;
    cell->car = car;
    cell->cdr = cdr;
    value_t v = {0};
    v.tag = VT_C64;
    v.payload.vp_u64 = cell_index;
    return v;
}

static void do_pack_uncons(void) {
    value_t car, cdr, val;
    val = pop_value();
    value_uncons(val, &car, &cdr);
    push_value(car); push_value(cdr);
    if (val.tag == VT_C64) {
        decref_for_uncons(val);
    }
}

#define get_value_tag(v) (((v).tag == VT_U64) ? (v).payload.vp_i64 : (((v).tag == VT_C64) ? (heap[(v).payload.vp_u64].cdr.payload.vp_i64) : (i64)((v).payload.vp_u64 & 0xFFFF)))
#define get_top_data_tag() (get_value_tag(stack[stack_counter]))
#define value_cmp(v1,v2) ((((v1).tag == VT_U64) && ((v2).tag == VT_U64)) ? ((v1).payload.vp_i64 - (v2).payload.vp_i64) : value_cmp_hard((v1), (v2)))

static i64 value_cmp_hard(value_t v1, value_t v2) {
    while(1) {
        i64 t1 = get_value_tag(v1);
        i64 t2 = get_value_tag(v2);
        if (t1 < t2) return -1;
        if (t1 > t2) return 1;
        if ((v1.tag == VT_U64) && (v2.tag == VT_U64)) return 0;
        value_t v1car, v1cdr, v2car, v2cdr;
        value_uncons(v1, &v1car, &v1cdr);
        value_uncons(v2, &v2car, &v2cdr);
        i64 cdrcmp = value_cmp(v1cdr, v2cdr);
        if (cdrcmp != 0) return cdrcmp;
        v1 = v1car; v2 = v2car;
    }
}

#define value_eq(v1,v2) (((v1).tag == (v2).tag) && (((v1).payload.vp_u64 == (v2).payload.vp_u64) || (((v1).tag & 0x80) && value_eq_hard((v1),(v2)))))

static bool value_eq_hard(value_t v1, value_t v2) {
    usize c1_index, c2_index; cell_t *c1, *c2;
    while (1) {
        if (v1.tag != v2.tag) return false;
        if (v1.payload.vp_u64 == v2.payload.vp_u64) return true;
        switch (v1.tag) {
            case VT_U64: return false;
            case VT_U32: return false;
            case VT_U21: return false;
            case VT_U16: return false;
            case VT_C64:
                c1_index = (usize)v1.payload.vp_u64;
                c2_index = (usize)v2.payload.vp_u64;
                break;
            case VT_C32:
                if (v1.payload.vp_u32 != v2.payload.vp_u32) return false;
                c1_index = (usize)(v1.payload.vp_u64 >> 32);
                c2_index = (usize)(v2.payload.vp_u64 >> 32);
                break;
            case VT_C21:
                if (  (v1.payload.vp_u64 & 0x03FFFFFFFFFF)
                != (v2.payload.vp_u64 & 0x03FFFFFFFFFF)) return false;
                c1_index = (usize)(v1.payload.vp_u64 >> 42);
                c2_index = (usize)(v2.payload.vp_u64 >> 42);
                break;
            case VT_C16:
                if (  (v1.payload.vp_u64 & 0xFFFFFFFFFFFF)
                != (v2.payload.vp_u64 & 0xFFFFFFFFFFFF)) return false;
                c1_index = (usize)(v1.payload.vp_u64 >> 48);
                c2_index = (usize)(v2.payload.vp_u64 >> 48);
                break;
        }
        c1 = heap + c1_index;
        c2 = heap + c2_index;
        if (!value_eq(c1->cdr, c2->cdr)) return false;
        v1 = c1->car; v2 = c2->car;
    }
}

#define do_run() do { do_pack_uncons(); fnptr fp = pop_fnptr(); fp(); } while(0)
#define mw_prim_id() 0
#define mw_prim_dup() do{ value_t v = stack[stack_counter]; push_value(v); incref(v); } while(0)

#define do_drop() decref(pop_value())
#define mw_prim_drop() do_drop()

#define do_swap() do{ value_t x = stack[stack_counter]; stack[stack_counter] = stack[stack_counter+1]; stack[stack_counter+1] = x; } while(0)
#define mw_prim_swap() do_swap()

static void mw_prim_dip (void) {
    value_t f = pop_value();
    value_t x = pop_value();
    push_value(f);
    do_run();
    push_value(x);
}

static void mw_prim_if (void) {
    value_t else_branch = pop_value();
    value_t then_branch = pop_value();
    bool b = pop_bool();
    if (b) {
        push_value(then_branch);
        decref(else_branch);
    } else {
        push_value(else_branch);
        decref(then_branch);
    }
    do_run();
}

static void mw_prim_while (void) {
    value_t body = pop_value();
    value_t cond = pop_value();
    while(1) {
        push_value(cond); incref(cond); do_run();
        bool b = pop_bool();
        if (!b) break;
        push_value(body); incref(body); do_run();
    }
    decref(cond); decref(body);
}

#define mw_prim_int_add() do { stack[stack_counter+1].payload.vp_u64 += stack[stack_counter].payload.vp_u64; stack_counter++; } while(0)
#define mw_prim_int_sub() do { stack[stack_counter+1].payload.vp_u64 -= stack[stack_counter].payload.vp_u64; stack_counter++; } while(0)
#define mw_prim_int_mul() do { stack[stack_counter+1].payload.vp_i64 *= stack[stack_counter].payload.vp_i64; stack_counter++; } while(0)
#define mw_prim_int_div() do { i64 a = stack[stack_counter+1].payload.vp_i64; i64 b = stack[stack_counter].payload.vp_i64; i64 r = a % b; i64 q = a / b; if (((a < 0) ^ (b < 0)) && r) q--; stack_counter++; stack[stack_counter].payload.vp_i64 = q; } while(0)
#define mw_prim_int_mod() do { i64 a = stack[stack_counter+1].payload.vp_i64; i64 b = stack[stack_counter].payload.vp_i64; i64 r = a % b; if (((a < 0) ^ (b < 0)) && r) r += b; stack_counter++; stack[stack_counter].payload.vp_i64 = r; } while(0)
#define mw_prim_int_and() do { stack[stack_counter+1].payload.vp_u64 &= stack[stack_counter].payload.vp_u64; stack_counter++; } while(0)
#define mw_prim_int_or() do { stack[stack_counter+1].payload.vp_u64 |= stack[stack_counter].payload.vp_u64; stack_counter++; } while(0)
#define mw_prim_int_xor() do { stack[stack_counter+1].payload.vp_u64 ^= stack[stack_counter].payload.vp_u64; stack_counter++; } while(0)
#define mw_prim_int_shl() do { stack[stack_counter+1].payload.vp_u64 <<= stack[stack_counter].payload.vp_u64; stack_counter++; } while(0)
#define mw_prim_int_shr() do { stack[stack_counter+1].payload.vp_u64 >>= stack[stack_counter].payload.vp_u64; stack_counter++; } while(0)
#define mw_prim_value_eq()  do { value_t v2 = pop_value(); value_t v1 = pop_value(); push_bool(value_eq(v1, v2)); decref(v1); decref(v2); } while(0)

#define mw_prim_value_lt()  do { value_t v2 = pop_value(); value_t v1 = pop_value(); push_bool(value_cmp(v1, v2) < 0); decref(v1); decref(v2); } while(0)

#define mw_prim_value_le()  do { value_t v2 = pop_value(); value_t v1 = pop_value(); push_bool(value_cmp(v1, v2) <= 0); decref(v1); decref(v2); } while(0)

static void mw_prim_posix_write (void) {
    usize n = (usize)pop_u64();
    value_t vp = pop_value();
    void* p = value_ptr(vp);
    int f = (int)pop_i64();
    push_i64((i64)write(f, p, n));
    decref(vp);
}

static void mw_prim_posix_read (void) {
    usize n = (usize)pop_u64();
    value_t vp = pop_value();
    void* p = value_ptr(vp);
    int f = (int)pop_i64();
    push_i64((i64)read(f,p,n));
    decref(vp);
}

static void mw_prim_posix_open (void) {
    int m = (int)pop_i64();
    int f = (int)pop_i64();
    value_t vp = pop_value();
    void* p = value_ptr(vp);
    push_i64((i64)open(p,f,m));
    decref(vp);
}

static void mw_prim_posix_close (void) {
    int x = (int)pop_i64();
    push_i64((i64)close(x));
}

static void mw_prim_posix_exit (void) {
    int x = (int)pop_i64();
    exit(x);
}

static void mw_prim_posix_mmap (void) {
    #ifdef MIRTH_WINDOWS
        pop_value(); pop_value(); pop_value(); pop_value();
        usize b = (usize)pop_u64();
        pop_value();
        push_ptr(malloc(b));
    #else
        int f = (int)pop_i64();
        int e = (int)pop_i64();
        int d = (int)pop_i64();
        int c = (int)pop_i64();
        usize b = (usize)pop_u64();
        value_t va = pop_value();
        void* a = value_ptr(va);
        void* p = mmap(a,b,c,d,e,f);
        push_ptr(p);
        decref(va);
    #endif
}

static void do_debug(void) {
    write(2, "??", 2);
    char c[32] = {0};
    char* cp;
    usize n;
    i64 x; i64 y;
    for (long i = STACK_SIZE-1; i >= (long)stack_counter; i--) {
        cp = c+30;
        x = stack[i].payload.vp_i64;
        n = 1;
        y = x; if (x < 0) { x = -x; }
        do { *cp-- = '0' + (x % 10); x /= 10; n++; } while(x);
        if (y < 0) { *cp-- = '-'; n++; }
        *cp = ' ';
        write(2, cp, n);
    }
    write(2, "\n", 1);
}

#define mw_prim_debug() do_debug()

#define mw_prim_value_get() do { value_t vp = pop_value(); value_t* p = value_ptr(vp); push_value(*p); incref(*p); decref(vp); } while(0)
#define mw_prim_int_get() do { value_t vp = pop_value(); i64* p = value_ptr(vp); push_i64(*p); decref(vp); } while(0)
#define mw_prim_ptr_get() do { value_t vp = pop_value(); void** p = value_ptr(vp); push_ptr(*p); decref(vp); } while(0)
#define mw_prim_u8_get() do { value_t vp = pop_value(); u8* p = value_ptr(vp); push_u8(*p); decref(vp); } while(0)
#define mw_prim_u16_get() do { value_t vp = pop_value(); u16* p = value_ptr(vp); push_u16(*p); decref(vp); } while(0)
#define mw_prim_u32_get() do { value_t vp = pop_value(); u32* p = value_ptr(vp); push_u32(*p); decref(vp); } while(0)
#define mw_prim_u64_get() do { value_t vp = pop_value(); u64* p = value_ptr(vp); push_u64(*p); decref(vp); } while(0)
#define mw_prim_i8_get() do { value_t vp = pop_value(); i8* p = value_ptr(vp); push_i8(*p); decref(vp); } while(0)
#define mw_prim_i16_get() do { value_t vp = pop_value(); i16* p = value_ptr(vp); push_i16(*p); decref(vp); } while(0)
#define mw_prim_i32_get() do { value_t vp = pop_value(); i32* p = value_ptr(vp); push_i32(*p); decref(vp); } while(0)
#define mw_prim_i64_get() do { value_t vp = pop_value(); i64* p = value_ptr(vp); push_i64(*p); decref(vp); } while(0)
#define mw_prim_int_set() do { value_t vp = pop_value(); i64* p = value_ptr(vp); *p = pop_i64(); decref(vp); } while(0)
#define mw_prim_u8_set() do { value_t vp = pop_value(); u8* p = value_ptr(vp); *p = pop_u8(); decref(vp); } while(0)
#define mw_prim_u16_set() do { value_t vp = pop_value(); u16* p = value_ptr(vp); *p = pop_u16(); decref(vp); } while(0)
#define mw_prim_u32_set() do { value_t vp = pop_value(); u32* p = value_ptr(vp); *p = pop_u32(); decref(vp); } while(0)
#define mw_prim_u64_set() do { value_t vp = pop_value(); u64* p = value_ptr(vp); *p = pop_u64(); decref(vp); } while(0)
#define mw_prim_i8_set() do { value_t vp = pop_value(); i8* p = value_ptr(vp); *p = pop_i8(); decref(vp); } while(0)
#define mw_prim_i16_set() do { value_t vp = pop_value(); i16* p = value_ptr(vp); *p = pop_i16(); decref(vp); } while(0)
#define mw_prim_i32_set() do { value_t vp = pop_value(); i32* p = value_ptr(vp); *p = pop_i32(); decref(vp); } while(0)
#define mw_prim_i64_set() do { value_t vp = pop_value(); i64* p = value_ptr(vp); *p = pop_i64(); decref(vp); } while(0)
#define mw_prim_ptr_set() do { value_t vp = pop_value(); value_t vx = pop_value(); void** p = value_ptr(vp); *p = value_ptr(vx); decref(vp); decref(vx); } while(0)
#define mw_prim_value_set() do { value_t vp = pop_value(); value_t vx = pop_value(); value_t* p = value_ptr(vp); value_t old = *p; *p = vx; decref(old); decref(vp); } while(0)
#if defined(MIRTH_WINDOWS)
#define mw_prim_sys_os() push_u64(1)
#elif defined(MIRTH_LINUX)
#define mw_prim_sys_os() push_u64(2)
#elif defined(MIRTH_MACOS)
#define mw_prim_sys_os() push_u64(3)
#else
#define mw_prim_sys_os() push_u64(0)
#endif

#define mw_prim_unsafe_cast() 0

#define mw_prim_run() do_run()

static void mw_prim_ptr_add (void) {
    value_t vp = pop_value();
    i64 y = pop_i64();
    if (vp.tag == VT_U64) {
        push_i64(y + vp.payload.vp_i64);
    } else if (value_has_ptr_offset(vp)) {
        value_t car, cdr;
        value_uncons(vp, &car, &cdr);
        cdr.payload.vp_i64 += y;
        push_value(mkcell(car, cdr));
    } else {
        value_t vy = { .tag = VT_U64, .payload = { .vp_i64 = y } };
        push_value(mkcell(vp, vy));
    }
}
#define mw_prim_bool_true() push_bool(true)
#define mw_prim_bool_false() push_bool(false)
#define mw_prim_bool_and() do { stack[stack_counter+1].payload.vp_u64 = stack[stack_counter+1].payload.vp_u64 && stack[stack_counter].payload.vp_u64; stack_counter++; } while(0)
#define mw_prim_bool_or() do { stack[stack_counter+1].payload.vp_u64 = stack[stack_counter+1].payload.vp_u64 || stack[stack_counter].payload.vp_u64; stack_counter++; } while(0)
#define mw_prim_sys_argc() push_i64(global_argc)
#define mw_prim_sys_argv() push_ptr(global_argv)
#define mw_prim_ptr_size() push_u64((u64)sizeof(void*))
static void mw_prim_ptr_alloc (void) {
    i64 psize = pop_i64();
    if (psize > 0) {
        usize size = (usize)psize;
        void* ptr = calloc(1,size);
        value_t vsize = { .tag = VT_U64, .payload = { .vp_i64 = psize } };
        value_t vptr = { .tag = VT_U64, .payload = { .vp_ptr = ptr } };
        value_t v = mkcell_freecdr(vsize, vptr);
        push_value(v);
    } else {
        push_u64(0);
    }
}

static void* alloc_but_copy (usize dstn, void* src, usize srcn) {
    void* dst = calloc(1,dstn);
    if (src) {
        usize cpyn = (dstn > srcn) ? srcn : dstn;
        memcpy(dst, src, cpyn);
    }
    return dst;
}
static void mw_prim_ptr_realloc (void) {
    i64 psize = pop_i64();
    value_t vptr = pop_value();
    if (psize <= 0) {
        decref(vptr);
        push_u64(0);
        return;
    }
    usize new_size = (usize)psize;
    if ((vptr.tag == VT_C64) && !value_has_ptr_offset(vptr)) {
        usize cell_index = get_cell_index(vptr);
        cell_t *cell = heap + cell_index;
        usize old_size = (usize)cell->car.payload.vp_u64;
        void* old_ptr = cell->cdr.payload.vp_ptr;
        void* new_ptr = realloc(old_ptr, new_size);
        cell->car.payload.vp_i64 = psize;
        cell->cdr.payload.vp_ptr = new_ptr;
        if (old_size < new_size) {
            memset((char*)new_ptr + old_size, 0, new_size - old_size);
        }
        push_value(vptr);
    } else {
        void* old_ptr = value_ptr(vptr);
        usize old_size = (usize)value_ptr_size(vptr);
        void* new_ptr = alloc_but_copy(new_size, old_ptr, old_size);
        value_t vsize = { .tag = VT_U64, .payload = { .vp_i64 = psize } };
        value_t vnew = { .tag = VT_U64, .payload = { .vp_ptr = new_ptr } };
        value_t v = mkcell_freecdr(vsize, vnew);
        push_value(v);
        decref(vptr);
    }
}

static void mw_prim_ptr_copy (void) {
    value_t vdst = pop_value();
    i64 ilen = pop_i64();
    value_t vsrc = pop_value();
    void* src = value_ptr(vsrc);
    void* dst = value_ptr(vdst);
    if (src && dst && (ilen > 0)) {
        memcpy(dst, src, (usize)ilen);
    }
    decref(vsrc);
    decref(vdst);
}

static void mw_prim_ptr_fill (void) {
    value_t vdst = pop_value();
    i64 ilen = pop_i64();
    i64 val = pop_i64();
    void* dst = value_ptr(vdst);
    if (dst && (ilen > 0)) {
        memset(dst, (int)val, (usize)ilen);
    }
    decref(vdst);
}

#define mw_prim_ptr_raw() do { usize i = stack_counter; push_ptr(value_ptr(stack[i])); } while(0)
static void mw_prim_str_eq (void) {
    value_t vptr1 = pop_value();
    value_t vptr2 = pop_value();
    const char* ptr1 = value_ptr(vptr1);
    const char* ptr2 = value_ptr(vptr2);
    bool result = (!ptr1 || !ptr2) ? (ptr1 == ptr2) : strcmp(ptr1,ptr2) == 0;
    push_bool(result);
    decref(vptr1); decref(vptr2);
}

static void mw_prim_str_alloc (void) {
    i64 psize = pop_i64();
    push_i64(psize + 4);
    mw_prim_ptr_alloc();
}

#define mw_prim_str_base() 0

#define mw_prim_str_size() do { value_t v = stack[stack_counter]; if (!v.payload.vp_u64) { push_u64(0); } else if (v.tag == VT_U64) { push_u64((u64)strlen(v.payload.vp_ptr)); } else { push_i64(value_ptr_size(v)-4); }  } while(0)

#define do_pack_cons() do { value_t cdr = pop_value(); value_t car = pop_value(); push_value(mkcell(car,cdr)); } while(0)
#define mw_prim_pack_nil()  push_u64(0)
#define mw_prim_pack_cons() do_pack_cons();
#define mw_prim_pack_uncons() do_pack_uncons();

#define mw_prim_mut_new() do { value_t car = pop_value(); value_t cdr = { 0 }; push_value(mkcell_raw(car,cdr)); } while(0)
#define mw_prim_mut_get() do { value_t mut = pop_value(); if (mut.tag == VT_U64 && mut.payload.vp_valueptr) { value_t val =*mut.payload.vp_valueptr; push_value(val); incref(val); } else { push_value(mut); do_pack_uncons(); pop_value(); } } while(0)
#define mw_prim_mut_set() do { value_t mut = pop_value(); value_t newval = pop_value(); push_value(mut); if (mut.tag == VT_U64 && mut.payload.vp_valueptr) { value_t oldval = *mut.payload.vp_valueptr; *mut.payload.vp_valueptr = newval; decref(oldval); } else { usize cellidx = get_cell_index(mut); if (cellidx) { cell_t* cell = heap + cellidx; value_t oldval = cell->car; cell->car = newval; decref(oldval); } else { decref(newval); } } } while(0)

/* GENERATED C99 */
