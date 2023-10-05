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
