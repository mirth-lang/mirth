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

typedef uint64_t TAG;
#define REFS_FLAG 0x0001

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

static void default_free   (VAL v);
static void default_trace_ (VAL v, int fd);
static void default_run    (VAL v);

static void int_trace_ (VAL v, int fd);
static TYPE TYPE_INT = {
	.name = "Int",
	.flags = 0,
	.free = default_free,
	.trace_ = int_trace_,
	.run = default_run,
};

static void fnptr_run (VAL v);
static TYPE TYPE_FNPTR = {
	.name = "FnPtr",
	.flags = 0,
	.free = default_free,
	.trace_ = default_trace_,
	.run = fnptr_run,
};

static TYPE TYPE_F64 = {
	.name = "F64",
	.flags = 0,
	.free = default_free,
	.trace_ = default_trace_,
	.run = default_run,
};

static TYPE TYPE_F32 = {
	.name = "F32",
	.flags = 0,
	.free = default_free,
	.trace_ = default_trace_,
	.run = default_run,
};

static void str_free (VAL v);
static void str_trace_ (VAL v, int fd);
static TYPE TYPE_STR = {
	.name = "Str",
	.flags = REFS_FLAG,
	.free = str_free,
	.trace_ = str_trace_,
	.run = default_run,
};

static void tup_free (VAL v);
static void tup_trace_ (VAL v, int fd);
static void tup_run (VAL v);
static TYPE TYPE_TUP = {
	.name = "Tup",
	.flags = REFS_FLAG,
	.free = tup_free,
	.trace_ = tup_trace_,
	.run = tup_run,
};

#define TAG_INT ((TAG)&TYPE_INT)
#define TAG_PTR TAG_INT
#define TAG_FNPTR ((TAG)&TYPE_FNPTR)
#define TAG_F64 ((TAG)&TYPE_F64)
#define TAG_F32 ((TAG)&TYPE_F32)
#define TAG_STR (REFS_FLAG | (TAG)&TYPE_STR)
#define TAG_TUP (REFS_FLAG | (TAG)&TYPE_TUP)

#define VALEQ(v1,v2) (((v1).tag == (v2).tag) && ((v1).data.u64 == (v2).data.u64))

#define VTYPE(v)  ((const TYPE*)((v).tag & 0xFFFFFFFFFFFFFFFC))
#define VREFS(v)  (*(v).data.refs)

#define VINT(v)   ((v).data.i64)
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

#define HAS_REFS(v) (((v).tag & REFS_FLAG) && (v).data.refs)
#define IS_VAL(v)   (1)
#define IS_INT(v)   ((v).tag == TAG_INT)
#define IS_I64(v)   ((v).tag == TAG_INT)
#define IS_U64(v)   ((v).tag == TAG_INT)
#define IS_BOOL(v)  ((v).tag == TAG_INT)
#define IS_F32(v)   ((v).tag == TAG_F32)
#define IS_F64(v)   ((v).tag == TAG_F64)
#define IS_PTR(v)   ((v).tag == TAG_PTR)
#define IS_FNPTR(v) ((v).tag == TAG_FNPTR)
#define IS_STR(v)   ((v).tag == TAG_STR)
#define IS_TUP(v)   ((v).tag == TAG_TUP)
#define IS_NIL(v)   (IS_TUP(v) && (VTUPLEN(v) == 0))

#define MKVAL(x)   (x)
#define MKINT(x)   ((VAL){.tag=TAG_INT, .data={.i64=(x)}})
#define MKI64(x)   ((VAL){.tag=TAG_INT, .data={.i64=(x)}})
#define MKI32(x)   ((VAL){.tag=TAG_INT, .data={.i64=(x)}})
#define MKI16(x)   ((VAL){.tag=TAG_INT, .data={.i64=(x)}})
#define MKI8(x)    ((VAL){.tag=TAG_INT, .data={.i64=(x)}})
#define MKU64(x)   ((VAL){.tag=TAG_INT, .data={.u64=(x)}})
#define MKU32(x)   ((VAL){.tag=TAG_INT, .data={.u64=(x)}})
#define MKU16(x)   ((VAL){.tag=TAG_INT, .data={.u64=(x)}})
#define MKU8(x)    ((VAL){.tag=TAG_INT, .data={.u64=(x)}})
#define MKBOOL(x)  ((VAL){.tag=TAG_INT, .data={.u64=(x)}})
#define MKF32(x)   ((VAL){.tag=TAG_F32, .data={.f32=(x)}})
#define MKF64(x)   ((VAL){.tag=TAG_F64, .data={.f64=(x)}})
#define MKFNPTR(x) ((VAL){.tag=TAG_FNPTR, .data={.fnptr=(x)}})
#define MKPTR(x)   ((VAL){.tag=TAG_PTR, .data={.ptr=(x)}})

#define VSTR(v)    ((v).data.str)
#define MKSTR(x)   ((VAL){.tag=TAG_STR, .data={.str=(x)}})

#define VTUP(v)    ((v).data.tup)
#define VTUPLEN(v) (tup_len_(VTUP(v)))
#define MKTUP(x,n) ((VAL){.tag=TAG_TUP, .data={.tup=(x)}})
#define MKNIL      ((VAL){.tag=TAG_TUP, .data={.tup=NULL}})

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
	ASSERT(VTYPE(v)->free);
	ASSERT(HAS_REFS(v));
	ASSERT(VREFS(v) == 0);
	VTYPE(v)->free(v);
}

static void default_free (VAL v) {
	TRACE("panic! tried to free ");
	TRACE(VTYPE(v)->name);
	TRACE(" value\n");
	exit(1);
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

static uint64_t value_u64 (VAL v) { ASSERT1(IS_INT(v),v); return VU64(v); }
static uint32_t value_u32 (VAL v) { uint64_t x = value_u64(v); ASSERT1(x <= UINT32_MAX, v); return (uint32_t)x; }
static uint16_t value_u16 (VAL v) { uint64_t x = value_u64(v); ASSERT1(x <= UINT16_MAX, v); return (uint16_t)x; }
static uint8_t value_u8 (VAL v) { uint64_t x = value_u64(v); ASSERT1(x <= UINT8_MAX,  v); return (uint8_t)x; }

static int64_t value_i64 (VAL v) { ASSERT1(IS_INT(v),v); return VI64(v); }
static int32_t value_i32 (VAL v) { int64_t x = value_i64(v); ASSERT1((INT32_MIN <= x) && (x <= INT32_MAX), v); return (int32_t)x; }
static int16_t value_i16 (VAL v) { int64_t x = value_i64(v); ASSERT1((INT16_MIN <= x) && (x <= INT16_MAX), v); return (int16_t)x; }
static int8_t value_i8 (VAL v) { int64_t x = value_i64(v); ASSERT1((INT8_MIN <= x) && (x <= INT8_MAX),  v); return (int8_t)x; }
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

static STR* str_cat (STR* s1, STR* s2) {
	EXPECT(s1 && s2, "invalid strings in prim-str-cat");
	USIZE m = s1->cap;
	USIZE n1 = s1->size;
	USIZE n2 = s2->size;
	if ((s1->refs == 1) && (n1 + n2 + 4 <= m)) {
		ASSERT(n2 <= SIZE_MAX);
		memcpy(s1->data + n1, s2->data, (size_t)n2);
		s1->size += n2;
		ASSERT(s1->size + 4 <= s1->cap);
		decref(MKSTR(s2));
		return s1;
	} else {
		USIZE m2 = n1 + n2 + 4;
		if ((s1->refs == 1) && (m2 < m*2)) m2 = m*2;
		STR* str = str_alloc(m2);
		str->size = n1+n2;
		ASSERT(n1 <= SIZE_MAX);
		ASSERT(n2 <= SIZE_MAX);
		memcpy(str->data, s1->data, (size_t)n1);
		memcpy(str->data+n1, s2->data, (size_t)n2);
		decref(MKSTR(s1));
		decref(MKSTR(s2));
		return str;
	}
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

static void default_run(VAL v) {
	TRACE("panic! tried to run ");
	TRACE(VTYPE(v)->name);
	TRACE("value\n");
	exit(1);
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
	ASSERT(VTYPE(v)->run);
	VTYPE(v)->run(v);
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

static STR* f32_show (float d) {
 	char result[DBL_DIG+32] = {0};
	int len = sprintf(result, "%.*g", DBL_DIG, d);
	return str_make(result, len);
}

static STR* f64_show (double d) {
 	char result[DBL_DIG+32] = {0};
	int len = sprintf(result, "%.*g", DBL_DIG, d);
	return str_make(result, len);
}

void int_repr(int64_t y, char** out_ptr, size_t *out_size) {
	static char c[32] = {0};
	memset(c, 0, 32);
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
	*out_ptr = p;
	*out_size = n;
}

void int_trace_(VAL v, int fd) {
	char* p; size_t n;
	int_repr(VINT(v), &p, &n);
	write(fd, p, n);
}

STR* i64_show (int64_t x) {
	bool cache = (0 <= x) && (x <= 255);
	static STR* scache[256] = {0};
	if (cache && scache[x]) {
		STR* s = scache[x];
		incref(MKSTR(s));
		return s;
	} else {
		char* p; size_t n;
		int_repr(x,&p,&n);
		STR* s = str_make(p,n);
		if (cache) {
			scache[x] = s;
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
	ASSERT(VTYPE(v));
	ASSERT(VTYPE(v)->trace_);
	VTYPE(v)->trace_(v,fd);
}

static void default_trace_(VAL v, int fd) {
	const char* name = VTYPE(v)->name;
	write(fd, "<", 1);
	write(fd, name, strlen(name));
	write(fd, ">", 1);
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
			int_trace_(MKI64(fstack[i-1].line), 2);
			TRACE(":");
			int_trace_(MKI64(fstack[i-1].col), 2);
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
