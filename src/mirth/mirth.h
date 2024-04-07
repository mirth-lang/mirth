/* MIRTH HEADER */
#line 3 "src/mirth/mirth.h"

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

typedef uint16_t TAG;
#define REFS_FLAG 	 0x8000
#define TUP_FLAG 	 0x4000
#define TUP_LEN_MASK 0x3FFF
#define TUP_LEN_MAX  0x3FFF

#define TAG_INT 1
#define TAG_STR (2 | REFS_FLAG)
#define TAG_PTR 3
#define TAG_FNPTR 4
#define TAG_TUP_NIL TUP_FLAG
#define TAG_TUP_LEN(t) ((t) & TUP_LEN_MASK)
#define TAG_TUP(n) (TUP_FLAG | REFS_FLAG | (n))

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
	void* ptr;
	FNPTR fnptr;
	REFS* refs;
	struct TUP* tup;
	struct STR* str;
} DATA;

typedef struct VAL {
	DATA data;
	TAG tag;
} VAL;

#define VALEQ(v1,v2) (((v1).tag == (v2).tag) && ((v1).data.u64 == (v2).data.u64))

#define VREFS(v)  (*(v).data.refs)
#define VINT(v)   ((v).data.i64)
#define VI64(v)   ((v).data.i64)
#define VU64(v)   ((v).data.u64)
#define VPTR(v)   ((v).data.ptr)
#define VFNPTR(v) ((v).data.fnptr)
#define VSTR(v)   ((v).data.str)
#define VTUP(v)   ((v).data.tup)
#define VTUPLEN(v) (TAG_TUP_LEN((v).tag))

#define HAS_REFS(v) ((v).tag & REFS_FLAG)
#define IS_INT(v)   ((v).tag == TAG_INT)
#define IS_U64(v)   ((v).tag == TAG_INT)
#define IS_I64(v)   ((v).tag == TAG_INT)
#define IS_PTR(v)   ((v).tag == TAG_PTR)
#define IS_FNPTR(v) ((v).tag == TAG_FNPTR)
#define IS_STR(v)   ((v).tag == TAG_STR)
#define IS_TUP(v)   ((v).tag & TUP_FLAG)
#define IS_NIL(v)   (IS_TUP(v) && (VTUPLEN(v) == 0))

#define MKINT(x)   ((VAL){.tag=TAG_INT, .data={.i64=(x)}})
#define MKI64(x)   ((VAL){.tag=TAG_INT, .data={.i64=(x)}})
#define MKU64(x)   ((VAL){.tag=TAG_INT, .data={.u64=(x)}})
#define MKFNPTR(x) ((VAL){.tag=TAG_FNPTR, .data={.fnptr=(x)}})
#define MKPTR(x)   ((VAL){.tag=TAG_PTR, .data={.ptr=(x)}})
#define MKSTR(x)   ((VAL){.tag=TAG_STR, .data={.str=(x)}})
#define MKTUP(x,n) ((VAL){.tag=TAG_TUP(n), .data={.tup=(x)}})
#define MKNIL_C	         {.tag=TAG_TUP_NIL, .data={.tup=NULL}}
#define MKNIL      ((VAL)MKNIL_C)

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
static void mw_std_prim_prim_debug(void);
static void mw_std_prim_prim_rdebug(void);

#if MIRTH_DEBUG
	typedef struct LOC {
		FNPTR fnptr;
		const char* word;
		const char* path;
		USIZE line, col;
		const char* atom;
	} LOC;
	static USIZE fstack_counter = 0;
	static LOC fstack [STACK_MAX] = {
		{
			.fnptr=(void(*)(void))0,
			.word="<word>",
			.path="<path>",
			.line=0, .col=0,
			.atom="<atom>"
		},
	};

	#define WORD_ENTER(_f,_w,_p,_l,_c) \
		do { \
			fstack[fstack_counter].fnptr = (_f); \
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

	#define WORD_EXIT(_f) \
		do { \
			if ((fstack_counter == 0) || (fstack[fstack_counter-1].fnptr != (_f))) { \
				TRACE("mismatched WORD_EXIT, expected " #_f "\n"); \
				exit(1); \
			} \
			fstack_counter--; \
		} while(0)
	#define PRIM_ENTER(_f,_w) WORD_ENTER(_f,_w,__FILE__,__LINE__,1)
	#define PRIM_EXIT(_f) WORD_EXIT(_f)
#else
	#define PRIM_ENTER(_f,_w)
	#define PRIM_EXIT(_f)
#endif

#define TRACE(x) write(2,x,strlen(x))
#define _STR(x) #x
#define STR(x) _STR(x)

#define EXPECT(test,msg) \
	do { \
		if (!(test)) { \
			TRACE(msg "\n"); \
			mw_std_prim_prim_debug(); \
			mw_std_prim_prim_rdebug(); \
			exit(1); \
		} \
	} while(0)

#define EXPECT1(test,msg,v1) \
	do { \
		if (!(test)) { \
			TRACE(msg "\n"); \
			push_value(v1); \
			mw_std_prim_prim_debug(); \
			mw_std_prim_prim_rdebug(); \
			exit(1); \
		} \
	} while(0)

#define EXPECT2(test,msg,v1,v2) \
	do { \
		if (!(test)) { \
			TRACE(msg "\n"); \
			push_value(v1); \
			push_value(v2); \
			mw_std_prim_prim_debug(); \
			mw_std_prim_prim_rdebug(); \
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
	ASSERT(HAS_REFS(v));
	ASSERT(VREFS(v) == 0);
	ASSERT1(IS_TUP(v)||IS_STR(v), v);
	if (IS_TUP(v)) {
		TUP* tup = VTUP(v);
		ASSERT(tup);
		for (TUPLEN i = 0; i < tup->size; i++) {
			decref(tup->cells[i]);
		}
		free(tup);
	} else if (IS_STR(v)) {
		STR* str = VSTR(v);
		ASSERT(str);
		free(str);
	}
}

static void value_uncons_c(VAL val, VAL* tail, VAL* head) {
	if (IS_TUP(val)) {
		TUPLEN len = VTUPLEN(val);
		TUP* tup = VTUP(val);
		ASSERT1((len > 0) && tup, val);
		VAL tailval = MKTUP(tup, len-1);
		VAL headval = tup->cells[len-1];
		if (len == 1) {
			incref(headval);
			decref(val);
			tailval = MKNIL;
		} else {
			if (tup->refs == 1) {
				for (TUPLEN i=len; i < tup->size; i++) { decref(tup->cells[i]); }
				memset(tup->cells + (len-1), 0, sizeof(VAL)*(tup->size - (len-1)));
				tup->size = len-1;
			} else {
				incref(headval);
			}
			if (len == 2) {
				VAL ptval = tup->cells[0];
				if (!IS_TUP(ptval)) {
					incref(ptval);
					decref(tailval);
					tailval = ptval;
				}
			}
		}
		*tail = tailval;
		*head = headval;
	} else {
		*tail = MKNIL;
		*head = val;
	}
}

static uint64_t value_u64 (VAL v) {
	ASSERT1(IS_INT(v),v);
	return VU64(v);
}

static int64_t value_i64 (VAL v) {
	ASSERT1(IS_INT(v),v);
	return VI64(v);
}

static void* value_ptr (VAL v) {
	ASSERT1(IS_PTR(v),v);
	return VPTR(v);
}

static FNPTR value_fnptr (VAL v) {
	ASSERT1(IS_FNPTR(v),v);
	return VFNPTR(v);
}

#define pop_u8() ((uint8_t)pop_u64())
#define pop_u16() ((uint16_t)pop_u64())
#define pop_u32() ((uint32_t)pop_u64())
#define pop_u64() (value_u64(pop_value()))
#define pop_i8() ((int8_t)pop_i64())
#define pop_i16() ((int16_t)pop_i64())
#define pop_i32() ((int32_t)pop_i64())
#define pop_i64() (value_i64(pop_value()))
#define pop_usize() (pop_u64())
#define pop_bool() (pop_u64())
#define pop_ptr() (value_ptr(pop_value()))
#define pop_fnptr() (value_fnptr(pop_value()))

#define push_u64(v) push_value(MKU64(v))
#define push_i64(v) push_value(MKI64(v))
#define push_usize(v) push_u64((uint64_t)(v))
#define push_bool(b) push_u64((uint64_t)((bool)(b)))
#define push_u8(b) push_u64((uint64_t)(b))
#define push_u16(b) push_u64((uint64_t)(b))
#define push_u32(b) push_u64((uint64_t)(b))
#define push_i8(b) push_i64((int64_t)(b))
#define push_i16(b) push_i64((int64_t)(b))
#define push_i32(b) push_i64((int64_t)(b))
#define push_ptr(p) push_value(MKPTR(p))
#define push_fnptr(p) push_value(MKFNPTR(p))

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

static void push_resource(VAL x) {
	ASSERT(rstack_counter > 0);
	rstack[--rstack_counter] = x;
}

static VAL top_resource(void) {
	ASSERT(rstack_counter < STACK_MAX);
	return rstack[rstack_counter];
}

static VAL pop_resource(void) {
	ASSERT(rstack_counter < STACK_MAX);
	return rstack[rstack_counter++];
}

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

// Create a TUP with at least min(max(old_tup->size, cap_hint), TUP_LEN_MAX) capacity.
// Consume old_tup and copy its elements over to the new tuple.
static TUP* tup_resize (TUP* old_tup, TUPLEN cap_hint) {
	ASSERT(old_tup);
	if (cap_hint < old_tup->size) cap_hint = old_tup->size;
	if (old_tup->refs == 1) {
		if (cap_hint < 3) cap_hint = 3;
		if (cap_hint > TUP_LEN_MAX) cap_hint = TUP_LEN_MAX;
		TUPLEN old_cap = old_tup->cap;
		TUP *new_tup = realloc(old_tup, sizeof(TUP) + sizeof(VAL)*(USIZE)cap_hint);
		ASSERT(new_tup);
		if (old_cap < cap_hint) {
			memset(new_tup->cells + old_cap, 0, sizeof(VAL)*(cap_hint - old_cap));
		}
		new_tup->cap = cap_hint;
		return new_tup;
	} else {
		TUP* new_tup = tup_new(cap_hint);
		for (TUPLEN i = 0; i < old_tup->size; i++) {
			VAL v = old_tup->cells[i];
			new_tup->cells[i] = v;
			incref(v);
		}
		new_tup->size = old_tup->size;
		old_tup->refs--;
		return new_tup;
	}
}

static VAL mkcons_hint (VAL tail, VAL head, TUPLEN cap_hint) {
	if (IS_TUP(tail) && HAS_REFS(tail)) {
		TUPLEN tail_len = VTUPLEN(tail);
		TUP *tail_tup = VTUP(tail);
		ASSERT1(tail_tup, tail);
		ASSERT1(tail_len <= tail_tup->size, tail);
		if (tail_len < tail_tup->size) {
			ASSERT1(tail_tup->refs >= 1, tail);
			if (tail_tup->refs == 1) {
				decref(tail_tup->cells[tail_len]);
				tail_tup->cells[tail_len] = head;
				return MKTUP(tail_tup, tail_len+1);
			} else {
				VAL *cmp = &tail_tup->cells[tail_len];
				if (VALEQ(*cmp, head)) {
					decref(head);
					return MKTUP(tail_tup, tail_len+1);
				} else {
					if (cap_hint < tail_len+1) cap_hint = 2*tail_len+1;
					TUP* new_tup = tup_new(cap_hint);
					for (TUPLEN i = 0; i < tail_len; i++) {
						VAL v = tail_tup->cells[i];
						new_tup->cells[i] = v;
						incref(v);
					}
					new_tup->cells[tail_len] = head;
					new_tup->size = tail_len+1;
					tail_tup->refs--;
					return MKTUP(new_tup, tail_len+1);
				}
			}
		} else {
			ASSERT1(tail_len < TUP_LEN_MAX, tail);
			ASSERT1(tail_len <= tail_tup->cap, tail);
			if (tail_len < tail_tup->cap) {
				tail_tup->cells[tail_len] = head;
				tail_tup->size = tail_len+1;
				return MKTUP(tail_tup, tail_len+1);
			} else {
				if (cap_hint < tail_len+1) cap_hint = 2*tail_len+1;
				TUP* new_tup = tup_resize(tail_tup, cap_hint);
				ASSERT(tail_len < new_tup->cap);
				new_tup->size = tail_len+1;
				new_tup->cells[tail_len] = head;
				return MKTUP(new_tup, tail_len+1);
			}
		}
	} else if (IS_TUP(tail)) { // cons onto nil
		ASSERT(IS_NIL(tail));
		if (IS_TUP(head)) {
			TUP* tup = tup_new(cap_hint);
			tup->size = 1;
			tup->cells[0] = head;
			return MKTUP(tup,1);
		} else { // non-tup value pretends to be unary tuple
			return head;
		}
	} else { // cons onto non-tup value pretending to be unary tuple
		TUP* tup = tup_new(cap_hint);
		tup->size = 2;
		tup->cells[0] = tail;
		tup->cells[1] = head;
		return MKTUP(tup,2);
	}
}
static VAL mkcons(VAL tail, VAL head) {
	VAL v = mkcons_hint(tail,head,3);
	return v;
}

static VAL lpop(VAL* stk) {
	VAL cons=*stk, lcar, lcdr; value_uncons_c(cons, &lcar, &lcdr);
	*stk=lcar; return lcdr;
}
static void lpush(VAL* stk, VAL cdr) { *stk = mkcons(*stk, cdr); }
#define LPOP(v) push_value(lpop(&(v)))
#define LPUSH(v) lpush(&(v),pop_value())

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
	VAL val, tail, head;
	val = pop_value();
	value_uncons_c(val, &tail, &head);
	push_value(tail);
	push_value(head);
}

static USIZE get_data_tag(VAL v) {
	if (IS_TUP(v)) {
		ASSERT(VTUPLEN(v) > 0);
		return VU64(VTUP(v)->cells[VTUPLEN(v)-1]);
	} else {
		return VU64(v);
	}
}

static USIZE get_top_data_tag(void) {
	return get_data_tag(top_value());
}

static USIZE get_top_resource_data_tag(void) {
	return get_data_tag(top_resource());
}

static int str_cmp_(STR* s1, STR* s2) {
	ASSERT(s1 && s2);
	USIZE n1 = s1->size;
	USIZE n2 = s2->size;
	USIZE n = (n1 < n2 ? n1 : n2);
	ASSERT(n < SIZE_MAX);
	int r = memcmp(s1->data, s2->data, (size_t)n);
	if (r) return r;
	if (n1 < n2) return -1;
	if (n1 > n2) return 1;
	return 0;
}

static void run_value(VAL v) {
	// TODO Make a closure tag or something.
	// As it is, this feels kinda wrong.
	VAL car, cdr;
	value_uncons_c(v, &car, &cdr);
	push_value(car);
	ASSERT(IS_FNPTR(cdr) && VFNPTR(cdr));
	VFNPTR(cdr)();
}

static void mw_std_prim_prim_id (void) {}
static void mw_std_prim_prim_dup (void) {
	PRIM_ENTER(mw_std_prim_prim_dup,"prim-dup");
	VAL v = top_value();
	push_value(v);
	incref(v);
	PRIM_EXIT(mw_std_prim_prim_dup);
}
static void mw_std_prim_prim_drop (void) {
	PRIM_ENTER(mw_std_prim_prim_drop,"prim-drop");
	VAL v = pop_value();
	decref(v);
	PRIM_EXIT(mw_std_prim_prim_drop);
}

static void mw_std_prim_prim_swap (void) {
	PRIM_ENTER(mw_std_prim_prim_swap,"prim-swap");
	VAL a = pop_value();
	VAL b = pop_value();
	push_value(a);
	push_value(b);
	PRIM_EXIT(mw_std_prim_prim_swap);
}

static void mw_std_prim_prim_rswap (void) {
	PRIM_ENTER(mw_std_prim_prim_rswap,"prim-rswap");
	VAL a = pop_resource();
	VAL b = pop_resource();
	push_resource(a);
	push_resource(b);
	PRIM_EXIT(mw_std_prim_prim_rswap);
}

static void mw_std_prim_prim_int_add (void) {
	PRIM_ENTER(mw_std_prim_prim_int_add,"prim-int-add");
	// TODO promote to bigint on overflow.
	int64_t b = pop_i64();
	int64_t a = pop_i64();
	if (b >= 0) {
		EXPECT(a <= INT64_MAX - b, "integer overflow during addition (too positive)");
	} else {
		EXPECT(a >= INT64_MIN - b, "integer overflow during addition (too negative)");
	}
	push_i64(a + b);
	PRIM_EXIT(mw_std_prim_prim_int_add);
}
static void mw_std_prim_prim_int_sub (void) {
	PRIM_ENTER(mw_std_prim_prim_int_sub,"prim-int-sub");
	// TODO promote to bigint on overflow
	int64_t b = pop_i64();
	int64_t a = pop_i64();
	if (b >= 0) {
		EXPECT(a >= INT64_MIN + b, "integer overflow during subtraction (too negative)");
	} else {
		EXPECT(a <= INT64_MAX + b, "integer overflow during subtraction (too positive)");
	}
	push_i64(a - b);
	PRIM_EXIT(mw_std_prim_prim_int_sub);
}
static void mw_std_prim_prim_int_mul (void) {
	PRIM_ENTER(mw_std_prim_prim_int_mul,"prim-int-mul");
	// TODO promote to bigint on overflow
	int64_t b = pop_i64();
	int64_t a = pop_i64();
	// overflow checks for multiplication
	push_i64(a * b);
	PRIM_EXIT(mw_std_prim_prim_int_mul);
}
static void mw_std_prim_prim_int_div (void) {
	PRIM_ENTER(mw_std_prim_prim_int_div,"prim-int-div");
	// TODO promote to bigint on overflow
	int64_t b = pop_i64();
	int64_t a = pop_i64();
	EXPECT(b != 0, "divide by zero");
	EXPECT(!((b == -1) && (a == INT64_MIN)), "overflow during division");
	int64_t r = a % b;
	int64_t q = a / b;
	if (((a < 0) ^ (b < 0)) && r) q--;
	push_i64(q);
	PRIM_EXIT(mw_std_prim_prim_int_div);
}
static void mw_std_prim_prim_int_mod (void) {
	PRIM_ENTER(mw_std_prim_prim_int_mod,"prim-int-mod");
	int64_t b = pop_i64();
	int64_t a = pop_i64();
	EXPECT(b != 0, "divide by zero");
	if (b == -1) { push_i64(0); return; }
	int64_t r = a % b;
	int64_t q = a / b;
	if (((a < 0) ^ (b < 0)) && r) r += b;
	push_i64(r);
	PRIM_EXIT(mw_std_prim_prim_int_mod);
}

static void mw_std_prim_prim_int_and (void) {
	PRIM_ENTER(mw_std_prim_prim_int_and,"prim-int-and");
	uint64_t b = pop_u64();
	uint64_t a = pop_u64();
	push_u64(a & b);
	PRIM_EXIT(mw_std_prim_prim_int_and);
}
static void mw_std_prim_prim_int_or (void) {
	PRIM_ENTER(mw_std_prim_prim_int_or,"prim-int-or");
	uint64_t b = pop_u64();
	uint64_t a = pop_u64();
	push_u64(a | b);
	PRIM_EXIT(mw_std_prim_prim_int_or);
}
static void mw_std_prim_prim_int_xor (void) {
	PRIM_ENTER(mw_std_prim_prim_int_xor,"prim-int-xor");
	uint64_t b = pop_u64();
	uint64_t a = pop_u64();
	push_u64(a ^ b);
	PRIM_EXIT(mw_std_prim_prim_int_xor);
}
static void mw_std_prim_prim_int_shl (void) {
	PRIM_ENTER(mw_std_prim_prim_int_shl,"prim-int-shl");
	uint64_t b = pop_u64();
	uint64_t a = pop_u64();
	push_u64((b >= 64) ? 0 : (a << b));
	PRIM_EXIT(mw_std_prim_prim_int_shl);
}
static void mw_std_prim_prim_int_shr (void) {
	PRIM_ENTER(mw_std_prim_prim_int_shr,"prim-int-shr");
	uint64_t b = pop_u64();
	uint64_t a = pop_u64();
	push_u64((b >= 64) ? 0 : (a >> b));
	PRIM_EXIT(mw_std_prim_prim_int_shr);
}

static void mw_std_prim_prim_int_eq (void) {
	PRIM_ENTER(mw_std_prim_prim_int_eq,"prim-int-eq");
	VAL b = pop_value();
	VAL a = pop_value();
	ASSERT1(IS_INT(a), a);
	ASSERT1(IS_INT(b), a);
	push_bool(VINT(a) == VINT(b));
	PRIM_EXIT(mw_std_prim_prim_int_eq);
}
static void mw_std_prim_prim_int_lt (void) {
	PRIM_ENTER(mw_std_prim_prim_int_lt,"prim-int-lt");
	VAL b = pop_value();
	VAL a = pop_value();
	ASSERT2(IS_INT(a) && IS_INT(b), a, b);
	push_bool(VINT(a) < VINT(b));
	PRIM_EXIT(mw_std_prim_prim_int_lt);
}
static void mw_std_prim_prim_str_cmp (void) {
	PRIM_ENTER(mw_std_prim_prim_str_cmp,"prim-str-cmp");
	VAL b = pop_value();
	VAL a = pop_value();
	ASSERT2(IS_STR(a) && IS_STR(b), a, b);
	int64_t cmp = str_cmp_(VSTR(a), VSTR(b));
	push_i64(cmp);
	decref(a); decref(b);
	PRIM_EXIT(mw_std_prim_prim_str_cmp);
}

static void mw_std_prim_prim_sys_argc (void) {
	PRIM_ENTER(mw_std_prim_prim_sys_argc,"prim-sys-argc");
	push_i64(global_argc);
	PRIM_EXIT(mw_std_prim_prim_sys_argc);
}
static void mw_std_prim_prim_sys_argv (void) {
	PRIM_ENTER(mw_std_prim_prim_sys_argv,"prim-sys-argv");
	push_ptr(global_argv);
	PRIM_EXIT(mw_std_prim_prim_sys_argv);
}

static void mw_std_prim_prim_posix_write (void) {
	PRIM_ENTER(mw_std_prim_prim_posix_write,"prim-posix-write");
	USIZE n = pop_usize();
	VAL vp = pop_value();
	void* p = value_ptr(vp);
	int fd = (int)pop_i64();
	ASSERT(n <= SIZE_MAX);
	push_i64((int64_t)write(fd, p, (size_t)n));
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_posix_write);
}
static void mw_std_prim_prim_posix_read (void) {
	PRIM_ENTER(mw_std_prim_prim_posix_read,"prim-posix-read");
	USIZE n = pop_usize();
	VAL vp = pop_value();
	void* p = value_ptr(vp);
	int fd = (int)pop_i64();
	ASSERT(n <= SIZE_MAX);
	push_i64((int64_t)read(fd, p, (size_t)n));
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_posix_read);
}
static void mw_std_prim_prim_posix_open (void) {
	PRIM_ENTER(mw_std_prim_prim_posix_open,"prim-posix-open");
	int m = (int)pop_i64();
	int f = (int)pop_i64();
	VAL vp = pop_value();
	void* path = value_ptr(vp);
	push_i64((int64_t)open(path,f,m));
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_posix_open);
}
static void mw_std_prim_prim_posix_close (void) {
	PRIM_ENTER(mw_std_prim_prim_posix_close,"prim-posix-close");
	int fd = (int)pop_i64();
	push_i64((int64_t)close(fd));
	PRIM_EXIT(mw_std_prim_prim_posix_close);
}
static void mw_std_prim_prim_posix_exit (void) {
	PRIM_ENTER(mw_std_prim_prim_posix_exit,"prim-posix-exit");
	int x = (int)pop_i64();
	exit(x);
	PRIM_EXIT(mw_std_prim_prim_posix_exit);
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

void int_trace_(int64_t y, int fd) {
	char* p; size_t n;
	int_repr(y, &p, &n);
	write(fd, p, n);
}

void mw_std_prim_prim_int_to_str(void) {
	PRIM_ENTER(mw_std_prim_prim_int_to_str,"prim-int-to-str");
	int64_t x = pop_i64();
	bool cache = (0 <= x) && (x <= 255);
	static VAL scache[256] = {0};
	if (cache && scache[x].tag) {
		incref(scache[x]);
		push_value(scache[x]);
	} else {
		char* p; size_t n;
		int_repr(x,&p,&n);
		VAL out = mkstr(p,n);
		push_value(out);
		if (cache) {
			scache[x] = out;
			incref(out);
		}
	}
	PRIM_EXIT(mw_std_prim_prim_int_to_str);
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
	} else if (IS_PTR(val)) {
		write(fd, "<ptr>", 5);
	} else if (IS_FNPTR(val)) {
		write(fd, "<fnptr>", 7);
	} else if (IS_TUP(val)) {
		TUPLEN len = VTUPLEN(val);
		TUP* tup = VTUP(val);
		if (VTUPLEN(val) == 0) {
			write(fd, "[]", 2);
		} else {
			write(fd, "[", 2);
			for(TUPLEN i = 0; i < len; i++) {
				if (i > 0) write(fd, " ", 1);
				value_trace_(tup->cells[i], fd);
			}
			write(fd, "]", 1);
		}
	} else {
		TRACE("value cannot be traced");
		exit(1);
	}
}

static void mw_std_prim_prim_debug (void) {
	TRACE("??");
	for (long i = STACK_MAX-1; i >= (long)stack_counter; i--) {
		TRACE(" ");
		value_trace_(stack[i], 2);
	}
	TRACE("\n");
}

static void mw_std_prim_prim_rdebug (void) {
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
			int_trace_((int64_t)fstack[i-1].line, 2);
			TRACE(":");
			int_trace_((int64_t)fstack[i-1].col, 2);
			TRACE("\n");
		}
	#endif
}

static void mw_std_prim_prim_panic(void) {
	// TODO: expect less of the stack, i.e. panic gracefully even if stack
	// is in a weird state ... this is panic! after all
	VAL v = pop_value();
	ASSERT(IS_STR(v));
	ASSERT(VSTR(v)->size < SIZE_MAX);
	write(2,VSTR(v)->data, (size_t)VSTR(v)->size);
	mw_std_prim_prim_debug();
	mw_std_prim_prim_rdebug();
	exit(1);
}

static void mw_std_prim_prim_ptr_get (void) {
	PRIM_ENTER(mw_std_prim_prim_ptr_get,"prim-ptr-get");
	VAL vp = pop_value();
	void **p = value_ptr(vp);
	EXPECT(p, "tried to load from null pointer");
	push_ptr(*p);
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_ptr_get);
}

static void mw_std_prim_prim_u8_get (void) {
	PRIM_ENTER(mw_std_prim_prim_u8_get,"prim-u8-get");
	VAL vp = pop_value();
	uint8_t *p = value_ptr(vp);
	EXPECT(p, "tried to load from null pointer");
	push_u8(*p);
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_u8_get);
}

static void mw_std_prim_prim_u16_get (void) {
	PRIM_ENTER(mw_std_prim_prim_u16_get,"prim-u16-get");
	VAL vp = pop_value();
	uint16_t *p = value_ptr(vp);
	EXPECT(p, "tried to load from null pointer");
	push_u16(*p);
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_u16_get);
}

static void mw_std_prim_prim_u32_get (void) {
	PRIM_ENTER(mw_std_prim_prim_u32_get,"prim-u32-get");
	VAL vp = pop_value();
	uint32_t *p = value_ptr(vp);
	EXPECT(p, "tried to load from null pointer");
	push_u32(*p);
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_u32_get);
}

static void mw_std_prim_prim_u64_get (void) {
	PRIM_ENTER(mw_std_prim_prim_u64_get,"prim-u64-get");
	VAL vp = pop_value();
	uint64_t *p = value_ptr(vp);
	EXPECT(p, "tried to load from null pointer");
	push_u64(*p);
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_u64_get);
}

static void mw_std_prim_prim_i8_get (void) {
	PRIM_ENTER(mw_std_prim_prim_i8_get,"prim-i8-get");
	VAL vp = pop_value();
	int8_t *p = value_ptr(vp);
	EXPECT(p, "tried to load from null pointer");
	push_i8(*p);
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_i8_get);
}

static void mw_std_prim_prim_i16_get (void) {
	PRIM_ENTER(mw_std_prim_prim_i16_get,"prim-i16-get");
	VAL vp = pop_value();
	int16_t *p = value_ptr(vp);
	EXPECT(p, "tried to load from null pointer");
	push_i16(*p);
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_i16_get);
}

static void mw_std_prim_prim_i32_get (void) {
	PRIM_ENTER(mw_std_prim_prim_i32_get,"prim-i32-get");
	VAL vp = pop_value();
	int32_t *p = value_ptr(vp);
	EXPECT(p, "tried to load from null pointer");
	push_i32(*p);
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_i32_get);
}

static void mw_std_prim_prim_i64_get (void) {
	PRIM_ENTER(mw_std_prim_prim_i64_get,"prim-i64-get");
	VAL vp = pop_value();
	int64_t *p = value_ptr(vp);
	EXPECT(p, "tried to load from null pointer");
	push_i64(*p);
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_i64_get);
}

static void mw_std_prim_prim_int_set (void) {
	PRIM_ENTER(mw_std_prim_prim_int_set,"prim-int-set");
	VAL vp = pop_value();
	int64_t *p = value_ptr(vp);
	EXPECT(p, "tried to write to null pointer");
	*p = pop_i64();
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_int_set);
}

static void mw_std_prim_prim_ptr_set (void) {
	PRIM_ENTER(mw_std_prim_prim_ptr_set,"prim-ptr-set");
	VAL vp = pop_value();
	void **p = value_ptr(vp);
	EXPECT(p, "tried to write to null pointer");
	*p = pop_ptr();
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_ptr_set);
}

static void mw_std_prim_prim_u8_set (void) {
	PRIM_ENTER(mw_std_prim_prim_u8_set,"prim-u8-set");
	VAL vp = pop_value();
	uint8_t *p = value_ptr(vp);
	EXPECT(p, "tried to write to null pointer");
	*p = pop_u8();
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_u8_set);
}

static void mw_std_prim_prim_u16_set (void) {
	PRIM_ENTER(mw_std_prim_prim_u16_set,"prim-u16-set");
	VAL vp = pop_value();
	uint16_t *p = value_ptr(vp);
	EXPECT(p, "tried to write to null pointer");
	*p = pop_u16();
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_u16_set);
}

static void mw_std_prim_prim_u32_set (void) {
	PRIM_ENTER(mw_std_prim_prim_u32_set,"prim-u32-set");
	VAL vp = pop_value();
	uint32_t *p = value_ptr(vp);
	EXPECT(p, "tried to write to null pointer");
	*p = pop_u32();
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_u32_set);
}

static void mw_std_prim_prim_u64_set (void) {
	PRIM_ENTER(mw_std_prim_prim_u64_set,"prim-u64-set");
	VAL vp = pop_value();
	uint64_t *p = value_ptr(vp);
	EXPECT(p, "tried to write to null pointer");
	*p = pop_u64();
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_u64_set);
}

static void mw_std_prim_prim_i8_set (void) {
	PRIM_ENTER(mw_std_prim_prim_i8_set,"prim-i8-set");
	VAL vp = pop_value();
	int8_t *p = value_ptr(vp);
	EXPECT(p, "tried to write to null pointer");
	*p = pop_i8();
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_i8_set);
}

static void mw_std_prim_prim_i16_set (void) {
	PRIM_ENTER(mw_std_prim_prim_i16_set,"prim-i16-set");
	VAL vp = pop_value();
	int16_t *p = value_ptr(vp);
	EXPECT(p, "tried to write to null pointer");
	*p = pop_i16();
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_i16_set);
}

static void mw_std_prim_prim_i32_set (void) {
	PRIM_ENTER(mw_std_prim_prim_i32_set,"prim-i32-set");
	VAL vp = pop_value();
	int32_t *p = value_ptr(vp);
	EXPECT(p, "tried to write to null pointer");
	*p = pop_i32();
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_i32_set);
}

static void mw_std_prim_prim_i64_set (void) {
	PRIM_ENTER(mw_std_prim_prim_i64_set,"prim-i64-set");
	VAL vp = pop_value();
	int64_t *p = value_ptr(vp);
	EXPECT(p, "tried to write to null pointer");
	*p = pop_i64();
	decref(vp);
	PRIM_EXIT(mw_std_prim_prim_i64_set);
}


#if defined(MIRTH_WINDOWS)
#define mw_std_prim_prim_sys_os() push_u64(1)
#elif defined(MIRTH_LINUX)
#define mw_std_prim_prim_sys_os() push_u64(2)
#elif defined(MIRTH_MACOS)
#define mw_std_prim_prim_sys_os() push_u64(3)
#else
#define mw_std_prim_prim_sys_os() push_u64(0)
#endif

static void mw_std_prim_prim_run (void) {
	PRIM_ENTER(mw_std_prim_prim_run,"prim-run");
	VAL f = pop_value();
	run_value(f);
	PRIM_EXIT(mw_std_prim_prim_run);
}

static void mw_std_prim_prim_ptr_nil (void) {
	PRIM_ENTER(mw_std_prim_prim_ptr_nil,"prim-ptr-nil");
	push_ptr((void*)0);
	PRIM_EXIT(mw_std_prim_prim_ptr_nil);
}
static void mw_std_prim_prim_ptr_eq (void) {
	PRIM_ENTER(mw_std_prim_prim_ptr_eq,"prim-ptr-eq");
	void* a = pop_ptr();
	void* b = pop_ptr();
	push_bool(a == b);
	PRIM_EXIT(mw_std_prim_prim_ptr_eq);
}
static void mw_std_prim_prim_ptr_add (void) {
	PRIM_ENTER(mw_std_prim_prim_ptr_add,"prim-ptr-add");
	VAL vptr = pop_value();
	USIZE n = pop_usize();
	ASSERT1(IS_PTR(vptr), vptr);
	EXPECT(VPTR(vptr), "attempt to add to null pointer");
	char* ptr = (char*)VPTR(vptr);
	push_ptr(ptr + n);
	PRIM_EXIT(mw_std_prim_prim_ptr_add);
}
#define mw_std_prim_prim_ptr_size() push_u64((uint64_t)sizeof(void*))
static void mw_std_prim_prim_ptr_alloc (void) {
	PRIM_ENTER(mw_std_prim_prim_ptr_alloc,"prim-ptr-alloc");
	USIZE n = pop_usize();
	void* p = malloc((size_t)n);
	EXPECT(p, "failed to allocate buffer");
	push_ptr(p);
	PRIM_EXIT(mw_std_prim_prim_ptr_alloc);
}
static void mw_std_prim_prim_ptr_realloc (void) {
	PRIM_ENTER(mw_std_prim_prim_ptr_realloc,"prim-ptr-realloc");
	USIZE n = pop_usize();
	void* p0 = pop_ptr();
	void* p1 = realloc(p0, (size_t)n);
	EXPECT(p1, "failed to reallocate buffer");
	push_ptr(p1);
	PRIM_EXIT(mw_std_prim_prim_ptr_realloc);
}
static void mw_std_prim_prim_ptr_free (void) {
	PRIM_ENTER(mw_std_prim_prim_ptr_free,"prim-ptr-free");
	void* p = pop_ptr();
	free(p);
	PRIM_EXIT(mw_std_prim_prim_ptr_free);
}

static void mw_std_prim_prim_ptr_copy (void) {
	PRIM_ENTER(mw_std_prim_prim_ptr_copy,"prim-ptr-copy");
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
	PRIM_EXIT(mw_std_prim_prim_ptr_copy);
}

static void mw_std_prim_prim_ptr_fill (void) {
	PRIM_ENTER(mw_std_prim_prim_ptr_fill,"prim-ptr-fill");
	VAL vdst = pop_value();
	ASSERT1(IS_PTR(vdst), vdst);
	int64_t ilen = pop_i64();
	uint64_t val = pop_u64();
	void* dst = value_ptr(vdst);
	if (dst && (ilen > 0)) {
		ASSERT((USIZE)ilen <= SIZE_MAX);
		memset(dst, (int)val, (size_t)ilen);
	}
	PRIM_EXIT(mw_std_prim_prim_ptr_fill);
}

static void mw_std_prim_prim_str_copy (void) {
	PRIM_ENTER(mw_std_prim_prim_str_copy,"prim-str-copy");
	USIZE size = pop_usize();
	char* ptr = (char*)pop_ptr();
	ASSERT(size <= SIZE_MAX-sizeof(STR)-4);
	ASSERT(ptr);
	push_value(mkstr(ptr, size));
	PRIM_EXIT(mw_std_prim_prim_str_copy);
}

static void mw_std_prim_prim_str_cat (void) {
	PRIM_ENTER(mw_std_prim_prim_str_cat,"prim-str-cat");
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
		if ((s1->refs == 1) && (m2 < m*2)) m2 = m*2;
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
	PRIM_EXIT(mw_std_prim_prim_str_cat);
}

static void mw_std_prim_prim_str_base (void) {
	PRIM_ENTER(mw_std_prim_prim_str_base,"prim-str-base");
	VAL vstr = pop_value();
	ASSERT1(IS_STR(vstr) && VSTR(vstr), vstr);
	push_ptr(VSTR(vstr)->data);
	decref(vstr);
	PRIM_EXIT(mw_std_prim_prim_str_base);
}

static void mw_std_prim_prim_str_num_bytes (void) {
	PRIM_ENTER(mw_std_prim_prim_str_num_bytes,"prim-str-num-bytes");
	VAL v = pop_value();
	ASSERT(IS_STR(v) && VSTR(v));
	push_usize(VSTR(v)->size);
	decref(v);
	PRIM_EXIT(mw_std_prim_prim_str_num_bytes);
}

static void mw_std_prim_prim_pack_nil (void) {
	PRIM_ENTER(mw_std_prim_prim_pack_nil,"prim-pack-nil");
	push_u64(0);
	PRIM_EXIT(mw_std_prim_prim_pack_nil);
}

static void mw_std_prim_prim_pack_cons (void) {
	PRIM_ENTER(mw_std_prim_prim_pack_cons,"prim-pack-cons");
	VAL cdr = pop_value();
	VAL car = pop_value();
	push_value(mkcons(car,cdr));
	PRIM_EXIT(mw_std_prim_prim_pack_cons);
}

static void mw_std_prim_prim_pack_uncons (void) {
	PRIM_ENTER(mw_std_prim_prim_pack_uncons,"prim-pack-uncons");
	do_uncons();
	PRIM_EXIT(mw_std_prim_prim_pack_uncons);
}

static void mw_std_prim_prim_mut_get (void) {
	PRIM_ENTER(mw_std_prim_prim_mut_get,"prim-mut-get");
	VAL mut = pop_value();
	ASSERT1(IS_PTR(mut) && VPTR(mut), mut);
	VAL v = *(VAL*)VPTR(mut);
	EXPECT(v.tag, "tried to read uninitialized value");
	push_value(v);
	incref(v);
	PRIM_EXIT(mw_std_prim_prim_mut_get);
}
static void mw_std_prim_prim_mut_set (void) {
	PRIM_ENTER(mw_std_prim_prim_mut_set,"prim-mut-set");
	VAL mut = pop_value();
	VAL newval = pop_value();
	ASSERT1(IS_PTR(mut) && VPTR(mut), mut);
	VAL oldval = *(VAL*)VPTR(mut);
	*(VAL*)VPTR(mut) = newval;
	if (oldval.tag) {
		decref(oldval);
	}
	decref(mut);
	PRIM_EXIT(mw_std_prim_prim_mut_set);
}
static void mw_std_prim_prim_mut_is_set (void) {
	PRIM_ENTER(mw_std_prim_prim_mut_is_set,"prim-mut-is-set");
	VAL mut = pop_value();
	ASSERT1(IS_PTR(mut) && VPTR(mut), mut);
	VAL val = *(VAL*)VPTR(mut);
	push_bool(val.tag);
	decref(mut);
	PRIM_EXIT(mw_std_prim_prim_mut_is_set);
}

/* GENERATED C99 */
