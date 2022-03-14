// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "fastrandom_nif.h"

#define UINT64MASK (uint64_t)(0xffffffffffffffff)

#include "splitmix64.h"
#include "xoroshiro116p.h"
#include "xoshiro256p.h"
#include "xoshiro256px8.h"
#include "xoshiro256pp.h"
#include "xoshiro256ppx8.h"

/* Global Variables */

static fastrandom_nif_atom_table_t fastrandom_nif_atom_table_internal;

fastrandom_nif_atom_table_t *fastrandom_nif_atom_table = &fastrandom_nif_atom_table_internal;

/* Static Variables */

typedef struct fastrandom_nif_ctx_s fastrandom_nif_ctx_t;

struct fastrandom_nif_ctx_s {
    xoroshiro116p_state_t xoroshiro116p;
    xoshiro256p_state_t xoshiro256p;
    xoshiro256px8_state_t xoshiro256px8;
    xoshiro256pp_state_t xoshiro256pp;
    xoshiro256ppx8_state_t xoshiro256ppx8;
};

ErlNifResourceType *fastrandom_nif_ctx_resource_type = NULL;
static ErlNifTSDKey fastrandom_nif_tsd_key;
static ErlNifRWLock *fastrandom_nif_global_rwlock = NULL;
static uint64_t fastrandom_nif_global_seed = 0;
static fastrandom_nif_ctx_t fastrandom_nif_global_ctx;

static void fastrandom_nif_ctx_jump(fastrandom_nif_ctx_t *ctx);
static void fastrandom_nif_ctx_seed(fastrandom_nif_ctx_t *ctx, uint64_t *seed);

inline void
fastrandom_nif_ctx_jump(fastrandom_nif_ctx_t *ctx)
{
    (void)enif_rwlock_rwlock(fastrandom_nif_global_rwlock);

    (void)memcpy(&ctx->xoroshiro116p, &fastrandom_nif_global_ctx.xoroshiro116p, sizeof(xoroshiro116p_state_t));
    (void)xoroshiro116p_jump(&fastrandom_nif_global_ctx.xoroshiro116p);

    (void)memcpy(&ctx->xoshiro256p, &fastrandom_nif_global_ctx.xoshiro256p, sizeof(xoshiro256p_state_t));
    (void)xoshiro256p_jump(&fastrandom_nif_global_ctx.xoshiro256p);

    (void)memcpy(&ctx->xoshiro256px8, &fastrandom_nif_global_ctx.xoshiro256px8, sizeof(xoshiro256px8_state_t));
    (void)xoshiro256px8_jump(&fastrandom_nif_global_ctx.xoshiro256px8);

    (void)memcpy(&ctx->xoshiro256pp, &fastrandom_nif_global_ctx.xoshiro256pp, sizeof(xoshiro256pp_state_t));
    (void)xoshiro256pp_jump(&fastrandom_nif_global_ctx.xoshiro256pp);

    (void)memcpy(&ctx->xoshiro256ppx8, &fastrandom_nif_global_ctx.xoshiro256ppx8, sizeof(xoshiro256ppx8_state_t));
    (void)xoshiro256ppx8_jump(&fastrandom_nif_global_ctx.xoshiro256ppx8);

    (void)enif_rwlock_rwunlock(fastrandom_nif_global_rwlock);
}

inline void
fastrandom_nif_ctx_seed(fastrandom_nif_ctx_t *ctx, uint64_t *seed)
{
    for (int i = 0; i < 4; i++) {
        if (i < 2)
            ctx->xoroshiro116p.s[i] = splitmix64_next(seed);
        ctx->xoshiro256p.s[i] = splitmix64_next(seed);
        for (int j = 0; j < XOSHIRO256PX8_UNROLL; j++)
            ctx->xoshiro256px8.s[i][j] = splitmix64_next(seed);
        ctx->xoshiro256pp.s[i] = splitmix64_next(seed);
        for (int j = 0; j < XOSHIRO256PPX8_UNROLL; j++)
            ctx->xoshiro256px8.s[i][j] = splitmix64_next(seed);
    }
    ctx->xoshiro256px8.offset = XOSHIRO256PX8_UNROLL;
    ctx->xoshiro256ppx8.offset = XOSHIRO256PPX8_UNROLL;
}

/* Resource Type Functions (Declarations) */

/* NIF Function Declarations */

static ERL_NIF_TERM fastrandom_nif_seed_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM fastrandom_nif_xoroshiro116p_next_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM fastrandom_nif_xoroshiro116p_next_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM fastrandom_nif_xoshiro256p_next_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM fastrandom_nif_xoshiro256p_next_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM fastrandom_nif_xoshiro256px8_next_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM fastrandom_nif_xoshiro256px8_next_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM fastrandom_nif_xoshiro256pp_next_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM fastrandom_nif_xoshiro256pp_next_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM fastrandom_nif_xoshiro256ppx8_next_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM fastrandom_nif_xoshiro256ppx8_next_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* NIF Function Definitions */

static ERL_NIF_TERM
fastrandom_nif_seed_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t seed = 0;

    if (argc != 1) {
        return EXCP_NOTSUP(env, "argc must be 1");
    }
    if (!enif_get_uint64(env, argv[0], (ErlNifUInt64 *)&seed)) {
        return EXCP_BADARG(env, "Bad argument: 'Seed'");
    }

    (void)enif_rwlock_rwlock(fastrandom_nif_global_rwlock);
    fastrandom_nif_global_seed = seed;
    (void)fastrandom_nif_ctx_seed(&fastrandom_nif_global_ctx, &fastrandom_nif_global_seed);
    (void)enif_rwlock_rwunlock(fastrandom_nif_global_rwlock);

    return ATOM(ok);
}

static ERL_NIF_TERM
fastrandom_nif_xoroshiro116p_next_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    fastrandom_nif_ctx_t *ctx = NULL;
    ErlNifUInt64 output;

    if (argc != 0) {
        return EXCP_NOTSUP(env, "argc must be 0");
    }

    ctx = (void *)enif_tsd_get(fastrandom_nif_tsd_key);
    if (ctx == NULL) {
        ctx = (void *)enif_alloc(sizeof(fastrandom_nif_ctx_t));
        if (ctx == NULL) {
            return EXCP_ERROR(env, "Can't allocate fastrandom_nif_ctx_t");
        }
        (void)fastrandom_nif_ctx_jump(ctx);
        (void)enif_tsd_set(fastrandom_nif_tsd_key, (void *)ctx);
    }

    output = (ErlNifUInt64)xoroshiro116p_next(&ctx->xoroshiro116p);
    output %= UINT58MASK;

    return enif_make_uint64(env, output);
}

static ERL_NIF_TERM
fastrandom_nif_xoroshiro116p_next_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    fastrandom_nif_ctx_t *ctx = NULL;
    uint64_t n;
    uint64_t max_minus_n;
    uint64_t v = 0;
    uint64_t i = 0;

    if (argc != 1) {
        return EXCP_NOTSUP(env, "argc must be 1");
    }
    if (!enif_get_uint64(env, argv[0], (ErlNifUInt64 *)&n) || n == 0 || n > UINT58MASK) {
        return EXCP_BADARG(env, "Bad argument: 'Seed'");
    }

    ctx = (void *)enif_tsd_get(fastrandom_nif_tsd_key);
    if (ctx == NULL) {
        ctx = (void *)enif_alloc(sizeof(fastrandom_nif_ctx_t));
        if (ctx == NULL) {
            return EXCP_ERROR(env, "Can't allocate fastrandom_nif_ctx_t");
        }
        (void)fastrandom_nif_ctx_jump(ctx);
        (void)enif_tsd_set(fastrandom_nif_tsd_key, (void *)ctx);
    }

    max_minus_n = UINT58MASK - n + 1;
    while (v == 0) {
        v = xoroshiro116p_next(&ctx->xoroshiro116p);
        if (v < n)
            v += 1;
        else {
            i = v % n;
            if ((v - i) <= max_minus_n)
                v = i + 1;
            else
                v = 0;
        }
    }

    return enif_make_uint64(env, (ErlNifUInt64)v);
}

static ERL_NIF_TERM
fastrandom_nif_xoshiro256p_next_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    fastrandom_nif_ctx_t *ctx = NULL;
    ErlNifUInt64 output;

    if (argc != 0) {
        return EXCP_NOTSUP(env, "argc must be 0");
    }

    ctx = (void *)enif_tsd_get(fastrandom_nif_tsd_key);
    if (ctx == NULL) {
        ctx = (void *)enif_alloc(sizeof(fastrandom_nif_ctx_t));
        if (ctx == NULL) {
            return EXCP_ERROR(env, "Can't allocate fastrandom_nif_ctx_t");
        }
        (void)fastrandom_nif_ctx_jump(ctx);
        (void)enif_tsd_set(fastrandom_nif_tsd_key, (void *)ctx);
    }

    output = (ErlNifUInt64)xoshiro256p_next(&ctx->xoshiro256p);
    output %= UINT58MASK;

    return enif_make_uint64(env, output);
}

static ERL_NIF_TERM
fastrandom_nif_xoshiro256p_next_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    fastrandom_nif_ctx_t *ctx = NULL;
    uint64_t n;
    uint64_t max_minus_n;
    uint64_t v = 0;
    uint64_t i = 0;

    if (argc != 1) {
        return EXCP_NOTSUP(env, "argc must be 1");
    }
    if (!enif_get_uint64(env, argv[0], (ErlNifUInt64 *)&n) || n == 0 || n > UINT58MASK) {
        return EXCP_BADARG(env, "Bad argument: 'Seed'");
    }

    ctx = (void *)enif_tsd_get(fastrandom_nif_tsd_key);
    if (ctx == NULL) {
        ctx = (void *)enif_alloc(sizeof(fastrandom_nif_ctx_t));
        if (ctx == NULL) {
            return EXCP_ERROR(env, "Can't allocate fastrandom_nif_ctx_t");
        }
        (void)fastrandom_nif_ctx_jump(ctx);
        (void)enif_tsd_set(fastrandom_nif_tsd_key, (void *)ctx);
    }

    max_minus_n = UINT64MASK - n + 1;
    while (v == 0) {
        v = xoshiro256p_next(&ctx->xoshiro256p);
        if (v < n)
            v += 1;
        else {
            i = v % n;
            if ((v - i) <= max_minus_n)
                v = i + 1;
            else
                v = 0;
        }
    }

    return enif_make_uint64(env, (ErlNifUInt64)v);
}

static ERL_NIF_TERM
fastrandom_nif_xoshiro256px8_next_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    fastrandom_nif_ctx_t *ctx = NULL;
    ErlNifUInt64 output;

    if (argc != 0) {
        return EXCP_NOTSUP(env, "argc must be 0");
    }

    ctx = (void *)enif_tsd_get(fastrandom_nif_tsd_key);
    if (ctx == NULL) {
        ctx = (void *)enif_alloc(sizeof(fastrandom_nif_ctx_t));
        if (ctx == NULL) {
            return EXCP_ERROR(env, "Can't allocate fastrandom_nif_ctx_t");
        }
        (void)fastrandom_nif_ctx_jump(ctx);
        (void)enif_tsd_set(fastrandom_nif_tsd_key, (void *)ctx);
    }

    output = (ErlNifUInt64)xoshiro256px8_next(&ctx->xoshiro256px8);
    output %= UINT58MASK;

    return enif_make_uint64(env, output);
}

static ERL_NIF_TERM
fastrandom_nif_xoshiro256px8_next_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    fastrandom_nif_ctx_t *ctx = NULL;
    uint64_t n;
    uint64_t max_minus_n;
    uint64_t v = 0;
    uint64_t i = 0;

    if (argc != 1) {
        return EXCP_NOTSUP(env, "argc must be 1");
    }
    if (!enif_get_uint64(env, argv[0], (ErlNifUInt64 *)&n) || n == 0 || n > UINT58MASK) {
        return EXCP_BADARG(env, "Bad argument: 'Seed'");
    }

    ctx = (void *)enif_tsd_get(fastrandom_nif_tsd_key);
    if (ctx == NULL) {
        ctx = (void *)enif_alloc(sizeof(fastrandom_nif_ctx_t));
        if (ctx == NULL) {
            return EXCP_ERROR(env, "Can't allocate fastrandom_nif_ctx_t");
        }
        (void)fastrandom_nif_ctx_jump(ctx);
        (void)enif_tsd_set(fastrandom_nif_tsd_key, (void *)ctx);
    }

    max_minus_n = UINT64MASK - n + 1;
    while (v == 0) {
        v = xoshiro256px8_next(&ctx->xoshiro256px8);
        if (v < n)
            v += 1;
        else {
            i = v % n;
            if ((v - i) <= max_minus_n)
                v = i + 1;
            else
                v = 0;
        }
    }

    return enif_make_uint64(env, (ErlNifUInt64)v);
}

static ERL_NIF_TERM
fastrandom_nif_xoshiro256pp_next_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    fastrandom_nif_ctx_t *ctx = NULL;
    ErlNifUInt64 output;

    if (argc != 0) {
        return EXCP_NOTSUP(env, "argc must be 0");
    }

    ctx = (void *)enif_tsd_get(fastrandom_nif_tsd_key);
    if (ctx == NULL) {
        ctx = (void *)enif_alloc(sizeof(fastrandom_nif_ctx_t));
        if (ctx == NULL) {
            return EXCP_ERROR(env, "Can't allocate fastrandom_nif_ctx_t");
        }
        (void)fastrandom_nif_ctx_jump(ctx);
        (void)enif_tsd_set(fastrandom_nif_tsd_key, (void *)ctx);
    }

    output = (ErlNifUInt64)xoshiro256pp_next(&ctx->xoshiro256pp);
    output %= UINT58MASK;

    return enif_make_uint64(env, output);
}

static ERL_NIF_TERM
fastrandom_nif_xoshiro256pp_next_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    fastrandom_nif_ctx_t *ctx = NULL;
    uint64_t n;
    uint64_t max_minus_n;
    uint64_t v = 0;
    uint64_t i = 0;

    if (argc != 1) {
        return EXCP_NOTSUP(env, "argc must be 1");
    }
    if (!enif_get_uint64(env, argv[0], (ErlNifUInt64 *)&n) || n == 0 || n > UINT58MASK) {
        return EXCP_BADARG(env, "Bad argument: 'Seed'");
    }

    ctx = (void *)enif_tsd_get(fastrandom_nif_tsd_key);
    if (ctx == NULL) {
        ctx = (void *)enif_alloc(sizeof(fastrandom_nif_ctx_t));
        if (ctx == NULL) {
            return EXCP_ERROR(env, "Can't allocate fastrandom_nif_ctx_t");
        }
        (void)fastrandom_nif_ctx_jump(ctx);
        (void)enif_tsd_set(fastrandom_nif_tsd_key, (void *)ctx);
    }

    max_minus_n = UINT64MASK - n + 1;
    while (v == 0) {
        v = xoshiro256pp_next(&ctx->xoshiro256pp);
        if (v < n)
            v += 1;
        else {
            i = v % n;
            if ((v - i) <= max_minus_n)
                v = i + 1;
            else
                v = 0;
        }
    }

    return enif_make_uint64(env, (ErlNifUInt64)v);
}

static ERL_NIF_TERM
fastrandom_nif_xoshiro256ppx8_next_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    fastrandom_nif_ctx_t *ctx = NULL;
    ErlNifUInt64 output;

    if (argc != 0) {
        return EXCP_NOTSUP(env, "argc must be 0");
    }

    ctx = (void *)enif_tsd_get(fastrandom_nif_tsd_key);
    if (ctx == NULL) {
        ctx = (void *)enif_alloc(sizeof(fastrandom_nif_ctx_t));
        if (ctx == NULL) {
            return EXCP_ERROR(env, "Can't allocate fastrandom_nif_ctx_t");
        }
        (void)fastrandom_nif_ctx_jump(ctx);
        (void)enif_tsd_set(fastrandom_nif_tsd_key, (void *)ctx);
    }

    output = (ErlNifUInt64)xoshiro256ppx8_next(&ctx->xoshiro256ppx8);
    output %= UINT58MASK;

    return enif_make_uint64(env, output);
}

static ERL_NIF_TERM
fastrandom_nif_xoshiro256ppx8_next_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    fastrandom_nif_ctx_t *ctx = NULL;
    uint64_t n;
    uint64_t max_minus_n;
    uint64_t v = 0;
    uint64_t i = 0;

    if (argc != 1) {
        return EXCP_NOTSUP(env, "argc must be 1");
    }
    if (!enif_get_uint64(env, argv[0], (ErlNifUInt64 *)&n) || n == 0 || n > UINT58MASK) {
        return EXCP_BADARG(env, "Bad argument: 'Seed'");
    }

    ctx = (void *)enif_tsd_get(fastrandom_nif_tsd_key);
    if (ctx == NULL) {
        ctx = (void *)enif_alloc(sizeof(fastrandom_nif_ctx_t));
        if (ctx == NULL) {
            return EXCP_ERROR(env, "Can't allocate fastrandom_nif_ctx_t");
        }
        (void)fastrandom_nif_ctx_jump(ctx);
        (void)enif_tsd_set(fastrandom_nif_tsd_key, (void *)ctx);
    }

    max_minus_n = UINT64MASK - n + 1;
    while (v == 0) {
        v = xoshiro256ppx8_next(&ctx->xoshiro256ppx8);
        if (v < n)
            v += 1;
        else {
            i = v % n;
            if ((v - i) <= max_minus_n)
                v = i + 1;
            else
                v = 0;
        }
    }

    return enif_make_uint64(env, (ErlNifUInt64)v);
}

/* NIF Callbacks */

static ErlNifFunc fastrandom_nif_funcs[] = {
    // clang-format off
    {"seed", 1, fastrandom_nif_seed_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"xoroshiro116p_next", 0, fastrandom_nif_xoroshiro116p_next_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"xoroshiro116p_next", 1, fastrandom_nif_xoroshiro116p_next_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"xoshiro256p_next", 0, fastrandom_nif_xoshiro256p_next_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"xoshiro256p_next", 1, fastrandom_nif_xoshiro256p_next_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"xoshiro256px8_next", 0, fastrandom_nif_xoshiro256px8_next_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"xoshiro256px8_next", 1, fastrandom_nif_xoshiro256px8_next_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"xoshiro256pp_next", 0, fastrandom_nif_xoshiro256pp_next_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"xoshiro256pp_next", 1, fastrandom_nif_xoshiro256pp_next_1, ERL_NIF_NORMAL_JOB_BOUND},
    {"xoshiro256ppx8_next", 0, fastrandom_nif_xoshiro256ppx8_next_0, ERL_NIF_NORMAL_JOB_BOUND},
    {"xoshiro256ppx8_next", 1, fastrandom_nif_xoshiro256ppx8_next_1, ERL_NIF_NORMAL_JOB_BOUND},
    // clang-format on
};

static void
fastrandom_nif_make_atoms(ErlNifEnv *env)
{
#define MAKE_ATOM(Id, Value)                                                                                                       \
    {                                                                                                                              \
        fastrandom_nif_atom_table->ATOM_##Id = enif_make_atom(env, Value);                                                         \
    }
    MAKE_ATOM(badarg, "badarg");
    MAKE_ATOM(error, "error");
    MAKE_ATOM(false, "false");
    MAKE_ATOM(nil, "nil");
    MAKE_ATOM(no_context, "no_context");
    MAKE_ATOM(notsup, "notsup");
    MAKE_ATOM(ok, "ok");
    MAKE_ATOM(true, "true");
    MAKE_ATOM(undefined, "undefined");
#undef MAKE_ATOM
}

static int
fastrandom_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    int retval = 0;
    (void)priv_data;
    (void)load_info;
    retval = enif_tsd_key_create("fastrandom_nif_tsd_key", &fastrandom_nif_tsd_key);
    if (retval != 0) {
        return retval;
    }
    fastrandom_nif_global_rwlock = enif_rwlock_create("fastrandom_nif_global_rwlock");
    if (fastrandom_nif_global_rwlock == NULL) {
        retval = -1;
        (void)enif_tsd_key_destroy(fastrandom_nif_tsd_key);
        return retval;
    }
    (void)enif_rwlock_rwlock(fastrandom_nif_global_rwlock);
    fastrandom_nif_global_seed = 0;
    (void)fastrandom_nif_ctx_seed(&fastrandom_nif_global_ctx, &fastrandom_nif_global_seed);
    (void)enif_rwlock_rwunlock(fastrandom_nif_global_rwlock);
    /* Initialize common atoms */
    (void)fastrandom_nif_make_atoms(env);
    return retval;
}

static int
fastrandom_nif_upgrade(ErlNifEnv *env, void **new_priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    int retval = 0;
    (void)old_priv_data;
    (void)load_info;
    /* Initialize common atoms */
    (void)fastrandom_nif_make_atoms(env);
    return retval;
}

static void
fastrandom_nif_unload(ErlNifEnv *env, void *priv_data)
{
    (void)env;
    (void)priv_data;
    // (void)enif_tsd_key_destroy()
    return;
}

ERL_NIF_INIT(fastrandom_nif, fastrandom_nif_funcs, fastrandom_nif_load, NULL, fastrandom_nif_upgrade, fastrandom_nif_unload);
