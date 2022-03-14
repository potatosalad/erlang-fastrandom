// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

/*  Written in 2019 by David Blackman and Sebastiano Vigna (vigna@acm.org)

To the extent possible under law, the author has dedicated all copyright
and related and neighboring rights to this software to the public domain
worldwide. This software is distributed without any warranty.

See <http://creativecommons.org/publicdomain/zero/1.0/>. */

#ifndef XOSHIRO256PP_H
#define XOSHIRO256PP_H

#include <inttypes.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct xoshiro256pp_state_s xoshiro256pp_state_t;

struct xoshiro256pp_state_s {
    uint64_t s[4];
};

static uint64_t xoshiro256pp_rotl(const uint64_t x, int k);
static uint64_t xoshiro256pp_next(xoshiro256pp_state_t *state);
static void xoshiro256pp_jump(xoshiro256pp_state_t *state);
static void xoshiro256pp_long_jump(xoshiro256pp_state_t *state);

/* This is xoshiro256++ 1.0, one of our all-purpose, rock-solid generators.
   It has excellent (sub-ns) speed, a state (256 bits) that is large
   enough for any parallel application, and it passes all tests we are
   aware of.

   For generating just floating-point numbers, xoshiro256+ is even faster.

   The state must be seeded so that it is not everywhere zero. If you have
   a 64-bit seed, we suggest to seed a splitmix64 generator and use its
   output to fill s. */

inline uint64_t
xoshiro256pp_rotl(const uint64_t x, int k)
{
    return (x << k) | (x >> (64 - k));
}

inline uint64_t
xoshiro256pp_next(xoshiro256pp_state_t *state)
{
    const uint64_t result = xoshiro256pp_rotl(state->s[0] + state->s[3], 23) + state->s[0];

    const uint64_t t = state->s[1] << 17;

    state->s[2] ^= state->s[0];
    state->s[3] ^= state->s[1];
    state->s[1] ^= state->s[2];
    state->s[0] ^= state->s[3];

    state->s[2] ^= t;

    state->s[3] = xoshiro256pp_rotl(state->s[3], 45);

    return result;
}

/* This is the jump function for the generator. It is equivalent
   to 2^128 calls to next(); it can be used to generate 2^128
   non-overlapping subsequences for parallel computations. */

inline void
xoshiro256pp_jump(xoshiro256pp_state_t *state)
{
    static const uint64_t JUMP[] = {0x180ec6d33cfd0aba, 0xd5a61266f0c9392c, 0xa9582618e03fc9aa, 0x39abdc4529b1661c};

    uint64_t s0 = 0;
    uint64_t s1 = 0;
    uint64_t s2 = 0;
    uint64_t s3 = 0;
    for (int i = 0; i < sizeof JUMP / sizeof *JUMP; i++)
        for (int b = 0; b < 64; b++) {
            if (JUMP[i] & UINT64_C(1) << b) {
                s0 ^= state->s[0];
                s1 ^= state->s[1];
                s2 ^= state->s[2];
                s3 ^= state->s[3];
            }
            (void)xoshiro256pp_next(state);
        }

    state->s[0] = s0;
    state->s[1] = s1;
    state->s[2] = s2;
    state->s[3] = s3;
}

/* This is the long-jump function for the generator. It is equivalent to
   2^192 calls to next(); it can be used to generate 2^64 starting points,
   from each of which jump() will generate 2^64 non-overlapping
   subsequences for parallel distributed computations. */

inline void
xoshiro256pp_long_jump(xoshiro256pp_state_t *state)
{
    static const uint64_t LONG_JUMP[] = {0x76e15d3efefdcbbf, 0xc5004e441c522fb3, 0x77710069854ee241, 0x39109bb02acbe635};

    uint64_t s0 = 0;
    uint64_t s1 = 0;
    uint64_t s2 = 0;
    uint64_t s3 = 0;
    for (int i = 0; i < sizeof LONG_JUMP / sizeof *LONG_JUMP; i++)
        for (int b = 0; b < 64; b++) {
            if (LONG_JUMP[i] & UINT64_C(1) << b) {
                s0 ^= state->s[0];
                s1 ^= state->s[1];
                s2 ^= state->s[2];
                s3 ^= state->s[3];
            }
            (void)xoshiro256pp_next(state);
        }

    state->s[0] = s0;
    state->s[1] = s1;
    state->s[2] = s2;
    state->s[3] = s3;
}

#ifdef __cplusplus
}
#endif

#endif
