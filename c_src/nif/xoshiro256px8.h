// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

/*  Written in 2019 by Sebastiano Vigna (vigna@acm.org)

To the extent possible under law, the author has dedicated all copyright
and related and neighboring rights to this software to the public domain
worldwide. This software is distributed without any warranty.

See <http://creativecommons.org/publicdomain/zero/1.0/>. */

#ifndef XOSHIRO256PX8_H
#define XOSHIRO256PX8_H

#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// You can play with this value on your architecture
#define XOSHIRO256PX8_UNROLL (8)

typedef struct xoshiro256px8_state_s xoshiro256px8_state_t;

struct xoshiro256px8_state_s {
    uint64_t s[4][XOSHIRO256PX8_UNROLL];
    uint64_t array[XOSHIRO256PX8_UNROLL];
    size_t offset;
};

static uint64_t xoshiro256px8_rotl(const uint64_t x, int k);
static uint64_t xoshiro256px8_next(xoshiro256px8_state_t *state);
static void xoshiro256px8_next_unroll(xoshiro256px8_state_t *state);
static void xoshiro256px8_jump(xoshiro256px8_state_t *state);

inline uint64_t
xoshiro256px8_rotl(const uint64_t x, int k)
{
    return (x << k) | (x >> (64 - k));
}

inline uint64_t
xoshiro256px8_next(xoshiro256px8_state_t *state)
{
    if (state->offset >= XOSHIRO256PX8_UNROLL)
        (void)xoshiro256px8_next_unroll(state);
    return state->array[state->offset++];
}

inline void
xoshiro256px8_next_unroll(xoshiro256px8_state_t *state)
{
    uint64_t t[XOSHIRO256PX8_UNROLL];

    for (int i = 0; i < XOSHIRO256PX8_UNROLL; i++)
        state->array[i] = state->s[0][i] + state->s[3][i];

    for (int i = 0; i < XOSHIRO256PX8_UNROLL; i++)
        t[i] = state->s[1][i] << 17;

    for (int i = 0; i < XOSHIRO256PX8_UNROLL; i++)
        state->s[2][i] ^= state->s[0][i];
    for (int i = 0; i < XOSHIRO256PX8_UNROLL; i++)
        state->s[3][i] ^= state->s[1][i];
    for (int i = 0; i < XOSHIRO256PX8_UNROLL; i++)
        state->s[1][i] ^= state->s[2][i];
    for (int i = 0; i < XOSHIRO256PX8_UNROLL; i++)
        state->s[0][i] ^= state->s[3][i];

    for (int i = 0; i < XOSHIRO256PX8_UNROLL; i++)
        state->s[2][i] ^= t[i];

    for (int i = 0; i < XOSHIRO256PX8_UNROLL; i++)
        state->s[3][i] = xoshiro256px8_rotl(state->s[3][i], 45);

    state->offset = 0;
}

inline void
xoshiro256px8_jump(xoshiro256px8_state_t *state)
{
    static const uint64_t JUMP[] = {0x180ec6d33cfd0aba, 0xd5a61266f0c9392c, 0xa9582618e03fc9aa, 0x39abdc4529b1661c};

    uint64_t s0[XOSHIRO256PX8_UNROLL];
    uint64_t s1[XOSHIRO256PX8_UNROLL];
    uint64_t s2[XOSHIRO256PX8_UNROLL];
    uint64_t s3[XOSHIRO256PX8_UNROLL];
    for (int j = 0; j < XOSHIRO256PX8_UNROLL; j++) {
        s0[j] = 0;
        s1[j] = 0;
        s2[j] = 0;
        s3[j] = 0;
    }
    for (int i = 0; i < sizeof(JUMP) / sizeof(*JUMP); i++)
        for (int b = 0; b < 64; b++) {
            if (JUMP[i] & UINT64_C(1) << b) {
                for (int j = 0; j < XOSHIRO256PX8_UNROLL; j++) {
                    s0[j] ^= state->s[0][j];
                    s1[j] ^= state->s[1][j];
                    s2[j] ^= state->s[2][j];
                    s3[j] ^= state->s[3][j];
                }
            }
            (void)xoshiro256px8_next_unroll(state);
        }
    for (int j = 0; j < XOSHIRO256PX8_UNROLL; j++) {
        state->s[0][j] = s0[j];
        state->s[1][j] = s1[j];
        state->s[2][j] = s2[j];
        state->s[3][j] = s3[j];
    }
}

#ifdef __cplusplus
}
#endif

#endif
