// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c++ et

#ifndef XOROSHIRO116P_H
#define XOROSHIRO116P_H

#include <inttypes.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define UINT58MASK (uint64_t)(0x03ffffffffffffff)

typedef struct xoroshiro116p_state_s xoroshiro116p_state_t;

struct xoroshiro116p_state_s {
    uint64_t s[2];
};

static uint64_t xoroshiro116plus_rotl58(const uint64_t x, int k);
static uint64_t xoroshiro116p_next(xoroshiro116p_state_t *state);
static void xoroshiro116p_jump(xoroshiro116p_state_t *state);

inline uint64_t
xoroshiro116plus_rotl58(const uint64_t x, int k)
{
    return ((x << k) & UINT58MASK) | (x >> (58 - k));
}

inline uint64_t
xoroshiro116p_next(xoroshiro116p_state_t *state)
{
    const uint64_t s0 = state->s[0];
    uint64_t s1 = state->s[1];
    const uint64_t result = (s0 + s1) & UINT58MASK;

    s1 ^= s0;
    state->s[0] = xoroshiro116plus_rotl58(s0, 24) ^ s1 ^ ((s1 << 2) & UINT58MASK); // a, b
    state->s[1] = xoroshiro116plus_rotl58(s1, 35);                                 // c

    return result;
}

inline void
xoroshiro116p_jump(xoroshiro116p_state_t *state)
{
    static const uint64_t JUMP[] = {0x4a11293241fcb12a, 0x0009863200f83fcd};

    uint64_t s0 = 0;
    uint64_t s1 = 0;
    int i;
    int b;

    for (i = 0; i < sizeof(JUMP) / sizeof(*JUMP); i++) {
        for (b = 0; b < 64; b++) {
            if (JUMP[i] & (1 << b)) {
                s0 ^= state->s[0];
                s1 ^= state->s[1];
            }
            (void)xoroshiro116p_next(state);
        }
    }
    state->s[0] = s0;
    state->s[1] = s1;
}

#ifdef __cplusplus
}
#endif

#endif
