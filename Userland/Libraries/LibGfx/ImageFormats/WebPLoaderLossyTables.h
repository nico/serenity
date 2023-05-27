/*
 * Copyright (c) 2023, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

namespace Gfx {

using Prob = u8;
using TreeIndex = i8;

// https://datatracker.ietf.org/doc/html/rfc6386#section-10 "Segment-Based Feature Adjustments"
const TreeIndex METABLOCK_SEGMENT_TREE[2 * (4 - 1)] = {
    2, 4,   /* root: "0", "1" subtrees */
    -0, -1, /* "00" = 0th value, "01" = 1st value */
    -2, -3  /* "10" = 2nd value, "11" = 3rd value */
};

// https://datatracker.ietf.org/doc/html/rfc6386#section-8.2 "Tree Coding Example"
// Repeated in https://datatracker.ietf.org/doc/html/rfc6386#section-11.2 "Luma Modes"
enum IntraMetablockMode {
    DC_PRED,               /* predict DC using row above and column to the left */
    V_PRED,                /* predict rows using row above */
    H_PRED,                /* predict columns using column to the left */
    TM_PRED,               /* propagate second differences a la "True Motion" */
    B_PRED,                /* each Y subblock is independently predicted */
    num_uv_modes = B_PRED, /* first four modes apply to chroma */
    num_ymodes             /* all modes apply to luma */
};

// https://datatracker.ietf.org/doc/html/rfc6386#section-19.3 says "intra_y_mode selects the luminance intra-prediction mode (Section 16.1)",
// but for keyframes the correct reference is actually https://datatracker.ietf.org/doc/html/rfc6386#section-11.2 "Luma Modes".
// That is, we want "kf_ymode_tree", not "ymode_tree", and "kf_ymode_prob", not "ymode_prob".
// See "decode_kf_mb_mode" in the reference decoder in the spec.
static TreeIndex constexpr KEYFRAME_YMODE_TREE[2 * (num_ymodes - 1)] = {
    -B_PRED, 2,        /* root: B_PRED = "0", "1" subtree */
    4, 6,              /* "1" subtree has 2 descendant subtrees */
    -DC_PRED, -V_PRED, /* "10" subtree: DC_PRED = "100", V_PRED = "101" */
    -H_PRED, -TM_PRED  /* "11" subtree: H_PRED = "110", TM_PRED = "111" */
};
static Prob constexpr KEYFRAME_YMODE_PROBABILITIES[num_ymodes - 1] = { 145, 156, 163, 128 };

// https://datatracker.ietf.org/doc/html/rfc6386#section-11.2 "Luma Modes"
enum IntraBlockMode {
    B_DC_PRED, /* predict DC using row above and column
                  to the left */
    B_TM_PRED, /* propagate second differences a la
                  "True Motion" */

    B_VE_PRED, /* predict rows using row above */
    B_HE_PRED, /* predict columns using column to the left */

    B_LD_PRED, /* southwest (left and down) 45 degree diagonal
                  prediction */
    B_RD_PRED, /* southeast (right and down) "" */

    B_VR_PRED, /* SSE (vertical right) diagonal prediction */

    B_VL_PRED, /* SSW (vertical left) "" */

    B_HD_PRED, /* ESE (horizontal down) "" */
    B_HU_PRED, /* ENE (horizontal up) "" */

    num_intra_bmodes
};

// clang-format off
static TreeIndex constexpr BLOCK_MODE_TREE[2 * (num_intra_bmodes - 1)] = {
 -B_DC_PRED, 2,                   /* B_DC_PRED = "0" */
  -B_TM_PRED, 4,                  /* B_TM_PRED = "10" */
   -B_VE_PRED, 6,                 /* B_VE_PRED = "110" */
    8, 12,
     -B_HE_PRED, 10,              /* B_HE_PRED = "11100" */
      -B_RD_PRED, -B_VR_PRED,     /* B_RD_PRED = "111010",
                                     B_VR_PRED = "111011" */
     -B_LD_PRED, 14,              /* B_LD_PRED = "111110" */
       -B_VL_PRED, 16,            /* B_VL_PRED = "1111110" */
         -B_HD_PRED, -B_HU_PRED   /* HD = "11111110",
                                     HU = "11111111" */
};
// clang-format on

// https://datatracker.ietf.org/doc/html/rfc6386#section-11.4 "Chroma Modes"
// clang-format off
static TreeIndex constexpr UV_MODE_TREE[2 * (num_uv_modes - 1)] = {
    -DC_PRED, 2,              /* root: DC_PRED = "0", "1" subtree */
        -V_PRED, 4,           /* "1" subtree:  V_PRED = "10", "11" subtree */
            -H_PRED, -TM_PRED /* "11" subtree: H_PRED = "110", TM_PRED = "111" */
};
// clang-format on
static Prob constexpr KEYFRAME_UV_MODE_PROBABILITIES[num_uv_modes - 1] = { 142, 114, 183 };

// https://datatracker.ietf.org/doc/html/rfc6386#section-11.5 "Subblock Mode Probability Table"
// clang-format off
const Prob KEYFRAME_BLOCK_MODE_PROBABILITIES[num_intra_bmodes][num_intra_bmodes][num_intra_bmodes - 1] = {
    {
        { 231, 120,  48,  89, 115, 113, 120, 152, 112 },
        { 152, 179,  64, 126, 170, 118,  46,  70,  95 },
        { 175,  69, 143,  80,  85,  82,  72, 155, 103 },
        {  56,  58,  10, 171, 218, 189,  17,  13, 152 },
        { 144,  71,  10,  38, 171, 213, 144,  34,  26 },
        { 114,  26,  17, 163,  44, 195,  21,  10, 173 },
        { 121,  24,  80, 195,  26,  62,  44,  64,  85 },
        { 170,  46,  55,  19, 136, 160,  33, 206,  71 },
        {  63,  20,   8, 114, 114, 208,  12,   9, 226 },
        {  81,  40,  11,  96, 182,  84,  29,  16,  36 }
    },
    {
        { 134, 183,  89, 137,  98, 101, 106, 165, 148 },
        {  72, 187, 100, 130, 157, 111,  32,  75,  80 },
        {  66, 102, 167,  99,  74,  62,  40, 234, 128 },
        {  41,  53,   9, 178, 241, 141,  26,   8, 107 },
        { 104,  79,  12,  27, 217, 255,  87,  17,   7 },
        {  74,  43,  26, 146,  73, 166,  49,  23, 157 },
        {  65,  38, 105, 160,  51,  52,  31, 115, 128 },
        {  87,  68,  71,  44, 114,  51,  15, 186,  23 },
        {  47,  41,  14, 110, 182, 183,  21,  17, 194 },
        {  66,  45,  25, 102, 197, 189,  23,  18,  22 }
    },
    {
        {  88,  88, 147, 150,  42,  46,  45, 196, 205 },
        {  43,  97, 183, 117,  85,  38,  35, 179,  61 },
        {  39,  53, 200,  87,  26,  21,  43, 232, 171 },
        {  56,  34,  51, 104, 114, 102,  29,  93,  77 },
        { 107,  54,  32,  26,  51,   1,  81,  43,  31 },
        {  39,  28,  85, 171,  58, 165,  90,  98,  64 },
        {  34,  22, 116, 206,  23,  34,  43, 166,  73 },
        {  68,  25, 106,  22,  64, 171,  36, 225, 114 },
        {  34,  19,  21, 102, 132, 188,  16,  76, 124 },
        {  62,  18,  78,  95,  85,  57,  50,  48,  51 }
    },
    {
        { 193, 101,  35, 159, 215, 111,  89,  46, 111 },
        {  60, 148,  31, 172, 219, 228,  21,  18, 111 },
        { 112, 113,  77,  85, 179, 255,  38, 120, 114 },
        {  40,  42,   1, 196, 245, 209,  10,  25, 109 },
        { 100,  80,   8,  43, 154,   1,  51,  26,  71 },
        {  88,  43,  29, 140, 166, 213,  37,  43, 154 },
        {  61,  63,  30, 155,  67,  45,  68,   1, 209 },
        { 142,  78,  78,  16, 255, 128,  34, 197, 171 },
        {  41,  40,   5, 102, 211, 183,   4,   1, 221 },
        {  51,  50,  17, 168, 209, 192,  23,  25,  82 }
    },
    {
        { 125,  98,  42,  88, 104,  85, 117, 175,  82 },
        {  95,  84,  53,  89, 128, 100, 113, 101,  45 },
        {  75,  79, 123,  47,  51, 128,  81, 171,   1 },
        {  57,  17,   5,  71, 102,  57,  53,  41,  49 },
        { 115,  21,   2,  10, 102, 255, 166,  23,   6 },
        {  38,  33,  13, 121,  57,  73,  26,   1,  85 },
        {  41,  10,  67, 138,  77, 110,  90,  47, 114 },
        { 101,  29,  16,  10,  85, 128, 101, 196,  26 },
        {  57,  18,  10, 102, 102, 213,  34,  20,  43 },
        { 117,  20,  15,  36, 163, 128,  68,   1,  26 }
    },
    {
        { 138,  31,  36, 171,  27, 166,  38,  44, 229 },
        {  67,  87,  58, 169,  82, 115,  26,  59, 179 },
        {  63,  59,  90, 180,  59, 166,  93,  73, 154 },
        {  40,  40,  21, 116, 143, 209,  34,  39, 175 },
        {  57,  46,  22,  24, 128,   1,  54,  17,  37 },
        {  47,  15,  16, 183,  34, 223,  49,  45, 183 },
        {  46,  17,  33, 183,   6,  98,  15,  32, 183 },
        {  65,  32,  73, 115,  28, 128,  23, 128, 205 },
        {  40,   3,   9, 115,  51, 192,  18,   6, 223 },
        {  87,  37,   9, 115,  59,  77,  64,  21,  47 }
    },
    {
        { 104,  55,  44, 218,   9,  54,  53, 130, 226 },
        {  64,  90,  70, 205,  40,  41,  23,  26,  57 },
        {  54,  57, 112, 184,   5,  41,  38, 166, 213 },
        {  30,  34,  26, 133, 152, 116,  10,  32, 134 },
        {  75,  32,  12,  51, 192, 255, 160,  43,  51 },
        {  39,  19,  53, 221,  26, 114,  32,  73, 255 },
        {  31,   9,  65, 234,   2,  15,   1, 118,  73 },
        {  88,  31,  35,  67, 102,  85,  55, 186,  85 },
        {  56,  21,  23, 111,  59, 205,  45,  37, 192 },
        {  55,  38,  70, 124,  73, 102,   1,  34,  98 }
    },
    {
        { 102,  61,  71,  37,  34,  53,  31, 243, 192 },
        {  69,  60,  71,  38,  73, 119,  28, 222,  37 },
        {  68,  45, 128,  34,   1,  47,  11, 245, 171 },
        {  62,  17,  19,  70, 146,  85,  55,  62,  70 },
        {  75,  15,   9,   9,  64, 255, 184, 119,  16 },
        {  37,  43,  37, 154, 100, 163,  85, 160,   1 },
        {  63,   9,  92, 136,  28,  64,  32, 201,  85 },
        {  86,   6,  28,   5,  64, 255,  25, 248,   1 },
        {  56,   8,  17, 132, 137, 255,  55, 116, 128 },
        {  58,  15,  20,  82, 135,  57,  26, 121,  40 }
    },
    {
        { 164,  50,  31, 137, 154, 133,  25,  35, 218 },
        {  51, 103,  44, 131, 131, 123,  31,   6, 158 },
        {  86,  40,  64, 135, 148, 224,  45, 183, 128 },
        {  22,  26,  17, 131, 240, 154,  14,   1, 209 },
        {  83,  12,  13,  54, 192, 255,  68,  47,  28 },
        {  45,  16,  21,  91,  64, 222,   7,   1, 197 },
        {  56,  21,  39, 155,  60, 138,  23, 102, 213 },
        {  85,  26,  85,  85, 128, 128,  32, 146, 171 },
        {  18,  11,   7,  63, 144, 171,   4,   4, 246 },
        {  35,  27,  10, 146, 174, 171,  12,  26, 128 }
    },
    {
        { 190,  80,  35,  99, 180,  80, 126,  54,  45 },
        {  85, 126,  47,  87, 176,  51,  41,  20,  32 },
        { 101,  75, 128, 139, 118, 146, 116, 128,  85 },
        {  56,  41,  15, 176, 236,  85,  37,   9,  62 },
        { 146,  36,  19,  30, 171, 255,  97,  27,  20 },
        {  71,  30,  17, 119, 118, 255,  17,  18, 138 },
        { 101,  38,  60, 138,  55,  70,  43,  26, 142 },
        { 138,  45,  61,  62, 219,   1,  81, 188,  64 },
        {  32,  41,  20, 117, 151, 142,  20,  21, 163 },
        { 112,  19,  12,  61, 195, 128,  48,   4,  24 }
    }
};
// clang-format on

// https://datatracker.ietf.org/doc/html/rfc6386#section-13.2 "Coding of Individual Coefficient Values"
enum DCTToken {
    DCT_0,         /* value 0 */
    DCT_1,         /* 1 */
    DCT_2,         /* 2 */
    DCT_3,         /* 3 */
    DCT_4,         /* 4 */
    dct_cat1,      /* range 5 - 6  (size 2) */
    dct_cat2,      /* 7 - 10   (4) */
    dct_cat3,      /* 11 - 18  (8) */
    dct_cat4,      /* 19 - 34  (16) */
    dct_cat5,      /* 35 - 66  (32) */
    dct_cat6,      /* 67 - 2048  (1982) */
    dct_eob,       /* end of block */
    num_dct_tokens /* 12 */
};

// clang-format off
const TreeIndex COEFFICIENT_TREE[2 * (num_dct_tokens - 1)] = {
 -dct_eob, 2,               /* eob = "0"   */
  -DCT_0, 4,                /* 0   = "10"  */
   -DCT_1, 6,               /* 1   = "110" */
    8, 12,
     -DCT_2, 10,            /* 2   = "11100" */
      -DCT_3, -DCT_4,       /* 3   = "111010", 4 = "111011" */
     14, 16,
      -dct_cat1, -dct_cat2, /* cat1 =  "111100",
                               cat2 = "111101" */
     18, 20,
      -dct_cat3, -dct_cat4, /* cat3 = "1111100",
                               cat4 = "1111101" */
      -dct_cat5, -dct_cat6  /* cat4 = "1111110",
                               cat4 = "1111111" */
};
// clang-format on

// https://datatracker.ietf.org/doc/html/rfc6386#section-13.4 "Token Probability Updates"
// clang-format off
static Prob constexpr COEFFICIENT_UPDATE_PROBABILITIES[4][8][3][num_dct_tokens - 1] = {
    {
        {
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 176, 246, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 223, 241, 252, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 249, 253, 253, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 244, 252, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 234, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 253, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 246, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 239, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 254, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 248, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 251, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 251, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 254, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 254, 253, 255, 254, 255, 255, 255, 255, 255, 255 },
            { 250, 255, 254, 255, 254, 255, 255, 255, 255, 255, 255 },
            { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        }
    },
    {
        {
            { 217, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 225, 252, 241, 253, 255, 255, 254, 255, 255, 255, 255 },
            { 234, 250, 241, 250, 253, 255, 253, 254, 255, 255, 255 }
        },
        {
            { 255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 223, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 238, 253, 254, 254, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 248, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 249, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 253, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 247, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 252, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 253, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 254, 253, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 250, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        }
    },
    {
        {
            { 186, 251, 250, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 234, 251, 244, 254, 255, 255, 255, 255, 255, 255, 255 },
            { 251, 251, 243, 253, 254, 255, 254, 255, 255, 255, 255 }
        },
        {
            { 255, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 236, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 251, 253, 253, 254, 254, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 254, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 254, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        }
    },
    {
        {
            { 248, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 250, 254, 252, 254, 255, 255, 255, 255, 255, 255, 255 },
            { 248, 254, 249, 253, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 253, 253, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 246, 253, 253, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 252, 254, 251, 254, 254, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 254, 252, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 248, 254, 253, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 253, 255, 254, 254, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 251, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 245, 251, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 253, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 251, 253, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 252, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 252, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 249, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 255, 253, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 250, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        },
        {
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
            { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
        }
    }
};
// clang-format on

// https://datatracker.ietf.org/doc/html/rfc6386#section-13.5 "Default Token Probability Table"
// clang-format off
static Prob constexpr DEFAULT_COEFFICIENT_PROBABILITIES[4][8][3][num_dct_tokens - 1] = {
    {
        {
            { 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128 },
            { 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128 },
            { 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128 },
        },
        {
            { 253, 136, 254, 255, 228, 219, 128, 128, 128, 128, 128 },
            { 189, 129, 242, 255, 227, 213, 255, 219, 128, 128, 128 },
            { 106, 126, 227, 252, 214, 209, 255, 255, 128, 128, 128 },
        },
        {
            { 1, 98, 248, 255, 236, 226, 255, 255, 128, 128, 128 },
            { 181, 133, 238, 254, 221, 234, 255, 154, 128, 128, 128 },
            { 78, 134, 202, 247, 198, 180, 255, 219, 128, 128, 128 },
        },
        {
            { 1, 185, 249, 255, 243, 255, 128, 128, 128, 128, 128 },
            { 184, 150, 247, 255, 236, 224, 128, 128, 128, 128, 128 },
            { 77, 110, 216, 255, 236, 230, 128, 128, 128, 128, 128 },
        },
        {
            { 1, 101, 251, 255, 241, 255, 128, 128, 128, 128, 128 },
            { 170, 139, 241, 252, 236, 209, 255, 255, 128, 128, 128 },
            { 37, 116, 196, 243, 228, 255, 255, 255, 128, 128, 128 },
        },
        {
            { 1, 204, 254, 255, 245, 255, 128, 128, 128, 128, 128 },
            { 207, 160, 250, 255, 238, 128, 128, 128, 128, 128, 128 },
            { 102, 103, 231, 255, 211, 171, 128, 128, 128, 128, 128 },
        },
        {
            { 1, 152, 252, 255, 240, 255, 128, 128, 128, 128, 128 },
            { 177, 135, 243, 255, 234, 225, 128, 128, 128, 128, 128 },
            { 80, 129, 211, 255, 194, 224, 128, 128, 128, 128, 128 },
        },
        {
            { 1, 1, 255, 128, 128, 128, 128, 128, 128, 128, 128 },
            { 246, 1, 255, 128, 128, 128, 128, 128, 128, 128, 128 },
            { 255, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128 },
        },
    },
    {
        {
            { 198, 35, 237, 223, 193, 187, 162, 160, 145, 155, 62 },
            { 131, 45, 198, 221, 172, 176, 220, 157, 252, 221, 1 },
            { 68, 47, 146, 208, 149, 167, 221, 162, 255, 223, 128 },
        },
        {
            { 1, 149, 241, 255, 221, 224, 255, 255, 128, 128, 128 },
            { 184, 141, 234, 253, 222, 220, 255, 199, 128, 128, 128 },
            { 81, 99, 181, 242, 176, 190, 249, 202, 255, 255, 128 },
        },
        {
            { 1, 129, 232, 253, 214, 197, 242, 196, 255, 255, 128 },
            { 99, 121, 210, 250, 201, 198, 255, 202, 128, 128, 128 },
            { 23, 91, 163, 242, 170, 187, 247, 210, 255, 255, 128 },
        },
        {
            { 1, 200, 246, 255, 234, 255, 128, 128, 128, 128, 128 },
            { 109, 178, 241, 255, 231, 245, 255, 255, 128, 128, 128 },
            { 44, 130, 201, 253, 205, 192, 255, 255, 128, 128, 128 },
        },
        {
            { 1, 132, 239, 251, 219, 209, 255, 165, 128, 128, 128 },
            { 94, 136, 225, 251, 218, 190, 255, 255, 128, 128, 128 },
            { 22, 100, 174, 245, 186, 161, 255, 199, 128, 128, 128 },
        },
        {
            { 1, 182, 249, 255, 232, 235, 128, 128, 128, 128, 128 },
            { 124, 143, 241, 255, 227, 234, 128, 128, 128, 128, 128 },
            { 35, 77, 181, 251, 193, 211, 255, 205, 128, 128, 128 },
        },
        {
            { 1, 157, 247, 255, 236, 231, 255, 255, 128, 128, 128 },
            { 121, 141, 235, 255, 225, 227, 255, 255, 128, 128, 128 },
            { 45, 99, 188, 251, 195, 217, 255, 224, 128, 128, 128 },
        },
        {
            { 1, 1, 251, 255, 213, 255, 128, 128, 128, 128, 128 },
            { 203, 1, 248, 255, 255, 128, 128, 128, 128, 128, 128 },
            { 137, 1, 177, 255, 224, 255, 128, 128, 128, 128, 128 },
        },
    },
    {
        {
            { 253, 9, 248, 251, 207, 208, 255, 192, 128, 128, 128 },
            { 175, 13, 224, 243, 193, 185, 249, 198, 255, 255, 128 },
            { 73, 17, 171, 221, 161, 179, 236, 167, 255, 234, 128 },
        },
        {
            { 1, 95, 247, 253, 212, 183, 255, 255, 128, 128, 128 },
            { 239, 90, 244, 250, 211, 209, 255, 255, 128, 128, 128 },
            { 155, 77, 195, 248, 188, 195, 255, 255, 128, 128, 128 },
        },
        {
            { 1, 24, 239, 251, 218, 219, 255, 205, 128, 128, 128 },
            { 201, 51, 219, 255, 196, 186, 128, 128, 128, 128, 128 },
            { 69, 46, 190, 239, 201, 218, 255, 228, 128, 128, 128 },
        },
        {
            { 1, 191, 251, 255, 255, 128, 128, 128, 128, 128, 128 },
            { 223, 165, 249, 255, 213, 255, 128, 128, 128, 128, 128 },
            { 141, 124, 248, 255, 255, 128, 128, 128, 128, 128, 128 },
        },
        {
            { 1, 16, 248, 255, 255, 128, 128, 128, 128, 128, 128 },
            { 190, 36, 230, 255, 236, 255, 128, 128, 128, 128, 128 },
            { 149, 1, 255, 128, 128, 128, 128, 128, 128, 128, 128 },
        },
        {
            { 1, 226, 255, 128, 128, 128, 128, 128, 128, 128, 128 },
            { 247, 192, 255, 128, 128, 128, 128, 128, 128, 128, 128 },
            { 240, 128, 255, 128, 128, 128, 128, 128, 128, 128, 128 },
        },
        {
            { 1, 134, 252, 255, 255, 128, 128, 128, 128, 128, 128 },
            { 213, 62, 250, 255, 255, 128, 128, 128, 128, 128, 128 },
            { 55, 93, 255, 128, 128, 128, 128, 128, 128, 128, 128 },
        },
        {
            { 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128 },
            { 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128 },
            { 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128 },
        },
    },
    {
        {
            { 202, 24, 213, 235, 186, 191, 220, 160, 240, 175, 255 },
            { 126, 38, 182, 232, 169, 184, 228, 174, 255, 187, 128 },
            { 61, 46, 138, 219, 151, 178, 240, 170, 255, 216, 128 },
        },
        {
            { 1, 112, 230, 250, 199, 191, 247, 159, 255, 255, 128 },
            { 166, 109, 228, 252, 211, 215, 255, 174, 128, 128, 128 },
            { 39, 77, 162, 232, 172, 180, 245, 178, 255, 255, 128 },
        },
        {
            { 1, 52, 220, 246, 198, 199, 249, 220, 255, 255, 128 },
            { 124, 74, 191, 243, 183, 193, 250, 221, 255, 255, 128 },
            { 24, 71, 130, 219, 154, 170, 243, 182, 255, 255, 128 },
        },
        {
            { 1, 182, 225, 249, 219, 240, 255, 224, 128, 128, 128 },
            { 149, 150, 226, 252, 216, 205, 255, 171, 128, 128, 128 },
            { 28, 108, 170, 242, 183, 194, 254, 223, 255, 255, 128 },
        },
        {
            { 1, 81, 230, 252, 204, 203, 255, 192, 128, 128, 128 },
            { 123, 102, 209, 247, 188, 196, 255, 233, 128, 128, 128 },
            { 20, 95, 153, 243, 164, 173, 255, 203, 128, 128, 128 },
        },
        {
            { 1, 222, 248, 255, 216, 213, 128, 128, 128, 128, 128 },
            { 168, 175, 246, 252, 235, 205, 255, 255, 128, 128, 128 },
            { 47, 116, 215, 255, 211, 212, 255, 255, 128, 128, 128 },
        },
        {
            { 1, 121, 236, 253, 212, 214, 255, 255, 128, 128, 128 },
            { 141, 84, 213, 252, 201, 202, 255, 219, 128, 128, 128 },
            { 42, 80, 160, 240, 162, 185, 255, 205, 128, 128, 128 },
        },
        {
            { 1, 1, 255, 128, 128, 128, 128, 128, 128, 128, 128 },
            { 244, 1, 255, 128, 128, 128, 128, 128, 128, 128, 128 },
            { 238, 1, 255, 128, 128, 128, 128, 128, 128, 128, 128 },
        },
    },
};
// clang-format on

// https://datatracker.ietf.org/doc/html/rfc6386#section-14.1 "Dequantization"
static int constexpr dc_qlookup[] = {
     4,   5,   6,   7,   8,   9,  10,  10,   11,  12,  13,  14,  15,
    16,  17,  17,  18,  19,  20,  20,  21,   21,  22,  22,  23,  23,
    24,  25,  25,  26,  27,  28,  29,  30,   31,  32,  33,  34,  35,
    36,  37,  37,  38,  39,  40,  41,  42,   43,  44,  45,  46,  46,
    47,  48,  49,  50,  51,  52,  53,  54,   55,  56,  57,  58,  59,
    60,  61,  62,  63,  64,  65,  66,  67,   68,  69,  70,  71,  72,
    73,  74,  75,  76,  76,  77,  78,  79,   80,  81,  82,  83,  84,
    85,  86,  87,  88,  89,  91,  93,  95,   96,  98, 100, 101, 102,
    104, 106, 108, 110, 112, 114, 116, 118, 122, 124, 126, 128, 130,
    132, 134, 136, 138, 140, 143, 145, 148, 151, 154, 157,
};
static int constexpr ac_qlookup[] = {
      4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,  16,
     17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
     30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,
     43,  44,  45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,
     56,  57,  58,  60,  62,  64,  66,  68,  70,  72,  74,  76,  78,
     80,  82,  84,  86,  88,  90,  92,  94,  96,  98, 100, 102, 104,
    106, 108, 110, 112, 114, 116, 119, 122, 125, 128, 131, 134, 137,
    140, 143, 146, 149, 152, 155, 158, 161, 164, 167, 170, 173, 177,
    181, 185, 189, 193, 197, 201, 205, 209, 213, 217, 221, 225, 229,
    234, 239, 245, 249, 254, 259, 264, 269, 274, 279, 284,
};


// https://datatracker.ietf.org/doc/html/rfc6386#section-14.3 "Implementation of the WHT Inversion"
inline void vp8_short_inv_walsh4x4_c(i16* input, i16* output)
{
    i16 *ip = input;
    i16 *op = output;

    for(int i = 0; i < 4; i++) {
        int a1 = ip[0] + ip[12];
        int b1 = ip[4] + ip[8];
        int c1 = ip[4] - ip[8];
        int d1 = ip[0] - ip[12];

        op[0] = a1 + b1;
        op[4] = c1 + d1;
        op[8] = a1 - b1;
        op[12]= d1 - c1;
        ip++;
        op++;
    }

    ip = output;
    op = output;
    for(int i = 0; i < 4; i++) {
        int a1 = ip[0] + ip[3];
        int b1 = ip[1] + ip[2];
        int c1 = ip[1] - ip[2];
        int d1 = ip[0] - ip[3];

        int a2 = a1 + b1;
        int b2 = c1 + d1;
        int c2 = a1 - b1;
        int d2 = d1 - c1;
        op[0] = (a2 + 3) >> 3;
        op[1] = (b2 + 3) >> 3;
        op[2] = (c2 + 3) >> 3;
        op[3] = (d2 + 3) >> 3;

        ip += 4;
        op += 4;
    }
}

// https://datatracker.ietf.org/doc/html/rfc6386#section-14.4 "Implementation of the DCT Inversion"
inline void short_idct4x4llm_c(i16* input, i16* output, int pitch)
{
    static constexpr int cospi8sqrt2minus1 = 20091;
    static constexpr int sinpi8sqrt2       = 35468;

    i16* ip = input;
    i16* op = output;
    int shortpitch = pitch >> 1;

    for(int i = 0; i < 4; i++) {
        int a1 = ip[0] + ip[8];
        int b1 = ip[0] - ip[8];

        int temp1 = (ip[4] * sinpi8sqrt2) >> 16;
        int temp2 = ip[12] + ((ip[12] * cospi8sqrt2minus1) >> 16);
        int c1 = temp1 - temp2;

        temp1 = ip[4] + ((ip[4] * cospi8sqrt2minus1) >> 16);
        temp2 = (ip[12] * sinpi8sqrt2) >> 16;
        int d1 = temp1 + temp2;

        op[shortpitch * 0] = a1 + d1;
        op[shortpitch * 3] = a1 - d1;
        op[shortpitch * 1] = b1 + c1;
        op[shortpitch * 2] = b1 - c1;

        ip++;
        op++;
    }

    ip = output;
    op = output;
    for(int i = 0; i < 4; i++) {
        int a1 = ip[0] + ip[2];
        int b1 = ip[0] - ip[2];

        int temp1 = (ip[1] * sinpi8sqrt2) >> 16;
        int temp2 = ip[3] + ((ip[3] * cospi8sqrt2minus1) >> 16);
        int c1 = temp1 - temp2;

        temp1 = ip[1] + ((ip[1] * cospi8sqrt2minus1) >> 16);
        temp2 = (ip[3] * sinpi8sqrt2) >> 16;
        int d1 = temp1 + temp2;

        op[0] = (a1 + d1 + 4) >> 3;
        op[3] = (a1 - d1 + 4) >> 3;
        op[1] = (b1 + c1 + 4) >> 3;
        op[2] = (b1 - c1 + 4) >> 3;

        ip += shortpitch;
        op += shortpitch;
    }
}

}
