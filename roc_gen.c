/*
 * roc_gen.c
 * Synthetic ROC value/pattern pair generator
 *
 * Generates matched pairs of values and patterns suitable for piping
 * directly into roc_parser.  Every generated pair is guaranteed to
 * produce a MATCH so you can verify correctness as well as performance.
 *
 * Parameters (all via command-line flags):
 *
 *   -d <int>    max depth         — maximum nesting depth of tuples (default 3)
 *   -b <int>    branching factor  — number of children per tuple node (default 2)
 *   -w <int>    width             — number of elements in leaf tuples (default 3)
 *   -W <float>  wildcard density  — probability [0,1] that a leaf position in
 *                                   the pattern becomes a wildcard n? (default 0.3)
 *                                   When a wildcard is chosen the corresponding
 *                                   value element is made bindable (v#) so rule
 *                                   M3 fires.  Otherwise the element is left
 *                                   unbindable and the pattern mirrors the value
 *                                   (rules M4/M5 fire instead).
 *   -B <float>  bindable density  — probability [0,1] that a non-wildcard leaf
 *                                   value is made bindable v# anyway (exercises
 *                                   rules M1/M2) (default 0.2)
 *   -n <int>    num pairs         — number of value/pattern pairs to emit (default 1)
 *   -s <int>    random seed       — seed for repeatable output (default: time-based)
 *   -p          pretty            — indent output for readability (default: compact)
 *   -h          help              — print this message
 *
 * Output format:
 *   One value expression per line followed immediately by its pattern,
 *   repeated -n times.  Feed directly to roc_parser:
 *
 *     ./roc_gen -d 4 -b 2 -W 0.5 -n 10 | ./roc_parser
 *
 * Design notes:
 *   - Names are drawn from a small alphabet (a..z) suffixed with a counter
 *     so they are always distinct: a0, b1, c2, ...
 *   - Agents are uppercase versions: A0, B1, C2, ...  introduced with
 *     probability 0.1 at leaf positions to exercise agent matching.
 *   - nil is introduced with probability 0.05 at leaf positions.
 *   - The tree shape is uniform: every non-leaf node has exactly
 *     <branching factor> children.  Leaves are either scalars (name/agent/nil)
 *     or flat tuples of <width> scalars.
 *   - At depth == max_depth the node is always a scalar (prevents runaway size).
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

/* ----------------------------------------------------------------
 * Parameters
 * ---------------------------------------------------------------- */
typedef struct {
    int   max_depth;        /* -d */
    int   branching;        /* -b */
    int   width;            /* -w */
    double wildcard_density;/* -W */
    double bindable_density;/* -B */
    int   num_pairs;        /* -n */
    unsigned int seed;      /* -s */
    int   pretty;           /* -p */
} Params;

static Params P = {
    .max_depth        = 3,
    .branching        = 2,
    .width            = 3,
    .wildcard_density = 0.3,
    .bindable_density = 0.2,
    .num_pairs        = 1,
    .seed             = 0,
    .pretty           = 0,
};

/* ----------------------------------------------------------------
 * Name counter — ensures all leaf names are unique within a pair
 * ---------------------------------------------------------------- */
static int name_counter = 0;

static void name_reset(void) { name_counter = 0; }

/* Write a fresh name into buf (at least 16 bytes).
 * 10% chance of uppercase agent, 5% chance of nil, rest lowercase name. */
static void fresh_name(char *buf, int *is_agent, int *is_nil) {
    double r = (double)rand() / RAND_MAX;
    *is_agent = 0;
    *is_nil   = 0;
    if (r < 0.05) {
        strcpy(buf, "nil");
        *is_nil = 1;
    } else if (r < 0.15) {
        char base = 'A' + (name_counter % 26);
        sprintf(buf, "%c%d", base, name_counter);
        *is_agent = 1;
        name_counter++;
    } else {
        char base = 'a' + (name_counter % 26);
        sprintf(buf, "%c%d", base, name_counter);
        name_counter++;
    }
}

/* ----------------------------------------------------------------
 * Output helpers
 * ---------------------------------------------------------------- */


/* ----------------------------------------------------------------
 * Core generator
 *
 * We build value and pattern simultaneously so we can guarantee
 * every pair matches.  Both are written to dynamically-grown
 * string buffers, then printed.
 *
 * Strategy:
 *   - At each tuple node, recurse into each child.
 *   - At each leaf scalar:
 *       * Roll wildcard_density:
 *           YES → value gets  name#  (bindable, rule M3)
 *                  pattern gets n?   (wildcard)
 *           NO  → roll bindable_density:
 *                   YES → value  name#  pattern  name# or name (rule M1/M2/M5)
 *                   NO  → value  name   pattern  name          (rule M4)
 * ---------------------------------------------------------------- */

/* Dynamic string buffer */
typedef struct { char *s; int len; int cap; } Buf;

static void buf_init(Buf *b) {
    b->cap = 256; b->len = 0;
    b->s = malloc(b->cap);
    b->s[0] = '\0';
}
static void buf_free(Buf *b) { free(b->s); }
static void buf_append(Buf *b, const char *str) {
    int slen = strlen(str);
    while (b->len + slen + 1 > b->cap) {
        b->cap *= 2;
        b->s = realloc(b->s, b->cap);
    }
    memcpy(b->s + b->len, str, slen + 1);
    b->len += slen;
}
static void buf_appendc(Buf *b, char c) {
    char tmp[2] = { c, '\0' };
    buf_append(b, tmp);
}

/* Forward declaration */
static void gen_node(Buf *vbuf, Buf *pbuf, int depth);

/* Generate a single leaf scalar into both buffers */
static void gen_leaf(Buf *vbuf, Buf *pbuf) {
    char name[32];
    int is_agent, is_nil;
    fresh_name(name, &is_agent, &is_nil);

    double r_wc   = (double)rand() / RAND_MAX;
    double r_bind = (double)rand() / RAND_MAX;

    if (is_nil) {
        /* nil is always unbindable; cannot match wildcard — use M4 */
        buf_append(vbuf, "nil");
        buf_append(pbuf, "nil");
        return;
    }

    if (r_wc < P.wildcard_density) {
        /* Rule M3: value is bindable, pattern is wildcard */
        buf_append(vbuf, name);
        buf_appendc(vbuf, '#');

        /* Wildcard name: prepend 'w' to avoid collision with value names */
        buf_appendc(pbuf, 'w');
        buf_append(pbuf, name);   /* e.g. wa3 */
        buf_appendc(pbuf, '?');

    } else if (r_bind < P.bindable_density) {
        /* Rules M1/M2: value is bindable, pattern may or may not be */
        buf_append(vbuf, name);
        buf_appendc(vbuf, '#');

        double r_pat = (double)rand() / RAND_MAX;
        buf_append(pbuf, name);
        if (r_pat < 0.5) {
            /* M1: pattern also bindable */
            buf_appendc(pbuf, '#');
        }
        /* else M2: pattern is unbindable — v# ~ v */

    } else {
        /* Rule M4/M5: plain value */
        buf_append(vbuf, name);

        double r_pat = (double)rand() / RAND_MAX;
        buf_append(pbuf, name);
        if (r_pat < P.bindable_density) {
            /* M5: pattern is bindable, value is not — v ~ v# */
            buf_appendc(pbuf, '#');
        }
        /* else M4: both plain */
    }
}

/* Generate a flat tuple of <width> leaf scalars */
static void gen_flat_tuple(Buf *vbuf, Buf *pbuf) {
    buf_appendc(vbuf, '[');
    buf_appendc(pbuf, '[');
    for (int i = 0; i < P.width; i++) {
        if (i > 0) {
            buf_appendc(vbuf, ',');
            buf_appendc(pbuf, ',');
            if (P.pretty) {
                buf_append(vbuf, "\n  ");
                buf_append(pbuf, "\n  ");
            }
        }
        gen_leaf(vbuf, pbuf);
    }
    buf_appendc(vbuf, ']');
    buf_appendc(pbuf, ']');
}

/* Recursively generate a node at the given depth */
static void gen_node(Buf *vbuf, Buf *pbuf, int depth) {
    if (depth >= P.max_depth) {
        /* At max depth: emit a leaf scalar or flat tuple */
        double r = (double)rand() / RAND_MAX;
        if (P.width > 1 && r < 0.6) {
            gen_flat_tuple(vbuf, pbuf);
        } else {
            gen_leaf(vbuf, pbuf);
        }
        return;
    }

    /* Internal node: emit a tuple with <branching> children */
    buf_appendc(vbuf, '[');
    buf_appendc(pbuf, '[');

    for (int i = 0; i < P.branching; i++) {
        if (i > 0) {
            buf_appendc(vbuf, ',');
            buf_appendc(pbuf, ',');
            if (P.pretty) {
                buf_append(vbuf, "\n");
                for (int d = 0; d <= depth; d++) buf_append(vbuf, "  ");
                buf_append(pbuf, "\n");
                for (int d = 0; d <= depth; d++) buf_append(pbuf, "  ");
            }
        }

        /*
         * Special case: with wildcard_density probability, capture the
         * entire child subtree with a single wildcard.  This requires
         * generating the value subtree normally (with # on the root)
         * but replacing the entire pattern subtree with a single n?.
         */
        double r_wc = (double)rand() / RAND_MAX;
        if (depth < P.max_depth - 1 && r_wc < P.wildcard_density * 0.3) {
            /* Subtree wildcard: generate the value child into a temp buf,
             * wrap it in #, then emit a single wildcard in the pattern. */
            Buf tmp_v, tmp_p;
            buf_init(&tmp_v);
            buf_init(&tmp_p);
            gen_node(&tmp_v, &tmp_p, depth + 1);

            /* value side: wrap child in bindable */
            buf_append(vbuf, tmp_v.s);
            buf_appendc(vbuf, '#');

            /* pattern side: single wildcard capturing whole subtree */
            char wc_name[32];
            sprintf(wc_name, "sub%d", name_counter++);
            buf_appendc(pbuf, 'w');
            buf_append(pbuf, wc_name);
            buf_appendc(pbuf, '?');

            buf_free(&tmp_v);
            buf_free(&tmp_p);
        } else {
            gen_node(vbuf, pbuf, depth + 1);
        }
    }

    buf_appendc(vbuf, ']');
    buf_appendc(pbuf, ']');
}

/* ----------------------------------------------------------------
 * Usage
 * ---------------------------------------------------------------- */
static void usage(const char *prog) {
    fprintf(stderr,
        "Usage: %s [options]\n"
        "\n"
        "Options:\n"
        "  -d <int>    max depth          (default %d)\n"
        "  -b <int>    branching factor   (default %d)\n"
        "  -w <int>    leaf tuple width   (default %d)\n"
        "  -W <float>  wildcard density   0.0-1.0  (default %.2f)\n"
        "  -B <float>  bindable density   0.0-1.0  (default %.2f)\n"
        "  -n <int>    number of pairs    (default %d)\n"
        "  -s <int>    random seed        (default: time-based)\n"
        "  -p          pretty-print output\n"
        "  -h          this help\n"
        "\n"
        "Output: one value line + one pattern line per pair, ready for roc_parser.\n"
        "\n"
        "Examples:\n"
        "  %s -d 3 -b 2 -W 0.5 -n 5 | ./roc_parser\n"
        "  %s -d 5 -b 3 -W 0.8 -B 0.1 -n 1 -p\n"
        "  %s -d 2 -b 4 -w 5 -W 0.0 -n 10 | ./roc_parser   # no wildcards\n"
        "  %s -d 4 -b 2 -W 1.0 -n 3  | ./roc_parser         # all wildcards\n",
        prog,
        P.max_depth, P.branching, P.width,
        P.wildcard_density, P.bindable_density, P.num_pairs,
        prog, prog, prog, prog);
}

/* ----------------------------------------------------------------
 * main
 * ---------------------------------------------------------------- */
int main(int argc, char **argv) {
    int seed_set = 0;

    for (int i = 1; i < argc; i++) {
        if      (!strcmp(argv[i], "-d") && i+1 < argc) P.max_depth        = atoi(argv[++i]);
        else if (!strcmp(argv[i], "-b") && i+1 < argc) P.branching        = atoi(argv[++i]);
        else if (!strcmp(argv[i], "-w") && i+1 < argc) P.width            = atoi(argv[++i]);
        else if (!strcmp(argv[i], "-W") && i+1 < argc) P.wildcard_density = atof(argv[++i]);
        else if (!strcmp(argv[i], "-B") && i+1 < argc) P.bindable_density = atof(argv[++i]);
        else if (!strcmp(argv[i], "-n") && i+1 < argc) P.num_pairs        = atoi(argv[++i]);
        else if (!strcmp(argv[i], "-s") && i+1 < argc) { P.seed = atoi(argv[++i]); seed_set = 1; }
        else if (!strcmp(argv[i], "-p"))                P.pretty           = 1;
        else if (!strcmp(argv[i], "-h")) { usage(argv[0]); return 0; }
        else { fprintf(stderr, "Unknown option: %s\n", argv[i]); usage(argv[0]); return 1; }
    }

    /* Validate */
    if (P.max_depth   < 1) { fprintf(stderr, "depth must be >= 1\n");      return 1; }
    if (P.branching   < 1) { fprintf(stderr, "branching must be >= 1\n");  return 1; }
    if (P.width       < 1) { fprintf(stderr, "width must be >= 1\n");      return 1; }
    if (P.num_pairs   < 1) { fprintf(stderr, "num_pairs must be >= 1\n");  return 1; }
    if (P.wildcard_density < 0.0 || P.wildcard_density > 1.0)
        { fprintf(stderr, "-W must be in [0,1]\n"); return 1; }
    if (P.bindable_density < 0.0 || P.bindable_density > 1.0)
        { fprintf(stderr, "-B must be in [0,1]\n"); return 1; }

    /* Seed RNG */
    if (!seed_set) P.seed = (unsigned int)time(NULL);
    srand(P.seed);

    /* Emit header comment so output is self-documenting */
    printf("# roc_gen  d=%d b=%d w=%d W=%.2f B=%.2f n=%d seed=%u\n",
           P.max_depth, P.branching, P.width,
           P.wildcard_density, P.bindable_density,
           P.num_pairs, P.seed);

    for (int pair = 0; pair < P.num_pairs; pair++) {
        name_reset();

        Buf vbuf, pbuf;
        buf_init(&vbuf);
        buf_init(&pbuf);

        gen_node(&vbuf, &pbuf, 0);

        if (P.pretty) {
            printf("# --- pair %d ---\n", pair + 1);
            printf("# value:\n%s\n", vbuf.s);
            printf("# pattern:\n%s\n", pbuf.s);
        } else {
            printf("%s\n%s\n", vbuf.s, pbuf.s);
        }

        buf_free(&vbuf);
        buf_free(&pbuf);
    }

    return 0;
}
