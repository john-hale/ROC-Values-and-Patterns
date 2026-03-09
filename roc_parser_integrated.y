%{
/*
 * roc_parser_integrated.y
 * Bison grammar for ROC values / patterns, integrated with roc_match.
 * Definitions 3.2.1 – 3.2.3 of the MOOSE dissertation (Chapter III).
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "roc_match.h"

/* ----  NodeList  ---- */
typedef struct {
    Node **items;
    int    count, capacity, all_values;
} NList;

static NList *nl_new(void) {
    NList *l = calloc(1, sizeof *l);
    l->capacity = 8;
    l->items    = malloc(8 * sizeof(Node *));
    l->all_values = 1;
    return l;
}
static NList *nl_app(NList *l, Node *n) {
    if (l->count == l->capacity) {
        l->capacity *= 2;
        l->items = realloc(l->items, l->capacity * sizeof(Node *));
    }
    l->items[l->count++] = n;
    if (!n->is_value) l->all_values = 0;
    return l;
}

/* ---- Accumulator for parsed nodes ---- */
static Node  **parsed_nodes = NULL;
static int     parsed_count = 0;
static int     parsed_cap   = 0;

static void push_node(Node *n) {
    if (parsed_count == parsed_cap) {
        parsed_cap = parsed_cap ? parsed_cap * 2 : 8;
        parsed_nodes = realloc(parsed_nodes, parsed_cap * sizeof(Node *));
    }
    parsed_nodes[parsed_count++] = n;
}

int  yylex(void);
void yyerror(const char *m) { fprintf(stderr, "Parse error: %s\n", m); }
%}

/* void* in union avoids forward-declaration problems with bison's header */
%union { char *sval; void *node; void *list; }

%token <sval> NAME AGENT WILDCARD
%token        BINDABLE NIL LBRACKET RBRACKET COMMA

%type <node> pattern
%type <list> pattern_list

%right BINDABLE
%start input

%%

/*
 * Unified grammar (see roc_values.y for full commentary).
 * Everything is parsed as a pattern; the is_value flag
 * distinguishes pure values (Def 3.2.1) from patterns (Def 3.2.2).
 */

input
    : /* empty */
    | input pattern {
        Node *nd = (Node *)$2;
        printf("Parsed %s:\n", nd->is_value ? "Value" : "Pattern");
        node_print(nd, 1);
        printf("\n");
        push_node(nd);
      }
    ;

pattern
    : NAME                           { $$ = mk_name($1);  free($1); }
    | AGENT                          { $$ = mk_agent($1); free($1); }
    | NIL                            { $$ = mk_nil(); }
    | WILDCARD                       { $$ = mk_wc($1);    free($1); }
    | pattern BINDABLE               { $$ = mk_bind((Node *)$1); }
    | LBRACKET pattern_list RBRACKET {
            NList *l = (NList *)$2;
            $$ = mk_tuple_from_array(l->items, l->count, l->all_values);
            free(l->items); free(l);
        }
    ;

pattern_list
    : pattern                    { $$ = nl_app(nl_new(), (Node *)$1); }
    | pattern_list COMMA pattern { $$ = nl_app((NList *)$1, (Node *)$3); }
    ;

%%

/* ----------------------------------------------------------------
 * Interactive driver
 *
 * Reads from stdin until EOF.  After parsing:
 *   1 node  : AST already printed during parse.
 *   2 nodes : if first is a value, runs roc_match and shows bindings.
 *   Even N  : pairwise match on consecutive pairs.
 *   Odd  N  : pairs all but the last; reports the leftover.
 * ---------------------------------------------------------------- */
int main(void) {
    printf("ROC Parser + Matcher  (Defs 3.2.1-3.2.3)\n");
    printf("Enter values/patterns one per line; EOF (Ctrl-D) to run matcher:\n\n");

    int rc = yyparse();
    if (rc != 0) return rc;
    if (parsed_count == 0) { printf("(no input)\n"); return 0; }
    if (parsed_count == 1) return 0;

    printf("===============================================\n");
    printf("Matching  (%d node(s) -> %d pair(s))\n\n",
           parsed_count, parsed_count / 2);

    for (int i = 0; i + 1 < parsed_count; i += 2) {
        Node *v = parsed_nodes[i];
        Node *x = parsed_nodes[i + 1];

        printf("Pair %d:\n", i / 2 + 1);
        printf("  value  : "); node_print(v, 0);
        printf("  pattern: "); node_print(x, 0);

        if (!v->is_value) {
            printf("  *** First node contains wildcards — "
                   "not a valid value. ***\n\n");
            continue;
        }

        BindingList *bl = binding_list_new();
        int matched = roc_match(v, x, bl);
        printf("  result : %s\n", matched ? "MATCH" : "NO MATCH");
        if (matched) {
            printf("  bindings:\n");
            binding_list_print(bl);
        }
        binding_list_free(bl);
        printf("\n");
    }

    if (parsed_count % 2 != 0)
        printf("(note: %d nodes — last node left unpaired)\n", parsed_count);

    return 0;
}
