#include "erl_nif.h"
#include "roaring.h"

#define __UNUSED(v) ((void)(v))

static ErlNifResourceType *rb_res_type;
static ErlNifResourceType *it_res_type;

static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_UNDEFINED;

typedef struct {
  roaring_bitmap_t *rb;
} rb_res;

typedef struct {
  roaring_uint32_iterator_t *it;
  rb_res *rbres;
} it_res;

static ERL_NIF_TERM rb_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res;

  if(argc != 0) return enif_make_badarg(env);

  res = (rb_res *)enif_alloc_resource(rb_res_type, sizeof(rb_res));
  if(!res) enif_make_badarg(env);

  res->rb = roaring_bitmap_create();
  if(!res->rb) enif_make_badarg(env);

  return enif_make_resource(env, res);
}

static ERL_NIF_TERM rb_from_range(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res;
  uint32_t min, max, step;

  if(!(argc == 3 &&
      enif_get_uint(env, argv[0], &min) &&
      enif_get_uint(env, argv[1], &max) &&
      enif_get_uint(env, argv[2], &step))) {
    return enif_make_badarg(env);
  }

  res = (rb_res *)enif_alloc_resource(rb_res_type, sizeof(rb_res));
  if(!res) enif_make_badarg(env);

  res->rb = roaring_bitmap_from_range(min, max, step);
  if(!res->rb) enif_make_badarg(env);

  return enif_make_resource(env, res);
}

static ERL_NIF_TERM rb_from_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res;
  uint32_t n;

  if(!(argc == 1 &&
      enif_is_list(env, argv[0]))) {
    return enif_make_badarg(env);
  }

  roaring_bitmap_t *rb = roaring_bitmap_create();
  if(!rb) enif_make_badarg(env);

  ERL_NIF_TERM head, tail;

  tail = argv[0];

  while(enif_get_list_cell(env, tail, &head, &tail)) {
    if(!enif_get_uint(env, head, &n))
      goto err;

    roaring_bitmap_add(rb, n);
  }


  res = (rb_res *)enif_alloc_resource(rb_res_type, sizeof(rb_res));
  if(!res) goto err;

  res->rb = rb;
  return enif_make_resource(env, res);

  err:
  roaring_bitmap_free(rb);
  return enif_make_badarg(env);
}

static ERL_NIF_TERM rb_intersection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res, *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  res = (rb_res *)enif_alloc_resource(rb_res_type, sizeof(rb_res));
  if(!res) enif_make_badarg(env);

  res->rb = roaring_bitmap_and(res1->rb, res2->rb);
  if(!res->rb) enif_make_badarg(env);

  return enif_make_resource(env, res);
}

static ERL_NIF_TERM rb_union(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res, *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  res = (rb_res *)enif_alloc_resource(rb_res_type, sizeof(rb_res));
  if(!res) enif_make_badarg(env);

  res->rb = roaring_bitmap_or(res1->rb, res2->rb);
  if(!res->rb) enif_make_badarg(env);

  return enif_make_resource(env, res);
}

static ERL_NIF_TERM rb_sym_difference(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res, *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  res = (rb_res *)enif_alloc_resource(rb_res_type, sizeof(rb_res));
  if(!res) enif_make_badarg(env);

  res->rb = roaring_bitmap_xor(res1->rb, res2->rb);
  if(!res->rb) enif_make_badarg(env);

  return enif_make_resource(env, res);
}

static ERL_NIF_TERM rb_difference(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res, *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  res = (rb_res *)enif_alloc_resource(rb_res_type, sizeof(rb_res));
  if(!res) enif_make_badarg(env);

  res->rb = roaring_bitmap_andnot(res1->rb, res2->rb);
  if(!res->rb) enif_make_badarg(env);

  return enif_make_resource(env, res);
}

static ERL_NIF_TERM rb_add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res, *res1;
  uint32_t n;

  if(!(argc == 2 &&
      enif_get_uint(env, argv[0], &n) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  res = (rb_res *)enif_alloc_resource(rb_res_type, sizeof(rb_res));
  if(!res) enif_make_badarg(env);

  res->rb = roaring_bitmap_copy(res1->rb);
  if(!res->rb) enif_make_badarg(env);

  roaring_bitmap_add(res->rb, n);

  return enif_make_resource(env, res);
}

static ERL_NIF_TERM rb_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res, *res1;
  uint32_t n;

  if(!(argc == 2 &&
      enif_get_uint(env, argv[0], &n) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  res = (rb_res *)enif_alloc_resource(rb_res_type, sizeof(rb_res));
  if(!res) enif_make_badarg(env);

  res->rb = roaring_bitmap_copy(res1->rb);
  if(!res->rb) enif_make_badarg(env);

  roaring_bitmap_remove(res->rb, n);

  return enif_make_resource(env, res);
}

static ERL_NIF_TERM rb_is_member(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;
  uint32_t n;

  if(!(argc == 2 &&
      enif_get_uint(env, argv[0], &n) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  if(roaring_bitmap_contains(res1->rb, n))
    return ATOM_TRUE;
  else
    return ATOM_FALSE;
}

static ERL_NIF_TERM rb_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  return enif_make_uint64(env, roaring_bitmap_get_cardinality(res1->rb));
}

static ERL_NIF_TERM rb_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  uint64_t c = roaring_bitmap_get_cardinality(res1->rb);
  ERL_NIF_TERM list = enif_make_list(env, 0);
  ERL_NIF_TERM n;

  if(c > 0) {
    uint32_t *ans = (uint32_t *)enif_alloc(c * sizeof(uint32_t));
    roaring_bitmap_to_uint32_array(res1->rb, ans);
    uint64_t i = c;

    do {
      n = enif_make_uint(env, ans[--i]);
      list = enif_make_list_cell(env, n, list);
    } while(i > 0);
  }

  return list;
}

static ERL_NIF_TERM rb_is_subset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  if(roaring_bitmap_is_subset(res1->rb, res2->rb))
    return ATOM_TRUE;
  else
    return ATOM_FALSE;
}

static ERL_NIF_TERM rb_is_strict_subset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  if(roaring_bitmap_is_strict_subset(res1->rb, res2->rb))
    return ATOM_TRUE;
  else
    return ATOM_FALSE;
}

static ERL_NIF_TERM rb_equals(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  if(roaring_bitmap_equals(res1->rb, res2->rb))
    return ATOM_TRUE;
  else
    return ATOM_FALSE;
}

static ERL_NIF_TERM rb_iterator(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;
  it_res *res;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  res = (it_res *)enif_alloc_resource(it_res_type, sizeof(it_res));
  if(!res) enif_make_badarg(env);

  res->it = NULL;
  res->rbres = res1;
  enif_keep_resource(res1);

  return enif_make_resource(env, res);
}

static ERL_NIF_TERM rb_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  it_res *res;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], it_res_type, (void**)&res))) {
    return enif_make_badarg(env);
  }

  if(!res->it) {
    res->it = roaring_create_iterator(res->rbres->rb);
    if(!res->it) enif_make_badarg(env);

    return enif_make_uint(env, res->it->current_value);
  }
  else if(roaring_advance_uint32_iterator(res->it))
    return enif_make_uint(env, res->it->current_value);
  else
    return ATOM_UNDEFINED;
}

/*
static ERL_NIF_TERM rb_move(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  it_res *res;
  uint32_t n;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], it_res_type, (void**)&res) &&
      enif_get_uint(env, argv[1], &n))) {
    return enif_make_badarg(env);
  }

  if(roaring_move_uint32_iterator_equalorlarger(res->it, n))
    return enif_make_uint(env, res->it->current_value);
  else
    return ATOM_UNDEFINED;
}
*/

static void rb_res_dtor(ErlNifEnv *env, void *resource) {
  __UNUSED(env);

  rb_res *res = (rb_res*)resource;

  if(res->rb)
    roaring_bitmap_free(res->rb);
}

static void it_res_dtor(ErlNifEnv *env, void *resource) {
  __UNUSED(env);

  it_res *res = (it_res*)resource;

  if(res->it)
    roaring_free_uint32_iterator(res->it);

  enif_release_resource(res->rbres);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  __UNUSED(priv_data);
  __UNUSED(load_info);

  rb_res_type =
    enif_open_resource_type(env, NULL, "roaring_bitmap",
      rb_res_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

  it_res_type =
    enif_open_resource_type(env, NULL, "roaring_iterator",
      it_res_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

  ATOM_TRUE = enif_make_atom(env, "true");
  ATOM_FALSE = enif_make_atom(env, "false");
  ATOM_UNDEFINED = enif_make_atom(env, "undefined");

  return 0;
}

static ErlNifFunc nif_funcs[] =
{
  {"new", 0, rb_new},
  {"from_list", 1, rb_from_list},
  {"from_range", 3, rb_from_range},
  {"intersection", 2, rb_intersection},
  {"union", 2, rb_union},
  {"sym_difference", 2, rb_sym_difference},
  {"difference", 2, rb_difference},
  {"add", 2, rb_add},
  {"delete", 2, rb_delete},
  {"is_member", 2, rb_is_member},
  {"size", 1, rb_size},
  {"to_list", 1, rb_to_list},
  {"is_subset", 2, rb_is_subset},
  {"is_strict_subset", 2, rb_is_strict_subset},
  {"equals", 2, rb_equals},
  {"iterator", 1, rb_iterator},
  {"next", 1, rb_next}
//  {"move", 1, rb_move}
};

ERL_NIF_INIT(rb_sets, nif_funcs, &load, NULL, NULL, NULL);
