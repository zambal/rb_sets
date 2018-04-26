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
  bool mutable;
} rb_res;

typedef struct {
  roaring_uint32_iterator_t *it;
  ErlNifMutex *lock;
  rb_res *rbres;
} it_res;

typedef struct {
  ErlNifMutex *lock;
} rb_priv_data;

static inline ERL_NIF_TERM rb_make_resource(ErlNifEnv *env, roaring_bitmap_t *rb, bool mutable) {
  if(!rb) return enif_make_badarg(env);

  rb_res *res = (rb_res *)enif_alloc_resource(rb_res_type, sizeof(rb_res));
  if(!res) return enif_make_badarg(env);

  res->rb = rb;
  res->mutable = mutable;

  return enif_make_resource(env, res);
}

static inline ErlNifMutex *rb_global_lock(ErlNifEnv *env) {
  rb_priv_data *priv = (rb_priv_data *)enif_priv_data(env);

  enif_mutex_lock(priv->lock);
  return priv->lock;
}

static inline ErlNifMutex *rb_get_global_lock(ErlNifEnv *env) {
  rb_priv_data *priv = (rb_priv_data *)enif_priv_data(env);
  return priv->lock;
}

static ERL_NIF_TERM rb_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if(argc != 0) return enif_make_badarg(env);

  return rb_make_resource(env, roaring_bitmap_create(), false);
}

static ERL_NIF_TERM rb_new_mutable(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if(argc != 0) return enif_make_badarg(env);

  return rb_make_resource(env, roaring_bitmap_create(), true);
}

static ERL_NIF_TERM rb_from_range(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  uint32_t min, max, step;

  if(!(argc == 3 &&
      enif_get_uint(env, argv[0], &min) &&
      enif_get_uint(env, argv[1], &max) &&
      enif_get_uint(env, argv[2], &step))) {
    return enif_make_badarg(env);
  }

  return rb_make_resource(env, roaring_bitmap_from_range(min, max, step), false);
}

static ERL_NIF_TERM rb_from_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  uint32_t n;

  if(!(argc == 1 &&
      enif_is_list(env, argv[0]))) {
    return enif_make_badarg(env);
  }

  roaring_bitmap_t *rb = roaring_bitmap_create();
  if(!rb) return enif_make_badarg(env);

  ERL_NIF_TERM head, tail;

  tail = argv[0];

  while(enif_get_list_cell(env, tail, &head, &tail)) {
    if(!enif_get_uint(env, head, &n))
      goto err;

    roaring_bitmap_add(rb, n);
  }

  return rb_make_resource(env, rb, false);

  err:
  roaring_bitmap_free(rb);
  return enif_make_badarg(env);
}

static ERL_NIF_TERM rb_from_mutable(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    roaring_bitmap_t *rb = roaring_bitmap_copy(res1->rb);
    enif_mutex_unlock(lock);
    return rb_make_resource(env, rb, false);
  }
  else
    return argv[0];
}

static ERL_NIF_TERM rb_set_immutable(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    res1->mutable = false;
    enif_mutex_unlock(lock);
  }

  return argv[0];
}


static ERL_NIF_TERM rb_from_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary bin;

  if(!(argc == 1 &&
      enif_inspect_binary(env, argv[0], &bin))) {
    return enif_make_badarg(env);
  }

  roaring_bitmap_t *rb = roaring_bitmap_deserialize(bin.data);
  if(!rb) return enif_make_badarg(env);

  return rb_make_resource(env, rb, false);
}

static ERL_NIF_TERM rb_to_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;
  size_t size;
  ERL_NIF_TERM bin;
  unsigned char *buf;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    size = roaring_bitmap_size_in_bytes(res1->rb);
    buf = enif_make_new_binary(env, size, &bin);
    if(!buf) return enif_make_badarg(env);
    roaring_bitmap_serialize(res1->rb, (char *)buf);
    enif_mutex_unlock(lock);
  }
  else {
    size = roaring_bitmap_size_in_bytes(res1->rb);
    buf = enif_make_new_binary(env, size, &bin);
    if(!buf) return enif_make_badarg(env);
    roaring_bitmap_serialize(res1->rb, (char *)buf);
  }

  return bin;
}


static ERL_NIF_TERM rb_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  ERL_NIF_TERM list = enif_make_list(env, 0);
  ERL_NIF_TERM n;
  ErlNifMutex *lock;
  bool mutable = res1->mutable;

  if(mutable) lock = rb_global_lock(env);
  uint64_t c = roaring_bitmap_get_cardinality(res1->rb);

  if(c > 0) {
    uint32_t *ans = (uint32_t *)enif_alloc(c * sizeof(uint32_t));
    roaring_bitmap_to_uint32_array(res1->rb, ans);
    if(mutable) enif_mutex_unlock(lock);

    uint64_t i = c;
    do {
      n = enif_make_uint(env, ans[--i]);
      list = enif_make_list_cell(env, n, list);
    } while(i > 0);

  } else if(mutable) enif_mutex_unlock(lock);

  return list;
}

static ERL_NIF_TERM rb_to_mutable(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable)
    return argv[0];
  else
    return rb_make_resource(env, roaring_bitmap_copy(res1->rb), true);
}

static ERL_NIF_TERM rb_is_mutable(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable)
    return ATOM_TRUE;
  else
    return ATOM_FALSE;
}

static ERL_NIF_TERM rb_is_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    bool empty = roaring_bitmap_is_empty(res1->rb);
    enif_mutex_unlock(lock);

    if(empty)
      return ATOM_TRUE;
    else
      return ATOM_FALSE;
  }
  else {
    if(roaring_bitmap_is_empty(res1->rb))
      return ATOM_TRUE;
    else
      return ATOM_FALSE;
  }
}

static ERL_NIF_TERM rb_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    uint64_t size = roaring_bitmap_get_cardinality(res1->rb);
    enif_mutex_unlock(lock);
    return enif_make_uint64(env, size);
  }
  else {
    return enif_make_uint64(env, roaring_bitmap_get_cardinality(res1->rb));
  }
}

static ERL_NIF_TERM rb_intersection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    roaring_bitmap_and_inplace(res1->rb, res2->rb);
    enif_mutex_unlock(lock);
    return argv[0];
  }
  else if(res2->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    roaring_bitmap_t *rb = roaring_bitmap_and(res1->rb, res2->rb);
    enif_mutex_unlock(lock);
    return rb_make_resource(env, rb, false);
  }
  else {
    roaring_bitmap_t *rb = roaring_bitmap_and(res1->rb, res2->rb);
    return rb_make_resource(env, rb, false);
  }
}

static ERL_NIF_TERM rb_union(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    roaring_bitmap_or_inplace(res1->rb, res2->rb);
    enif_mutex_unlock(lock);
    return argv[0];
  }
  else if(res2->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    roaring_bitmap_t *rb = roaring_bitmap_or(res1->rb, res2->rb);
    enif_mutex_unlock(lock);
    return rb_make_resource(env, rb, false);
  }
  else {
    roaring_bitmap_t *rb = roaring_bitmap_or(res1->rb, res2->rb);
    return rb_make_resource(env, rb, false);
  }
}

static ERL_NIF_TERM rb_sym_difference(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    roaring_bitmap_xor_inplace(res1->rb, res2->rb);
    enif_mutex_unlock(lock);
    return argv[0];
  }
  else if(res2->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    roaring_bitmap_t *rb = roaring_bitmap_xor(res1->rb, res2->rb);
    enif_mutex_unlock(lock);
    return rb_make_resource(env, rb, false);
  }
  else {
    roaring_bitmap_t *rb = roaring_bitmap_xor(res1->rb, res2->rb);
    return rb_make_resource(env, rb, false);
  }
}

static ERL_NIF_TERM rb_difference(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    roaring_bitmap_andnot_inplace(res1->rb, res2->rb);
    enif_mutex_unlock(lock);
    return argv[0];
  }
  else if(res2->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    roaring_bitmap_t *rb = roaring_bitmap_andnot(res1->rb, res2->rb);
    enif_mutex_unlock(lock);
    return rb_make_resource(env, rb, false);
  }
  else {
    roaring_bitmap_t *rb = roaring_bitmap_andnot(res1->rb, res2->rb);
    return rb_make_resource(env, rb, false);
  }
}

static ERL_NIF_TERM rb_flip(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;
  uint32_t start, end;

  if(!(argc == 3 &&
      enif_get_uint(env, argv[0], &start) &&
      enif_get_uint(env, argv[1], &end) &&
      enif_get_resource(env, argv[2], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    roaring_bitmap_flip_inplace(res1->rb, start, end);
    enif_mutex_unlock(lock);
    return argv[0];
  }
  else {
    roaring_bitmap_t *rb = roaring_bitmap_flip(res1->rb, start, end);
    return rb_make_resource(env, rb, false);
  }
}

static ERL_NIF_TERM rb_add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;
  uint32_t n;

  if(!(argc == 2 &&
      enif_get_uint(env, argv[0], &n) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    roaring_bitmap_add(res1->rb, n);
    enif_mutex_unlock(lock);
    return argv[1];
  }
  else {
    roaring_bitmap_t *rb = roaring_bitmap_copy(res1->rb);
    if(!rb) return enif_make_badarg(env);
    roaring_bitmap_add(rb, n);
    return rb_make_resource(env, rb, false);
  }
}

static ERL_NIF_TERM rb_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;
  uint32_t n;

  if(!(argc == 2 &&
      enif_get_uint(env, argv[0], &n) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    roaring_bitmap_remove(res1->rb, n);
    enif_mutex_unlock(lock);
    return argv[1];
  }
  else {
    roaring_bitmap_t *rb = roaring_bitmap_copy(res1->rb);
    if(!rb) return enif_make_badarg(env);
    roaring_bitmap_remove(rb, n);
    return rb_make_resource(env, rb, false);
  }
}

static ERL_NIF_TERM rb_is_member(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;
  uint32_t n;

  if(!(argc == 2 &&
      enif_get_uint(env, argv[0], &n) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  bool member;
  if(res1->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    member = roaring_bitmap_contains(res1->rb, n);
    enif_mutex_unlock(lock);
  }
  else {
    member = roaring_bitmap_contains(res1->rb, n);
  }

  if(member) return ATOM_TRUE;
  else return ATOM_FALSE;
}

static ERL_NIF_TERM rb_is_subset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  bool subset;
  if(res1->mutable || res2->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    subset = roaring_bitmap_is_subset(res1->rb, res2->rb);
    enif_mutex_unlock(lock);
  }
  else {
    subset = roaring_bitmap_is_subset(res1->rb, res2->rb);
  }

  if(subset) return ATOM_TRUE;
  else return ATOM_FALSE;
}

static ERL_NIF_TERM rb_is_strict_subset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  bool subset;
  if(res1->mutable || res2->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    subset = roaring_bitmap_is_strict_subset(res1->rb, res2->rb);
    enif_mutex_unlock(lock);
  }
  else {
    subset = roaring_bitmap_is_strict_subset(res1->rb, res2->rb);
  }

  if(subset) return ATOM_TRUE;
  else return ATOM_FALSE;
}

static ERL_NIF_TERM rb_equals(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1, *res2;

  if(!(argc == 2 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1) &&
      enif_get_resource(env, argv[1], rb_res_type, (void**)&res2))) {
    return enif_make_badarg(env);
  }

  bool equals;
  if(res1->mutable || res2->mutable) {
    ErlNifMutex *lock = rb_global_lock(env);
    equals = roaring_bitmap_equals(res1->rb, res2->rb);
    enif_mutex_unlock(lock);
  }
  else {
    equals = roaring_bitmap_equals(res1->rb, res2->rb);
  }

  if(equals) return ATOM_TRUE;
  else return ATOM_FALSE;
}

static ERL_NIF_TERM rb_iterator(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  rb_res *res1;
  it_res *res;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], rb_res_type, (void**)&res1))) {
    return enif_make_badarg(env);
  }

  res = (it_res *)enif_alloc_resource(it_res_type, sizeof(it_res));
  if(!res) return enif_make_badarg(env);

  if(res1->mutable)
    res->lock = rb_get_global_lock(env);
  else {
    res->lock = enif_mutex_create("it_lock");
    if(!res->lock) return enif_make_badarg(env);
  }

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

  enif_mutex_lock(res->lock);
  ERL_NIF_TERM ret;
  if(!res->it) {
    res->it = roaring_create_iterator(res->rbres->rb);
    if(!res->it) return enif_make_badarg(env);
    ret = enif_make_uint(env, res->it->current_value);
  }
  else if(roaring_advance_uint32_iterator(res->it))
    ret = enif_make_uint(env, res->it->current_value);
  else
    ret = ATOM_UNDEFINED;

  enif_mutex_unlock(res->lock);
  return ret;
}

static ERL_NIF_TERM rb_move(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  it_res *res;
  uint32_t n;

  if(!(argc == 2 &&
      enif_get_uint(env, argv[0], &n) &&
      enif_get_resource(env, argv[1], it_res_type, (void**)&res))) {
    return enif_make_badarg(env);
  }

  enif_mutex_lock(res->lock);
  if(roaring_move_uint32_iterator_equalorlarger(res->it, n)) {
    ERL_NIF_TERM ret = enif_make_uint(env, res->it->current_value);
    enif_mutex_unlock(res->lock);
    return ret;
  }
  else
    return ATOM_UNDEFINED;
}

static ERL_NIF_TERM rb_reset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  it_res *res;

  if(!(argc == 1 &&
      enif_get_resource(env, argv[0], it_res_type, (void**)&res))) {
    return enif_make_badarg(env);
  }

  enif_mutex_lock(res->lock);
  if(res->it) {
    roaring_free_uint32_iterator(res->it);
    res->it = NULL;
  }
  enif_mutex_unlock(res->lock);
  return argv[0];
}

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

  if(!res->rbres->mutable && res->lock)
    enif_mutex_destroy(res->lock);

  enif_release_resource(res->rbres);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  __UNUSED(load_info);

  rb_priv_data *priv = (rb_priv_data *)enif_alloc(sizeof(rb_priv_data));
  priv->lock = enif_mutex_create("rb_global_lock");
  if(!priv->lock) return -1;

  *priv_data = priv;

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

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv, ERL_NIF_TERM load_info) {
  __UNUSED(env);
  __UNUSED(priv_data);
  __UNUSED(old_priv);
  __UNUSED(load_info);

  return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {
  __UNUSED(env);
  rb_priv_data *priv = (rb_priv_data *)priv_data;

  if(priv->lock)
    enif_mutex_destroy(priv->lock);

  enif_free(priv);
}


static ErlNifFunc nif_funcs[] =
{
  {"new", 0, rb_new},
  {"new_mutable", 0, rb_new_mutable},
  {"from_list", 1, rb_from_list},
  {"from_range", 3, rb_from_range},
  {"from_mutable", 1, rb_from_mutable},
  {"set_immutable", 1, rb_set_immutable},
  {"from_binary", 1, rb_from_binary},
  {"to_binary", 1, rb_to_binary},
  {"to_list", 1, rb_to_list},
  {"to_mutable", 1, rb_to_mutable},
  {"is_mutable", 1, rb_is_mutable},
  {"is_empty", 1, rb_is_empty},
  {"size", 1, rb_size},
  {"intersection", 2, rb_intersection},
  {"union", 2, rb_union},
  {"sym_difference", 2, rb_sym_difference},
  {"difference", 2, rb_difference},
  {"flip", 3, rb_flip},
  {"add", 2, rb_add},
  {"delete", 2, rb_delete},
  {"is_member", 2, rb_is_member},
  {"is_subset", 2, rb_is_subset},
  {"is_strict_subset", 2, rb_is_strict_subset},
  {"equals", 2, rb_equals},
  {"iterator", 1, rb_iterator},
  {"next", 1, rb_next},
  {"move", 2, rb_move},
  {"reset", 1, rb_reset},
};

ERL_NIF_INIT(rb_sets, nif_funcs, &load, NULL, &upgrade, &unload);
