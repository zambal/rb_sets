-module(rb_sets).

-export([
  new/0,
  from_list/1,
  from_range/3,
  from_mutable/1,
  from_binary/1,
  to_binary/1,
  to_list/1,
  to_mutable/1,
  is_mutable/1,
  intersection/2,
  union/2,
  sym_difference/2,
  difference/2,
  add/2,
  delete/2,
  is_member/2,
  size/1,
  is_subset/2,
  is_strict_subset/2,
  equals/2,
  iterator/1,
  next/1,
  move/2,
  reset/1
]).

-on_load(init/0).

-define(NOT_LOADED, not_loaded(?LINE)).

new() -> ?NOT_LOADED.

from_list(_List) -> ?NOT_LOADED.

from_range(_Min, _Max, _Step) -> ?NOT_LOADED.

from_mutable(_BM) -> ?NOT_LOADED.

from_binary(_Bin) -> ?NOT_LOADED.

to_binary(_BM) -> ?NOT_LOADED.

to_list(_BM) -> ?NOT_LOADED.

to_mutable(_BM) -> ?NOT_LOADED.

is_mutable(_BM) -> ?NOT_LOADED.

intersection(_BM1, _BM2) -> ?NOT_LOADED.

union(_BM1, _BM2) -> ?NOT_LOADED.

sym_difference(_BM1, _BM2) -> ?NOT_LOADED.

difference(_BM1, _BM2) -> ?NOT_LOADED.

add(_N, _BM) -> ?NOT_LOADED.

delete(_N, _BM) -> ?NOT_LOADED.

is_member(_N, _BM) -> ?NOT_LOADED.

size(_BM) -> ?NOT_LOADED.

is_subset(_BM1, _BM2) -> ?NOT_LOADED.

is_strict_subset(_BM1, _BM2) -> ?NOT_LOADED.

equals(_BM1, _BM2) -> ?NOT_LOADED.

iterator(_BM) -> ?NOT_LOADED.

next(_It) -> ?NOT_LOADED.

move(_N, _It) -> ?NOT_LOADED.

reset(_It) -> ?NOT_LOADED.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    case erlang:load_nif(filename:join(PrivDir, "rb_sets"), 0) of
        ok ->                  ok;
        {error,{reload, _}} -> ok;
        Error ->               Error
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
