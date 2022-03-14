%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2022, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  11 Mar 2022 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(fastrandom_nif).

%% NIF API
-export([
	seed/1,
	xoroshiro116p_next/0,
	xoroshiro116p_next/1,
	xoshiro256p_next/0,
	xoshiro256p_next/1,
	xoshiro256px8_next/0,
	xoshiro256px8_next/1,
	xoshiro256pp_next/0,
	xoshiro256pp_next/1,
	xoshiro256ppx8_next/0,
	xoshiro256ppx8_next/1
]).

-on_load(init/0).

%%%===================================================================
%%% NIF API functions
%%%===================================================================

seed(_Seed) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

xoroshiro116p_next() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

xoroshiro116p_next(_Range) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

xoshiro256p_next() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

xoshiro256p_next(_Range) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

xoshiro256px8_next() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

xoshiro256px8_next(_Range) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

xoshiro256pp_next() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

xoshiro256pp_next(_Range) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

xoshiro256ppx8_next() ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

xoshiro256ppx8_next(_Range) ->
	erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
init() ->
	SoName = filename:join(fastrandom:priv_dir(), ?MODULE_STRING),
	erlang:load_nif(SoName, 0).
