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
-module(fastrandom).

%% API
-export([
	start/0
]).
%% Internal API
-export([priv_dir/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

start() ->
	application:ensure_all_started(?MODULE).

%%%===================================================================
%%% Internal API Functions
%%%===================================================================

-spec priv_dir() -> file:filename_all().
priv_dir() ->
	case code:priv_dir(?MODULE) of
		{error, bad_name} ->
			case code:which(?MODULE) of
				Filename when is_list(Filename) ->
					filename:join([filename:dirname(Filename), "../priv"]);
				_ ->
					"../priv"
			end;
		Dir ->
			Dir
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
