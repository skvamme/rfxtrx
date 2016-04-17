-module(rfxtrx_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	rfxtrx_sup:start_link().

stop(_State) ->
	ok.
