-module(symphony_compiler).

-export([
	scan/2
]).

%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
scan(Template, Encoding) when is_binary(Template) ->
	scan(unicode:characters_to_list(Template, Encoding), Encoding);
scan(Template, Encoding) when is_list(Template) ->
	ok.
