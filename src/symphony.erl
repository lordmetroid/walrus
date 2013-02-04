-module(symphony).

-export([
	compile_file/1,
	compile_file/2,
	compile/1,
	compile/2
]).



%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
compile_file(Filename) ->
	compile_file(Filename, utf8).
compile_file(Filename, Encoding) ->
	case file:read_file(Filename) of
		{ok, Template} ->
			compile(Template, Encoding);
		{error, Reason} ->
			{error, Reason}
	end.

%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
compile(Template) ->
	compile(Template, utf8).
compile(Template, Encoding) when is_binary(Template) ->
	symphony_compiler:scan(unicode:characters_to_list(Template, Encoding));
compile(Template, Encoding) when is_list(Template) ->
	symphony_compiler:scan(Template).

