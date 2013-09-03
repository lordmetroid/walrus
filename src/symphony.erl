-module(symphony).

-export([
	compile_file/1,
	compile_file/2,
	compile/1
]).


%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
compile_file(Filename) ->
	compile_file(Filename,utf8).

compile_file(Filename,Encoding) ->
	case file:read_file(Filename) of
		{ok, Binary} ->
			Template = unicode:characters_to_list(Binary,Encoding),
			compile(Template, Filename);

		{error, Reason} ->
			{error, Reason}
	end.

%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
compile(Template) ->
	compile(Template,"list").

compile(Template, Filename) when is_list(Template) ->
	symphony_compiler:scan(Template, Filename).

