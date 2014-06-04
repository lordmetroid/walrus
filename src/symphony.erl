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
	%%TODO: Detect encoding of file
	%%TODO: Convert file to utf8
	compile_file(Filename,utf8).

compile_file(Filename,Encoding) ->
	case file:read_file(Filename) of
		{error, Reason} ->
			{error, Reason};
		{ok, Binary} ->
			Template = unicode:characters_to_list(Binary,Encoding),
			compile(Template, Filename)
	end.

%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
compile(Template) ->
	compile(Template,"list").

compile(Template, Filename) when is_list(Template) ->
	symphony_compiler:scan(Template, Filename).

