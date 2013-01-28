-module(symphony).

-export([
	compile/1,
	compile/2,
	compile_file/1,
	compule_file/2
]).


%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
compile(Template) ->
	compile(Template, utf8);
compile(Template, Encoding) ->
	symphony_parser:scan(Template, Encoding).

%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
compile_file(Filename) ->
	compile_file(Filename, utf8);
compile_file(Filename, Encoding) ->
	case file:read_file(Filename) of
		{ok, Template} ->
			compile(Template, Encoding);
		{error, Reason} ->
			{error, Reason}
	end.

