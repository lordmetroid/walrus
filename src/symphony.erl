-module(symphony).

-export([
	make_file/1,
	make_file/2,
	make/1
]).

%% ----------------------------------------------------------------------------
% Template compilation API
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% @spec compile_file(FilePath) -> syntaxTree()
% @doc Compile a view file
%% ----------------------------------------------------------------------------
make_file(FilePath) ->
	%%TODO: Detect encoding of file
	%%TODO: Convert string to utf8 if necessary
	make_file(FilePath,utf8).

make_file(FilePath,Encoding) ->
	case file:read_file(FilePath) of
		{error, Reason} ->
			{error, Reason};
		{ok, Binary} ->
			Template = unicode:characters_to_list(Binary,Encoding),
			make(Template, FilePath)
	end.

%% ----------------------------------------------------------------------------
% @spec compile(Template) -> syntaxTree()
% @doc Compile template string
%% ----------------------------------------------------------------------------
make(Template) ->
	%% Compile without a filepath
	compile(Template, []).

make(Template, FilePath) when is_list(Template) ->
	%% TODO: Add render() functionality
	symphony_compiler:scan(Template, FilePath).
