-module(symphony).

-export([
	make/1,
	make/2,
	make_template/1
]).

%% ----------------------------------------------------------------------------
% Template compilation API
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% @spec compile_file(FilePath) -> syntaxTree()
% @doc Compile a view file
%% ----------------------------------------------------------------------------
make(FilePath) ->
	%%TODO: Detect encoding of file
	%%TODO: Convert string to utf8 if necessary
	make(FilePath,utf8).

make(FilePath,Encoding) ->
	case file:read_file(FilePath) of
		{error, Error} ->
			{error, Error};
		{ok, Binary} ->
			%% Convert binary to string
			String = unicode:characters_to_list(Binary,Encoding),
			{ok, make_template(String, FilePath)}
	end.

%% ----------------------------------------------------------------------------
% @spec compile(Template) -> syntaxTree()
% @doc Compile template string
%% ----------------------------------------------------------------------------
make_template(Template) ->
	%% Compile without a filepath
	make_template(Template, []).

make_template(Template, FilePath) when is_list(Template) ->
	%% TODO: Add render() functionality
	symphony_compiler:scan(Template, FilePath).
