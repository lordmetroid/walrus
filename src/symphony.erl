-module(symphony).

-export([
	make/1,
	render/2
]).

%% ----------------------------------------------------------------------------
% Template compilation API
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% @spec make(TemplateString) -> syntaxTree()
% @doc Compile a view file
%% ----------------------------------------------------------------------------
make(TemplateString) ->
	symphony_compiler:make(TemplateString)

%% ----------------------------------------------------------------------------
% @spec render(Token, Arguments) -> 
% @doc Compile a view file
%% ----------------------------------------------------------------------------
render({Type, String}, Arguments) ->
	case Type of
		text ->
			%% Template token is a text string
			{ok, String};
		variable ->
			%% Template token is a variable
			case lists:keyfind(String, 1, Arguments) of
				false ->
					%% Variable value not provided
					{error, "No value for variable " ++ String ++ " provided"};
				{String, Value} ->
					%% Return variable value
					{ok, Value}
			end
	end.
