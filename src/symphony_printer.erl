-module(symphony).

-export([
	render/4
]).

%% ----------------------------------------------------------------------------
% @spec render(Token, Arguments) -> {ok, [syntaxTree()]}
% @doc render view token
%% ----------------------------------------------------------------------------
render([], _Arguments, Content, Errors) ->
	%% Return rendered results
	case Errors of
		[] ->
			%% No error
			{ok, Content};
		Errors ->
			%% Return content and errors
			{error, Content, Errors}
	end;
render([{Type, String} | Rest], Arguments, Content, Errors) ->
	case Type of
		text ->
			%% Template token is a text string
			render(Rest, Arguments, [String | Content], Errors);
		variable ->
			%% Template token is a variable
			case lists:keyfind(String, 1, Arguments) of
				false ->
					%% Variable value not provided
					NewError = "No value for " ++ String ++ " provided",
					render(Rest, Arguments, Content, [NewError | Errors]);
				{String, Value} ->
					%% Return variable value
					render(Rest, Arguments, [Value | Content], Errors)
			end
	end;
render([Token | Rest], Arguments, Content, Errors) ->
	%% Can not render unrecognized token
	NewError = ["Unrecognized token ", Token],
	render(Rest, Arguments, Content, [NewError | Errors]).
