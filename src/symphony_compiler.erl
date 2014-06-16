-module(symphony_compiler).

-export([
	scan/2
]).

%% ----------------------------------------------------------------------------
% @spec scan(string(), string()) -> erlsyntax()
% @doc Scan and parse template into erlang syntax form
%% ----------------------------------------------------------------------------
scan(Template, Filename) ->
	scan(Template, Filename,{1,1}, [{text,[]}],[], text).

%% End of file
scan([], _Filename,{_Row,_Column}, Tokens, _Errors, _Type) ->
	%%TODO: print errors
	[CurrentToken | Rest] = Tokens,
	lists:reverse([finalize(CurrentToken) | Rest]);

%% Text special characters
scan("\n" ++ Rest, Filename,{Row,_Column}, Tokens,Errors, text) ->
	scan(Rest, Filename,{Row+1,1}, add("\n",Tokens),Errors, text);

%% Tag special characters
%% TODO: Find misplaced start and end tag token errors
scan("<!" ++ Rest, Filename,{Row,Column}, Tokens,Errors, text) ->
	scan(Rest, Filename,{Row,Column+2}, create(tag,Tokens),Errors, tag);
scan("!>" ++ Rest, Filename,{Row,Column}, Tokens,Errors, tag) ->
	scan(Rest, Filename,{Row,Column+2}, create(text,Tokens),Errors, text);

scan(" " ++ Rest, Filename,{Row,Column}, Tokens,Errors, tag) ->
	scan(Rest, Filename,{Row,Column+1}, Tokens,Errors, tag);
scan("\t" ++ Rest, Filename,{Row,Column}, Tokens,Errors, tag) ->
	scan(Rest, Filename,{Row,Column+1}, Tokens,Errors, tag);
scan("\n" ++ Rest, Filename,{Row,_Column}, Tokens,Errors, tag) ->
	scan(Rest, Filename,{Row+1,1}, Tokens,Errors, tag);

%% Add character
scan([Character | Rest], Filename,{Row,Column}, Tokens,Errors, Type) ->
	%% TODO: Allow only alphanumerical in tags
	%% TODO: Find tag-name errors
	scan(Rest, Filename,{Row,Column+1}, add(Character,Tokens),Errors, Type).

%% ----------------------------------------------------------------------------
% @spec create(atom, List) -> Tokens
% @doc Create new token
%% ----------------------------------------------------------------------------
create(Type, Tokens) ->
	[Token | Rest] = Tokens,
	[{Type,[]}, finalize(Token) | Rest].

%% ----------------------------------------------------------------------------
% @spec add(List::string(), List) -> Tokens
% @doc Add new character to token
%% ----------------------------------------------------------------------------
add(Character, Tokens) ->
	[{Type, String} | Rest] = Tokens,
	[{Type, [Character | String]} | Rest].

%% ----------------------------------------------------------------------------
% @spec finalize(atom, string()) -> Token
% @doc Order the reverse token content string
%% ----------------------------------------------------------------------------	
finalize({Type, String}) ->
	case Type of
		text ->
			%% Convert the token to a section of text
			erl_syntax:string(lists:flatten(lists:reverse(String)));
		tag ->
			%% Convert the token to a tag
			[First | Rest] = lists:flatten(lists:reverse(String)),
			erl_syntax:variable([string:to_upper(First) | Rest])
	end.
	
