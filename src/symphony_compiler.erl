-module(symphony_compiler).

-export([
	make/1
]).

%% ----------------------------------------------------------------------------
% @spec make(string()) -> erl_syntax()
% @doc Scan and parse template into erlang syntax form
%% ----------------------------------------------------------------------------
make(TemplateString) ->
	make(TemplateString, {1,1}, [{text,[]}],[], text).

%% End of file
make([], {_Row,_Column}, Tokens, _Errors, _Type) ->
	%% TODO: print errors
	[CurrentToken | Rest] = Tokens,
	erl_syntax:list([finalize(CurrentToken) | Rest]);

%% Text special characters
make("\n" ++ Rest, {Row,_Column}, Tokens,Errors, text) ->
	make(Rest, {Row+1,1}, add("\n",Tokens),Errors, text);

%% Tag special characters
%% TODO: Find misplaced start and end variable token errors
make("<?" ++ Rest, {Row,Column}, Tokens,Errors, text) ->
	make(Rest, {Row,Column+2}, create(variable,Tokens),Errors, variable);
make("?>" ++ Rest, Row,Column}, Tokens,Errors, variable) ->
	make(Rest, {Row,Column+2}, create(text,Tokens),Errors, text);

make(" " ++ Rest, {Row,Column}, Tokens,Errors, variable) ->
	make(Rest, {Row,Column+1}, Tokens,Errors, variable);
make("\t" ++ Rest, {Row,Column}, Tokens,Errors, variable) ->
	make(Rest, {Row,Column+1}, Tokens,Errors, variable);
make("\n" ++ Rest, {Row,_Column}, Tokens,Errors, variable) ->
	make(Rest, {Row+1,1}, Tokens,Errors, variable);

%% Add character
make([Character | Rest], {Row,Column}, Tokens,Errors, Type) ->
	%% TODO: Allow only alphanumerical in variables
	%% TODO: Find variable-name errors
	make(Rest, {Row,Column+1}, add(Character,Tokens),Errors, Type).

%% ----------------------------------------------------------------------------
% @spec create(atom, list()) -> Tokens
% @doc Create new token
%% ----------------------------------------------------------------------------
create(Type, Tokens) ->
	[Token | Rest] = Tokens,
	[{Type,[]}, finalize(Token) | Rest].

%% ----------------------------------------------------------------------------
% @spec add(character(), list()) -> Tokens::list()
% @doc Add new character to token
%% ----------------------------------------------------------------------------
add(Character, Tokens) ->
	[{Type, String} | Rest] = Tokens,
	[{Type, [Character | String]} | Rest].

%% ----------------------------------------------------------------------------
% @spec finalize(atom, string()) -> erl_syntax()
% @doc Create a storable erlang syntax token
%% ----------------------------------------------------------------------------	
finalize({Type, String}) ->
	erl_syntax:tuple([
		erl_syntax:atom(Type),
		erl_syntax:string(lists:reverse(String))
	]).
