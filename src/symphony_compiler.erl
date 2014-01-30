-module(symphony_compiler).

-export([
	scan/2
]).

%% ----------------------------------------------------------------------------
% @spec scan(List::string(), List::string()) -> Tokens::List
% @doc Scan and parse template
%% ----------------------------------------------------------------------------
scan(Template, Filename) ->
	scan(Template, Filename,{1,1}, [{text,[]}],[], text).

%% Text
scan("\n" ++ Rest, Filename,{Row,_Column}, Tokens,Errors, text) ->
	scan(Rest, Filename,{Row+1,1}, add("\n",Tokens),Errors, text);
scan([Character | Rest], Filename,{Row,Column}, Tokens,Errors, text) ->
	scan(Rest, Filename,{Row,Column+1}, add(Character,Tokens),Errors, text);

%% Start of tag
scan("<!" ++ Rest, Filename,{Row,Column}, Tokens,Errors, text) ->
	scan(Rest, Filename,{Row,Column+2}, create(tag,Tokens),Errors, start_tag);

%% Spacers in start of tag
scan(" " ++ Rest, Filename,{Row,Column}, Tokens,Errors, start_tag) ->
	scan(Rest, Filename,{Row,Column+1}, Tokens,Errors, start_tag);
scan("\t" ++ Rest, Filename,{Row,Column}, Tokens,Errors, start_tag) ->
	scan(Rest, Filename,{Row,Column+1}, Tokens,Errors, start_tag);
scan("\n" ++ Rest, Filename,{Row,_Column}, Tokens,Errors, start_tag) ->
	scan(Rest, Filename,{Row+1,1}, Tokens,Errors, start_tag);

%% Spacers at end of variable name
scan(" " ++ Rest, Filename,{Row,Column}, Tokens,Errors, variable) ->
	scan(Rest, Filename,{Row,Column+1}, Tokens,Errors, end_tag);
scan("\t" ++ Rest, Filename,{Row,Column}, Tokens,Errors, variable) ->
	scan(Rest, Filename,{Row,Column+1}, Tokens,Errors, end_tag);
scan("\n" ++ Rest, Filename,{Row,_Column}, Tokens,Errors, variable) ->
	scan(Rest, Filename,{Row+1,1}, Tokens,Errors, end_tag);
scan(" " ++ Rest, Filename,{Row,Column}, Tokens,Errors, end_tag) ->
	scan(Rest, Filename,{Row,Column}, Tokens,Errors, end_tag);
scan("\t" ++ Rest, Filename,{Row,Column}, Tokens,Errors, end_tag) ->
	scan(Rest, Filename,{Row,Column+1}, Tokens,Errors, end_tag);
scan("\n" ++ Rest, Filename,{Row,_Column}, Tokens,Errors, end_tag) ->
	scan(Rest, Filename,{Row+1,1}, Tokens,Errors, end_tag);

%% End of tag
scan("!>" ++ Rest, Filename,{Row,Column}, Tokens,Errors, end_tag) ->
	scan(Rest, Filename,{Row,Column+2}, create(text,Tokens),Errors, text);
scan("!>" ++ Rest, Filename,{Row,Column}, Tokens,Errors, variable) ->
	scan(Rest, Filename,{Row,Column+2}, create(text,Tokens),Errors, text);

%% End of file
scan([], _Filename,{_Row,_Column}, Tokens,_Errors, end_tag) ->
	%%TODO: print errors?
	[CurrentToken | Rest] = Tokens,
	lists:reverse([finalize(CurrentToken) | Rest]).
	
%% ----------------------------------------------------------------------------
% @spec create(atom, List) -> Tokens
% @doc Create new token
%% ----------------------------------------------------------------------------
create(Type, Tokens) ->
	[CurrentToken | Rest] = Tokens,
	[{Type,[]}, finalize(CurrentToken) | Rest].

%% ----------------------------------------------------------------------------
% @spec add(List::string(), List) -> Tokens
% @doc Add new character to token
%% ----------------------------------------------------------------------------
add(Character, Tokens) ->
	[{Type, String} | Rest] = Tokens,
	[{Type, [Character | String]}, Rest].

%% ----------------------------------------------------------------------------
% @spec finalize(atom, List) -> Token
% @doc Order the reverse token content string
%% ----------------------------------------------------------------------------	
finalize({Type, String}) ->
	 {Type, lists:reverse(String)}.
