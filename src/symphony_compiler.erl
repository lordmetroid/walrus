-module(symphony_compiler).

-export([
	scan/1
]).

%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
scan(Template) ->
	scan(Template, {1,1}, [{text,{1,1},[]}], text).

scan([], _, Tokens, text) ->
	lists:reverse(finalize(Tokens));


%% Text
scan("<%" ++ Rest, {Row,Column}, Tokens, text) ->
	scan(Rest, {Row,Column+2}, Tokens, start);

scan("\n" ++ Rest, {Row,_Column}, Tokens, text) ->
	scan(Rest, {Row+1,1}, append("\n",Tokens), text);

scan([Character | Rest], {Row,Column}, Tokens, text) ->
	scan(Rest, {Row,Column+1}, append(Character,Tokens), text);


%% Open tag
scan(" " ++ Rest, {Row,Column}, Tokens, start) ->
	scan(Rest, {Row,Column+1}, Tokens, start);

scan("\t" ++ Rest, {Row,Column}, Tokens, start) ->
	scan(Rest, {Row,Column+1}, Tokens, start);

scan("\n" ++ Rest, {Row,_Column}, Tokens, start) ->
	scan(Rest, {Row+1,1}, Tokens, start);

%% Create new variable
scan([Character | Rest], {Row,Column}, Tokens, start) ->
	UpdatedTokens = add({variable,{Row,Column},[]}, Tokens),
	scan(Rest, {Row,Column+1}, append(Character,UpdatedTokens), variable);

%% Variable

scan(" " ++ Rest, {Row,Column}, Tokens, variable) ->
	scan(Rest, {Row,Column+1}, Tokens, stop);

scan("\t" ++ Rest, {Row,Column}, Tokens, variable) ->
	scan(Rest, {Row,Column+1}, Tokens, stop);

scan("\n" ++ Rest, {Row,_Column}, Tokens, variable) ->
	scan(Rest, {Row+1,1}, Tokens, stop);

scan("%>" ++ Rest, {Row,Column}, Tokens, variable) ->
	UpdatedTokens = add({text,{Row,Column+2},[]}, Tokens),
	scan(Rest, {Row,Column+2}, UpdatedTokens, text);

scan([Character | Rest], {Row,Column}, Tokens, variable) ->
	scan(Rest, {Row,Column+1}, append(Character,Tokens), variable);

%% Stop tag
scan("%>" ++ Rest, {Row,Column}, Tokens, stop) ->
	UpdatedTokens = add({text,{Row,Column+2},[]}, Tokens),
	scan(Rest, {Row,Column+2}, UpdatedTokens, text).
	



%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
add(NewToken, Tokens) ->
	[NewToken | finalize(Tokens)].

%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
append(Character, [{Type,Position,Content} | Rest]) ->
	[{Type,Position,[Character | Content]} | Rest].

%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
finalize([{Type,Position,Content} | Rest]) ->
	[{Type,Position,lists:reverse(Content)} | Rest].

