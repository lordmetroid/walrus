-module(symphony_compiler).

-export([
	scan/1
]).

%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
scan(Template) ->
	scan(Template, {1,1}, [], text).

scan([], _, Tokens, text) ->
	%% Finalize last token

	lists:reverse(Tokens);


%% Text
scan("<%" ++ Rest, {Row,Column}, Tokens, text) ->
	scan(Rest, {Row,Column+2}, Tokens, start);

scan("\n" ++ Rest, {Row,Column}, Tokens, text) ->
	scan(Rest, {Row+1,1}, append(Tokens,{Row,Column},"\n"), text);

scan([Character | Rest], {Row,Column}, Tokens, text) ->
	scan(Rest, {Row,Column+1}, append(Tokens,{Row,Column},Character), text);


%% Open tag
scan(" " ++ Rest, {Row,Column}, Tokens, start) ->
	scan(Rest, {Row,Column+1}, Tokens, start);

scan("\t" ++ Rest, {Row,Column}, Tokens, start) ->
	scan(Rest, {Row,Column+1}, Tokens, start);

scan("\n" ++ Rest, {Row,_Column}, Tokens, start) ->
	scan(Rest, {Row+1,1}, Tokens, start);

scan([Character | Rest], {Row,Column}, Tokens, start) ->
	UpdatedTokens =  add({variable,{Row,Column},[Character]}, Tokens),
	scan(Rest, {Row,Column+1}, UpdatedTokens, variable).

%% Variable



%% ----------------------------------------------------------------------------
% @spec
% @doc
%% ----------------------------------------------------------------------------
append(
add(NewToken, Tokens) ->
	case Tokens of
		[] ->
			[NewToken];
		[{Type, Position, Content} | Rest] ->
			[NewToken, {Type,Position,lists:reverse(Content)} | Rest]
	end.

