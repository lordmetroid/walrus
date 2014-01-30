-module(symphony_compiler).

-export([
	scan/2
]).

%% ----------------------------------------------------------------------------
% @spec
% @doc Scan and parse template to token list
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
	scan(Rest, Filename,{Row,Column+2}, add(tag,Tokens),Errors, start_tag);

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
	scan(Rest, Filename,{Row,Column+2}, add(text,Tokens),Errors, text);
scan("!>" ++ Rest, Filename,{Row,Column}, Tokens,Errors, variable) ->
	scan(Rest, Filename,{Row,Column+2}, add(text,Tokens),Errors, text).

%% ----------------------------------------------------------------------------
% @spec
% @doc Add new tag token
%% ----------------------------------------------------------------------------
add(tag, Tokens) ->
	ok;

%% ----------------------------------------------------------------------------
% @spec
% @doc Add new text
%% ----------------------------------------------------------------------------	
add(text, Tokens) ->
	ok;

%% ----------------------------------------------------------------------------
% @spec
% @doc Add new character to token
%% ----------------------------------------------------------------------------
add(Character, Tokens) ->
	ok.


