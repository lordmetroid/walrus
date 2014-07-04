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
% @doc Compile a view
%% ----------------------------------------------------------------------------
make(TemplateString) ->
	symphony_compiler:make(TemplateString).

%% ----------------------------------------------------------------------------
% @spec render(Token, Arguments) -> 
% @doc render view token
%% ----------------------------------------------------------------------------
render(Template, Arguments) ->
	symphony_printer:render(Template, Arguments, [], []).
