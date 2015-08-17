-module(walrus).

-export([
	make/1,
	render/2
]).

%% ============================================================================
% Template API
%% ============================================================================
 
% @spec make(TemplateString) -> syntaxTree()
% @doc Compile a view
make(TemplateString) ->
	walrus_compiler:make(TemplateString).

% @spec render(Token, Arguments) -> 
% @doc render view token
render(Template, Arguments) ->
	walrus_printer:render(Template, Arguments).
