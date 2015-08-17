%% -*- mode: Erlang; -*-

{application, walrus, [
	{description, "Strict mustache view template engine"},
	{vsn, "1"},

	{applications, [
		kernel,
		stdlib,
		conductor
	]},

	{modules, [
		walrus,
		walrus_compiler,
		walrus_printer
	]},
	{registered, [
	]}
]}.

