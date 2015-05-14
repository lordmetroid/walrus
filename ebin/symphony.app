{application, symphony, [
	{description, "Strict view template engine"},
	{vsn, "1"},

	{applications, [
		kernel,
		stdlib,
		conductor
	]},

	{modules, [
		symphony,
		symphony_compiler,
		symphony_printer
	]},
	{registered, [
	]}
]}.

