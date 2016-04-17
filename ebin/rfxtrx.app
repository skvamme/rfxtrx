{application, rfxtrx, [
	{description, "New project"},
	{vsn, "0.0.1"},
	{modules, ['rfxtrx','rfxtrx_app','rfxtrx_sup']},
	{registered, [rfxtrx_sup]},
	{applications, [kernel,stdlib]},
	{mod, {rfxtrx_app, []}}
]}.