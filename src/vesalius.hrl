-define(CONSOLE(Str, Args), io:format(Str, Args)).
-define(CONSOLE(Str), io:format(Str, [])).

-define(XREF_SERVER, ?MODULE).
