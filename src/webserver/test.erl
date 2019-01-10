-module(test).

-export([get/0]).

get() ->
    jiffy:encode({[
		{<<"OMG">>, <<"YES">>}
	]}).
