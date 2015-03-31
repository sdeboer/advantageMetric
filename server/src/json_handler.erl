-module(json_handler).

-export([
				 return_json/2,
				 construct_response/2
				]).

return_json(Json, Req) ->
	case cowboy_req:qs_val(<<"jsonp">>, Req) of
		{undefined, R2} -> 
			{Json, R2};
		{Fn, R2} ->
			B = [Fn, <<"(">>, Json, <<");">>],
			{B, R2}
	end.

construct_response(Json, Req) ->
	{Body, R1} = return_json(Json, Req),
	R4 = cowboy_req:set_resp_body(Body, R1),
	cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, R4).
