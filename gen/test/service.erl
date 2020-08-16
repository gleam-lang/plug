-module(service).
-compile(no_auto_import).

-export([handle/1]).

handle(Req) ->
    gleam@http:set_resp_body(
        gleam@http:prepend_resp_header(
            gleam@http:response(200),
            <<"made-with"/utf8>>,
            <<"Gleam"/utf8>>
        ),
        erlang:element(4, Req)
    ).
