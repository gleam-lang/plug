-module(gleam@http@plug).
-compile(no_auto_import).

-export([port/1, host/1, scheme/1, method/1, request_path/1, req_headers/1, query_string/1, conn_to_request/2, send/2, halt/1]).

port(A) ->
    'Elixir.GleamPlug':port(A).

host(A) ->
    'Elixir.GleamPlug':host(A).

scheme(A) ->
    'Elixir.GleamPlug':scheme(A).

method(Conn) ->
    gleam@result:unwrap(
        gleam@http:method_from_dynamic('Elixir.GleamPlug':method(Conn)),
        get
    ).

request_path(A) ->
    'Elixir.GleamPlug':request_path(A).

req_headers(A) ->
    'Elixir.GleamPlug':req_headers(A).

query_string(Conn) ->
    case 'Elixir.GleamPlug':query_string(Conn) of
        <<""/utf8>> ->
            none;

        Q ->
            {some, Q}
    end.

conn_to_request(Conn, Body) ->
    {request,
     method(Conn),
     'Elixir.GleamPlug':req_headers(Conn),
     Body,
     'Elixir.GleamPlug':scheme(Conn),
     'Elixir.GleamPlug':host(Conn),
     {some, 'Elixir.GleamPlug':port(Conn)},
     'Elixir.GleamPlug':request_path(Conn),
     query_string(Conn)}.

send(Response, Conn) ->
    'Elixir.Plug.Conn':send_resp(
        'Elixir.Plug.Conn':merge_resp_headers(Conn, erlang:element(3, Response)),
        erlang:element(2, Response),
        erlang:element(4, Response)
    ).

halt(A) ->
    'Elixir.Plug.Conn':halt(A).
