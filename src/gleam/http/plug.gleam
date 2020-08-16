import gleam/http
import gleam/result
import gleam/option.{None, Option, Some}
import gleam/dynamic.{Dynamic}
import gleam/bit_builder.{BitBuilder}

pub external type Conn

pub external fn port(Conn) -> Int =
  "Elixir.GleamPlug" "port"

pub external fn host(Conn) -> String =
  "Elixir.GleamPlug" "host"

pub external fn scheme(Conn) -> http.Scheme =
  "Elixir.GleamPlug" "scheme"

external fn elixir_method(Conn) -> Dynamic =
  "Elixir.GleamPlug" "method"

pub fn method(conn: Conn) -> http.Method {
  conn
  |> elixir_method
  |> http.method_from_dynamic
  |> result.unwrap(http.Get)
}

pub external fn request_path(Conn) -> String =
  "Elixir.GleamPlug" "request_path"

pub external fn req_headers(Conn) -> List(http.Header) =
  "Elixir.GleamPlug" "req_headers"

external fn elixir_query_string(Conn) -> String =
  "Elixir.GleamPlug" "query_string"

pub fn query_string(conn: Conn) -> Option(String) {
  case elixir_query_string(conn) {
    "" -> None
    q -> Some(q)
  }
}

/// Convert a Plug connection to a Gleam HTTP request which can be
/// used to call a Gleam HTTP service.
///
/// It is common Plug applications to extract and decode the request
/// body using a middleware so this function does not attempt to read
/// the body directly from the conn, instead it must be given as the
/// second argument.
///
pub fn conn_to_request(conn: Conn, body: a) -> http.Request(a) {
  http.Request(
    body: body,
    headers: req_headers(conn),
    host: host(conn),
    path: request_path(conn),
    method: method(conn),
    port: Some(port(conn)),
    query: query_string(conn),
    scheme: scheme(conn),
  )
}

external fn send_resp(conn: Conn, status: Int, body: BitBuilder) -> Conn =
  "Elixir.Plug.Conn" "send_resp"

external fn merge_resp_headers(conn: Conn, headers: List(http.Header)) -> Conn =
  "Elixir.Plug.Conn" "merge_resp_headers"

/// Send a Gleam HTTP response over the Plug connection.
///
/// Note that this function does not halt the connection, so if subsequent
/// plugs try to send another response, it will error out. Use the `halt`
/// function after this function if you want to halt the plug pipeline.
///
pub fn send(response: http.Response(BitBuilder), conn: Conn) -> Conn {
  conn
  |> merge_resp_headers(response.headers)
  |> send_resp(response.status, response.body)
}

/// Halts the Plug pipeline by preventing further plugs downstream from being
/// invoked. See the docs for Plug.Builder for more information on halting a
/// Plug pipeline.
///
pub external fn halt(Conn) -> Conn =
  "Elixir.Plug.Conn" "halt"
