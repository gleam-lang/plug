import gleam/http
import gleam/int
import gleam/option.{None, Some}
import gleam/bit_builder.{BitBuilder}

pub fn handle(req: http.Request(BitBuilder)) {
  let port = case req.port {
    Some(p) -> int.to_string(p)
    None -> "<none>"
  }
  http.response(200)
  |> http.prepend_resp_header("made-with", "Gleam")
  |> http.prepend_resp_header("got-path", req.path)
  |> http.prepend_resp_header("got-method", http.method_to_string(req.method))
  |> http.prepend_resp_header("got-scheme", http.scheme_to_string(req.scheme))
  |> http.prepend_resp_header("got-port", port)
  |> http.prepend_resp_header("got-host", req.host)
  |> http.set_resp_body(req.body)
}
