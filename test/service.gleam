import gleam/http
import gleam/bit_builder.{BitBuilder}

pub fn handle(req: http.Request(BitBuilder)) {
  http.response(200)
  |> http.prepend_resp_header("made-with", "Gleam")
  |> http.set_resp_body(req.body)
}
