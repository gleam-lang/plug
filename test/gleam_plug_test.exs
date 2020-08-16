defmodule GleamPlugTest do
  use ExUnit.Case
  doctest GleamPlug
  import Plug.Test

  test "service test" do
    conn = conn(:get, "/hello")

    conn
    |> GleamPlug.conn_to_request("hello")
    |> :service.handle()
    |> GleamPlug.send(conn)

    assert sent_resp(conn) ==
             {200,
              [{"cache-control", "max-age=0, private, must-revalidate"}, {"made-with", "Gleam"}],
              "hello"}
  end
end
