defmodule GleamPlugTest do
  use ExUnit.Case
  doctest GleamPlug
  import Plug.Test

  test "service test" do
    conn = conn(:patch, "/hello")

    conn
    |> GleamPlug.conn_to_request("hello")
    |> :service.handle()
    |> GleamPlug.send(conn)

    assert sent_resp(conn) ==
             {200,
              [
                {"cache-control", "max-age=0, private, must-revalidate"},
                {"got-host", "www.example.com"},
                {"got-port", "80"},
                {"got-scheme", "http"},
                {"got-method", "patch"},
                {"got-path", "/hello"},
                {"made-with", "Gleam"}
              ], "hello"}
  end
end
