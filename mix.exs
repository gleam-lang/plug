defmodule GleamPlug.MixProject do
  use Mix.Project

  def project do
    [
      app: :gleam_plug,
      version: "0.1.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      erlc_paths: ["src", "gen"],
      compilers: [:gleam | Mix.compilers()],
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:mix_gleam, "~> 0.1"},
      {:gleam_http, "~> 1.3"},
      {:plug, "~> 1.10"}
    ]
  end
end
