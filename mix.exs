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
      description: "A Gleam HTTP service adapter for the Plug web application interface",
      package: [
        licenses: ["Apache-2.0"],
        links: %{github: "https://github.com/gleam-lang/plug"}
      ],
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
      {:plug, "~> 1.10"},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false}
    ]
  end
end
