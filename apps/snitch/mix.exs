defmodule Snitch.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :snitch,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.5",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  defp deps() do
    [
      {:banker, in_umbrella: true, manager: :rebar3},
      {:prospector, in_umbrella: true, manager: :rebar3},
      {:sheriff, in_umbrella: true, manager: :rebar3}
    ]
  end
end
