defmodule Snitch.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      # apps: [:snitch, :remittance],
      apps: [:snitch],
      version: "0.1.0",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  defp deps do
    [
    ]
  end
end
