{ pkgs, lib, config, inputs, ... }:

{
  packages = [
    pkgs.dune
  ];

  languages.ocaml = {
    enable = true;
    lsp.enable = true;
  };
}
