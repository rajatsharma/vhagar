let
  # Pinned nixpkgs, deterministic. Last updated: 2/12/21.
  # pkgs = import (fetchTarball ("https://github.com/NixOS/nixpkgs/archive/a58a0b5098f0c2a389ee70eb69422a052982d990.tar.gz")) { };
  # Rolling updates, not deterministic.
  pkgs = import (fetchTarball ("channel:nixpkgs-unstable")) { };
  dbStart = pkgs.writeShellScriptBin "db:start"
    ''pg_ctl -o "--unix_socket_directories='$PWD'" start'';
  initDb = pkgs.writeShellScriptBin "db:init" "initdb -U rajatsharma -W --no-locale --encoding=UTF8";
  dbStop = pkgs.writeShellScriptBin "db:stop" "pg_ctl stop";
  dbCreate = pkgs.writeShellScriptBin "db:create"
    "createdb -U rajatsharma -h localhost kensai";
  dbCheck = pkgs.writeShellScriptBin "db:check"
    "pg_isready -d kensai -h localhost -p 5432 -U rajatsharma";
in
with pkgs;
mkShell {
  buildInputs = [
    cargo
    rustc
    rustfmt
    clippy
    pkg-config
    openssl.dev
    postgresql
    dbStart
    initDb
    dbStop
    dbCreate
    dbCheck
  ];

  # See https://discourse.nixos.org/t/rust-src-not-found-and-other-misadventures-of-developing-rust-on-nixos/11570/3?u=samuela.
  PKG_CONFIG_PATH = "${openssl.dev}/lib/pkgconfig";
  RUST_SRC_PATH = "${rust.packages.stable.rustPlatform.rustLibSrc}";

  shellHook =
    ''
      export PGDATA="$HOME/pgdata"
      export PATH="$HOME/.cargo/bin:$PATH"
    '';
}
