#!/usr/bin/env nix-build

{ pkgs ? import <nixpkgs> {} }:

{
  main = pkgs.haskellPackages.callPackage ./cabal.nix {};
}
