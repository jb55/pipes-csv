{ mkDerivation, base, blaze-builder, bytestring, cassava, HUnit
, pipes, pipes-bytestring, stdenv, test-framework
, test-framework-hunit, unordered-containers, vector
}:
mkDerivation {
  pname = "pipes-csv";
  version = "1.4.0";
  src = ./.;
  buildDepends = [
    base blaze-builder bytestring cassava pipes unordered-containers
    vector
  ];
  testDepends = [
    base bytestring cassava HUnit pipes pipes-bytestring test-framework
    test-framework-hunit vector
  ];
  description = "Fast, streaming csv parser";
  license = stdenv.lib.licenses.mit;
}
