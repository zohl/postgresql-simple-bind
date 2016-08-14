{ mkDerivation, attoparsec, base, bytestring, case-conversion
, heredoc, HUnit, postgresql-simple, stdenv, template-haskell, text
, time
}:
mkDerivation {
  pname = "postgresql-simple-bind";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring heredoc postgresql-simple
    template-haskell text time
  ];
  testHaskellDepends = [
    attoparsec base bytestring case-conversion HUnit postgresql-simple
    text
  ];
  description = "A FFI-like bindings for PostgreSQL stored functions";
  license = stdenv.lib.licenses.gpl3;
}
