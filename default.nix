{ mkDerivation, attoparsec, base, bytestring, case-conversion
, data-default, exceptions, heredoc, hspec, postgresql-simple
, stdenv, template-haskell, text, time
}:
mkDerivation {
  pname = "postgresql-simple-bind";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring data-default exceptions heredoc
    postgresql-simple template-haskell text time
  ];
  testHaskellDepends = [
    attoparsec base bytestring case-conversion data-default hspec
    postgresql-simple text
  ];
  description = "A FFI-like bindings for PostgreSQL stored functions";
  license = stdenv.lib.licenses.bsd3;
}
