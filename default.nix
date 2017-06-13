{ mkDerivation, attoparsec, base, bytestring, data-default
, directory, exceptions, filepath, heredoc, hspec
, postgresql-simple, safe, stdenv, template-haskell, text, time
, transformers
}:
mkDerivation {
  pname = "postgresql-simple-bind";
  version = "0.4.1";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring data-default directory exceptions
    filepath heredoc postgresql-simple safe template-haskell text time
    transformers
  ];
  testHaskellDepends = [
    attoparsec base bytestring data-default exceptions heredoc hspec
    postgresql-simple text
  ];
  description = "FFI-like bindings for PostgreSQL stored functions";
  license = stdenv.lib.licenses.bsd3;
}
