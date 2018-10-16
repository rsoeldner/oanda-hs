{ mkDerivation, aeson, base, bytestring, http-client-tls, mtl
, servant, servant-client, servant-client-core, stdenv, text
}:
mkDerivation {
  pname = "oanda";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring http-client-tls mtl servant servant-client
    servant-client-core text
  ];
  license = stdenv.lib.licenses.bsd3;
}
