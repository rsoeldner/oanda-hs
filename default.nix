{ mkDerivation, aeson, base, bytestring, http-client-tls, mtl
, servant, servant-client, servant-client-core, stdenv, text, time
}:
mkDerivation {
  pname = "oanda";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring http-client-tls mtl servant servant-client
    servant-client-core text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
