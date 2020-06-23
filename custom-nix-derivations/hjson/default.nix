{ stdenv
, fetchFromGitHub
, python3Packages
}:

python3Packages.buildPythonPackage rec {
  pname = "hjson-py";
  version = "db1475e7ba453e17f5ca7829009c57a8ae780951";

  src = fetchFromGitHub {
    owner = "hjson";
    repo = "hjson-py";
    rev = version;
    sha256 = "16ywy6igpqsk1hw674mqb68mai908cbksz51mjf8nzh7l2hyqzvj";
  };
}
