#! /bin/sh

VERSION=$1

md5=`openssl md5 libcaml-grew-$VERSION.tar.gz | sed "s|MD5(libcaml-grew-$VERSION\.tar.gz)= ||"`
sha1=`openssl sha1 libcaml-grew-$VERSION.tar.gz | sed "s|SHA1(libcaml-grew-$VERSION\.tar.gz)= ||"`
rmd160=`openssl rmd160 libcaml-grew-$VERSION.tar.gz | sed "s|RIPEMD160(libcaml-grew-$VERSION\.tar.gz)= ||"`

cp default_portfile/Portfile.default Portfile

sed -i "s|@MD5_SUM@|$md5|" Portfile
sed -i "s|@SHA1_SUM@|$sha1|" Portfile
sed -i "s|@RMD160_SUM@|$rmd160|" Portfile
sed -i "s|@VERSION@|$VERSION|" Portfile

