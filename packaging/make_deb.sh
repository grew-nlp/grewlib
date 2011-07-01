#! /bin/sh

VERSION=$1
INSTALL_DIR=$2
DATA_DIR=$3
DOC_DIR=$4

rm -rf packaging/libcaml-grew*.deb

mkdir -p packaging/deb/DEBIAN/
mkdir -p packaging/deb/tmp/libcaml-grew/

cp packaging/libcaml-grew-$VERSION.tar.gz packaging/deb/tmp/libcaml-grew/


size="0"

cp packaging/DEBIAN/control packaging/deb/DEBIAN/control
cp packaging/DEBIAN/postinst packaging/deb/DEBIAN/postinst
cp packaging/DEBIAN/postrm packaging/deb/DEBIAN/postrm

chmod 755 packaging/deb/DEBIAN/postrm
chmod 755 packaging/deb/DEBIAN/postinst


sed -i "s|@VERSION@|$VERSION|" packaging/deb/DEBIAN/control
sed -i "s|@SIZE@|$size|" packaging/deb/DEBIAN/control
sed -i "s|@VERSION@|$VERSION|" packaging/deb/DEBIAN/postinst

(cd packaging/ && dpkg-deb --build deb && cd -) || (echo "Fatal error : maybe the version number is null (make ... VERSION=...)" && exit 1)
mv packaging/deb.deb packaging/libcaml-grew-$VERSION.deb
rm -rf packaging/deb
