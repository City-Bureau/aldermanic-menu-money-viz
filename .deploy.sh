#!/bin/bash

rm -f dist/*
npm run build
# Gzip encode files, then remove extension for upload
for f in dist/*.{map,js,css,csv,geojson}; do gzip -9 $f && mv $f.gz $f; done
aws s3 cp dist/ s3://city-bureau-graphics/menu-money/ --exclude "*.html" --recursive --content-encoding=gzip --acl=public-read
aws s3 cp dist/index.html s3://city-bureau-graphics/menu-money/ --acl=public-read
