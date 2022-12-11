#!/bin/bash

echo "==========================================================="
echo " Starting Sentilo Platform API v2.0.0 test "
echo "==========================================================="
echo ""

newman run ./postman/postman_collection.json -e ./postman/postman_environment.json --delay-request 5000 --reporters cli,json --reporter-json-export ./outputfile.json