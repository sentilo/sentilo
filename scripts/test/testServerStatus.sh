#!/bin/bash

#Config
# - token: it is the default token associated to sentilo-catalog application (@see ./mongodb/init_data.js)
##################################################################################################################
token="c956c302086a042dd0426b4e62652273e05a6ce74d0b77f8b5602e0811025066"
tomcatServer=http://127.0.0.1:8080
apiServer=http://127.0.0.1:8081


#Help
##################################################################################################################
function usage()
{
	echo
	echo "THIS SHELL SCRIPT CHECKS THIS SERVICES IN THIS ORDER: Tomcat, Psab, Redis, Mongo"
	echo
	echo "The response is in binary way 1 ok, 0 ko"
	echo "concatenating results for the services described above."
	echo "Use --verbose to see detailed response"
	echo
}

#Params
##################################################################################################################
verbose=false
while [ "$1" != "" ]; do
	PARAM=`echo $1 | awk -F= '{print $1}'`
	VALUE=`echo $1 | awk -F= '{print $2}'`
	case $PARAM in
	-h | --help)
		usage
		exit
		;;
	--verbose)
		verbose=true
		;;
	*)
		echo "ERROR: unknown parameter \"$PARAM\""
		usage
		exit 1
		;;
	esac
	shift
done
if $verbose; then echo "Tomcat check"; fi
wget --spider $tomcatServer/sentilo-catalog-web/ -o /tmp/servercheck.txt
res1=$(cat /tmp/servercheck.txt | grep "200 OK" | wc -l)
if $verbose; then echo $res1; fi
rm /tmp/servercheck.txt

if $verbose; then echo "Psab check"; fi
curl -I --silent --request GET  $apiServer/data/mock -o /tmp/servercheck.txt
res2=$(cat /tmp/servercheck.txt | grep "401 Unauthorized" | wc -l)
if $verbose; then echo $res2; fi
rm /tmp/servercheck.txt

if $verbose; then echo "Redis check"; fi
curl -I --silent --request GET --header "IDENTITY_KEY: $token" $apiServer/data/testApp_provider -o /tmp/servercheck.txt
res3=$(cat /tmp/servercheck.txt | grep "200 OK" | wc -l)
if $verbose; then echo $res3; fi
rm /tmp/servercheck.txt

if $verbose; then echo "Mongo check"; fi
curl -I --silent --request GET --header "IDENTITY_KEY: $token" $apiServer/catalog?type=xxxx -o /tmp/servercheck.txt
res4=$(cat /tmp/servercheck.txt | grep "200 OK" | wc -l)
if $verbose; then echo $res4; echo; fi
rm /tmp/servercheck.txt

echo "$res1$res2$res3$res4"
