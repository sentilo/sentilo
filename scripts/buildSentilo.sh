#!/bin/bash

function not_m2_home () {
        echo ""
        echo "ERROR: your operating system's PATH environment variable does not include properly Maven."
        echo "PATH = $PATH"
        echo "Please add Maven to your system's PATH environment variable."
        echo ""
        ERROR_LEVEL="1"
}

function not_java_home () {
        echo ""
        echo "ERROR: your operating system's PATH environment variable does not include properly Java."
        echo "PATH = $PATH"
        echo "Please add Java to your system's PATH environment variable."
        echo ""
        ERROR_LEVEL="1"
}

function error() {
        cd $CURRENT_DIR;
        echo "Error code : ${ERROR_LEVEL}"
        echo ""
        exit $ERROR_LEVEL;
}

function build_and_copy_module_artifacts() {
	MODULE_PATH=$1
	MODULE_NAME=$2
	IS_WEB=$3
	let DEPLOY_STEP++;	
	cd $MODULE_PATH
	if [[  "$IS_WEB" == "T"  ]]; then
	    COMMAND="mvn clean package" 
	else
	    COMMAND="mvn clean package appassembler:assemble"
	fi
	
	echo ""
	echo "==========================================================="
	echo "Step 2.$DEPLOY_STEP: Building artifacts for sentilo module $MODULE_NAME  "
	echo "==========================================================="
	echo "This command will compile and build module artifacts via Maven:"
	echo "$COMMAND"
	echo ""	

	eval ${COMMAND}
	ERROR_LEVEL=$?
	if [ "$ERROR_LEVEL" == "1" ]; then error; fi
	
	#Finally copy both module artifacts and configuration file to deploy dir
	mkdir -p $SENTILO_DPL_DIR/$MODULE_NAME
	if [[  "$IS_WEB" == "T"  ]]; then
	    cp ./target/*.war $SENTILO_DPL_DIR/$MODULE_NAME
	else	    
		cp -r ./target/appassembler/{repo,bin} $SENTILO_DPL_DIR/$MODULE_NAME
	fi		
}


clear

cd `dirname $0`
CURRENT_DIR=`pwd`

DEPLOY_STEP=0

# SENTILO_DPL_DIR is the directory where artifacts and configuration files will be copied finally
SENTILO_DPL_DIR="$CURRENT_DIR/../../sentilo-deploy-artifacts"
# Create deploy dir if it not exist
mkdir -p $SENTILO_DPL_DIR
# Remove contents from previous execution
rm -rf $SENTILO_DPL_DIR/*

mkdir $SENTILO_DPL_DIR/conf

cd "$CURRENT_DIR"

ERROR_LEVEL="0"

# Step 0: Validate prerequisites: environment variable PATH is properly defined
if hash mvn 2>/dev/null; then
        echo "maven runs..."
else
        not_m2_home;
fi
if hash java 2>/dev/null; then
        echo "java runs.."
else
        not_java_home;
fi
if [ "$ERROR_LEVEL" == "1" ]; then error; fi
clear

# Step 1: Compile and build platform code.
cd "$CURRENT_DIR/.."
COMMAND="mvn clean install"
echo ""
echo "======================================="
echo "Step 1: Compile and build platform code"
echo "======================================="
echo "This command will compile and build platform code via Maven:"
echo ""
echo "$COMMAND"
echo ""
echo "Press enter when ready."

read

eval ${COMMAND}
ERROR_LEVEL=$?
if [ "$ERROR_LEVEL" == "1" ]; then error; fi

# Step 2: Run appassempler plugin into java-standalone modules for generate scripts for starts java processes.
# For more  information about plugin visit http://mojo.codehaus.org/appassembler/appassembler-maven-plugin/ 
build_and_copy_module_artifacts $CURRENT_DIR/../sentilo-platform/sentilo-platform-server sentilo-platform-server
build_and_copy_module_artifacts $CURRENT_DIR/../sentilo-agent-alert sentilo-agent-alert
build_and_copy_module_artifacts $CURRENT_DIR/../sentilo-agent-relational sentilo-agent-relational
build_and_copy_module_artifacts $CURRENT_DIR/../sentilo-agent-location-updater sentilo-agent-location-updater
build_and_copy_module_artifacts $CURRENT_DIR/../sentilo-agent-historian sentilo-agent-historian
build_and_copy_module_artifacts $CURRENT_DIR/../sentilo-agent-activity-monitor sentilo-agent-activity-monitor
build_and_copy_module_artifacts $CURRENT_DIR/../sentilo-agent-kafka sentilo-agent-kafka
build_and_copy_module_artifacts $CURRENT_DIR/../sentilo-agent-alert sentilo-agent-alert
build_and_copy_module_artifacts $CURRENT_DIR/../sentilo-agent-federation sentilo-agent-federation
build_and_copy_module_artifacts $CURRENT_DIR/../sentilo-agent-metrics-monitor sentilo-agent-metrics-monitor
build_and_copy_module_artifacts $CURRENT_DIR/../sentilo-catalog-web sentilo-catalog-web T

echo ""
echo "===================================================="
echo "Step 3: Copy configuration files to deploy directory"
echo "===================================================="
echo ""
cd ..
mvn clean
find ./sentilo-* -name \sentilo*.conf -exec cp {} $SENTILO_DPL_DIR/conf \;
echo "Configuration files copied to $SENTILO_DPL_DIR/conf"

cd $CURRENT_DIR;
echo ""
echo "=================================================="
echo "Now you're ready to install the Sentilo platform. "
echo "=================================================="
exit "$ERROR_LEVEL"

