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


cd `dirname $0`
CURRENT_DIR=`pwd`
cd $CURRENT_DIR

ERROR_LEVEL="0"

clear

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
cd $CURRENT_DIR/..
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
cd $CURRENT_DIR/../sentilo-platform/sentilo-platform-server
COMMAND="mvn package appassembler:assemble -P dev"
echo ""
echo "==========================================================="
echo "Step 2.1: Generate script for start Sentilo PubSub server  "
echo "==========================================================="
echo "This command will compile and build platform code via Maven:"
echo "$COMMAND"
echo ""
echo "for ====== sentilo-platform process ======"
echo ""
echo "Press enter when ready."

read

eval ${COMMAND}
ERROR_LEVEL=$?
if [ "$ERROR_LEVEL" == "1" ]; then error; fi

cd $CURRENT_DIR/../sentilo-agent-alert;
echo ""
echo "========================================================="
echo "Step 2.2: Generate script to start Sentilo Alert agent   "
echo "========================================================="
echo "This command will compile and build platform code via Maven:"
echo "$COMMAND"
echo ""
echo "for ====== sentilo-agent-alert ======"
echo ""
echo "Press enter when ready."

read

eval ${COMMAND}
ERROR_LEVEL=$?
if [ "$ERROR_LEVEL" == "1" ]; then error; fi

cd "$CURRENT_DIR/../sentilo-agent-relational"
echo ""
echo "============================================================="
echo "Step 2.3: Generate script to start Sentilo Relational agent  "
echo "============================================================="
echo "This command will compile and build platform code via Maven:"
echo "$COMMAND"
echo ""
echo "for ====== sentilo-agent-relational ====== "
echo ""
echo "Press enter when ready."

read

eval ${COMMAND}
ERROR_LEVEL=$?
if [ "$ERROR_LEVEL" == "1" ]; then error; fi

cd $CURRENT_DIR/../sentilo-agent-location-updater;
echo ""
echo "==================================================================="
echo "Step 2.4: Generate scripts to start Sentilo Location Updater agent "
echo "==================================================================="
echo "This command will compile and build platform code via Maven:"
echo "$COMMAND"
echo ""
echo "for ====== sentilo-agent-location-updater ======"
echo ""
echo "Press enter when ready."

read

eval ${COMMAND}
ERROR_LEVEL=$?
if [ "$ERROR_LEVEL" == "1" ]; then error; fi


cd $CURRENT_DIR;
echo ""
echo "=================================================="
echo "Now you're ready to install the Sentilo platform. "
echo "=================================================="
exit "$ERROR_LEVEL"

