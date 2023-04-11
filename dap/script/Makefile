GEN_JSON    = gen_json
GPS_DIR     = ../generated
GNATPP_OPTS = --comments-fill --no-align-modes --no-separate-is --call-threshold=1 --par-threshold=2

all: create pretty

create: dap.json header.adt
	${GEN_JSON} --root-package DAP.Tools \
	  --enum-package Enum --header-file header.adt \
	  debugAdapterProtocol.json > dap_tools.txt
	gnatchop -w dap_tools.txt ${GPS_DIR}

pretty:
	sed -i -e 's/\["/("/' -e 's/"\])/"))/' ${GPS_DIR}/dap-tools-inputs.adb
	gnatpp ${GNATPP_OPTS} ${GPS_DIR}/dap-tools*.ad[sb]
	sed -i -e 's/("/["/' -e 's/"))/"])/' ${GPS_DIR}/dap-tools-inputs.adb	
