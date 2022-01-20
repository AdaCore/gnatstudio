BASE_NAME  = dap-tools
GPS_DIR    = ../generated
OUT_HEADER = ${GPS_DIR}/${BASE_NAME}.ads
OUT_BODY   = ${GPS_DIR}/${BASE_NAME}.adb

all: create pretty

create:
	python3 dap2ada.py

pretty:
	gnatpp ${OUT_HEADER} ${OUT_BODY}
