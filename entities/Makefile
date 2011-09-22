
MODEL=../share/dbschema.txt
INITIAL=../share/initialdata.txt

DB2ADA=gnatcoll_db2ada -output generated -dbmodel=${MODEL}

all:
	@mkdir -p obj

	@# Create the Ada API for the database
	@${DB2ADA} -api Database -orm Orm

	@# Create a dummy database, just to be able to extract the initial
	@# data from it. ??? Should be doable directly from initialdata.txt
	@${DB2ADA} -dbtype=sqlite -dbname=:memory: \
		-createdb \
		-load=${INITIAL} \
		-api-enums Database_Enums -enum "f2f_kind,id,name,F2F_,Integer"  \
		-api-enums Database_Enums -enum "e2e_kind,id,name,E2E_,Integer" 

	gprbuild -m -j0 -Pentities.gpr

test: createdb
	@gnatmake -u -p -Ptest1/test1.gpr

profile:
	@sync ; echo 3 | sudo tee /proc/sys/vm/drop_caches >/dev/null
	@echo "===== Flushing disk caches ====="
	@rm -f entities.db
	@./obj/test_entities

	@echo "===== Files in disk cache ======"
	@rm -f entities.db
	@./obj/test_entities

	#@echo "===== Start with an existing empty db ===="
	#@rm -f entities.db
	#@${DB2ADA} -dbtype=sqlite -dbname=entities.db -createdb -load=${INITIAL}
	#@./obj/test_entities


doc:
	@gnatcoll_db2ada -dbmodel=${MODEL} -dot
	@ps2pdf -sPAGESIZE=a4 schema.ps
	@${RM} schema.ps schema.dot
	@echo "Created schema.pdf"



