
MODEL=../share/dbschema.txt
INITIAL=../share/initialdata.txt

DB2ADA=gnatcoll_db2ada -output generated -dbmodel=${MODEL}

all: createdb
	@mkdir -p obj
	@${DB2ADA} -api Database -orm Orm
	gprbuild -m -j0 -Pentities.gpr

test: createdb
	@gnatmake -u -p -Ptest1/test1.gpr

profile:
	-@mv -f .gnatdebug .gnatdebug 2>/dev/null
	@echo "=================="
	@echo "Flushing disk caches"
	@sync ; echo 3 | sudo tee /proc/sys/vm/drop_caches >/dev/null
	@time ${MAKE} createdb
	@./obj/test_entities
	@echo "=================="
	@echo "With files in the cache"
	@time ${MAKE} createdb
	@./obj/test_entities

createdb:
	@rm -f entities.db
	@${DB2ADA} -dbtype=sqlite -dbname=entities.db \
		-createdb \
		-load=${INITIAL} \
		-api-enums Database_Enums -enum "f2f_kind,id,name,F2F_,Integer"

doc:
	@gnatcoll_db2ada -dbmodel=${MODEL} -dot
	@ps2pdf -sPAGESIZE=a4 schema.ps
	@${RM} schema.ps schema.dot
	@echo "Created schema.pdf"



