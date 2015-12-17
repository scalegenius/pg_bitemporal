#
#
#
PGDATABASE ?= bitemporal

sql/%.sql : %.sql.lit
	lit -c --code-dir=sql $<

docs/%.sql.md : %.sql.lit
	lit -m  --docs-dir=docs $<

all: docs/relationships.sql.md sql/relationships.sql
	@echo $(PGDATABASE)

load: all 
	@bash build/load_database.sh

tests: load tests/*.sql
	@bash build/test_database.sh


#  vim: set filetype=make noexpandtab tabstop=6 shiftwidth=6:
# syntax=make
