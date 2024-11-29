
.DEFAULT_GOAL := test

.PHONY: setup test.prev test test.down test.console
setup:
	docker build -t dbi-cp-test .

test.prev:
	docker-compose down || true
	rm -rf ./volumes
	mkdir ./volumes
	mkdir ./volumes/mysql
	mkdir ./volumes/postgresql
	mkdir ./volumes/postgresql/data
	mkdir ./volumes/postgresql/log
	sleep 1
	docker-compose up -d
	echo wait...
	sleep 10

test: test.prev
	@echo "Running tests..."
	docker-compose -f docker-compose.test-runner.yml run --rm --entrypoint rove dbi-cp-test dbi-cp-test.asd

test.down:
	docker-compose down


test.console:
	docker-compose -f docker-compose.test-runner.yml run -it -p 4005:4005 --rm --entrypoint /bin/bash dbi-cp-test
