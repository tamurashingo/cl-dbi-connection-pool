
.DEFAULT_GOAL := test

.PHONY: setup test.prev test test.down test.console test.swank
setup:
	docker build -t dbi-cp-test . --no-cache

test.prev:
	docker-compose down || true
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

test.swank:
	docker-compose -f docker-compose.test-runner.yml run -it -p 4005:4005 --rm --entrypoint ros dbi-cp-test run -e "(ql:quickload :swank)" -e "(setf swank::*loopback-interface* \"0.0.0.0\")" -e "(swank:create-server :dont-close t :style :spawn)"
