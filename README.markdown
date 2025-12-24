# CL-DBI-Connection-Pool - connection pool for CL-DBI

![ci workflow](https://github.com/tamurashingo/cl-dbi-connection-pool/actions/workflows/ci.yml/badge.svg)

This library provides connection pool for CL-DBI.


## Usage

### Create connection pool

make connection pool.

```common-lisp
(dbi-cp:make-db-connection-pool driver-name &key database-name username password (initial-size 10) (max-size 10) (checkout-timeout 30) (idle-timeout 600) (max-lifetime 1800) (keepalive-interval 0) validation-query (reaper-interval 60)) ;; => dbi-cp:<dbi-connection-pool>
```

- driver-name
    - `:sqlite`, `:mysql`, `:postgresql` (same as `CL-DBI`)
- database-name
    - database name (same as `CL-DBI`)
- username
    - username (same as `CL-DBI`)
- password
    - password (same as `CL-DBI`)
- initial-size
    - initial number of connections that are created when the pool is started
- max-size
    - maximum number of connections
- checkout-timeout
    - maximum wait time (in seconds) when acquiring a connection from the pool (default: 30 seconds)
- idle-timeout
    - time in seconds after which idle connections are removed from the pool (default: 600 seconds)
- max-lifetime
    - maximum lifetime in seconds for a connection since creation (default: 1800 seconds, NIL to disable)
- keepalive-interval
    - interval in seconds for checking connection validity (default: 0 to disable, recommended: 600 seconds)
- validation-query
    - query used to validate connection (e.g., "SELECT 1") (default: automatically set based on database type)
- reaper-interval
    - interval in seconds between reaper thread executions (default: 60 seconds, usually no need to change)


 Additionally, it accepts parameters used in cl-dbi.


```common-lisp
;;; Basic usage example
(defparameter *CONNECTION-POOL*
  (dbi-cp:make-dbi-connection-pool :mysql 
                                   :database-name "dbi-cp" 
                                   :username "root" 
                                   :password "password"))

;;; Advanced connection pool configuration example
(defparameter *POOL-WITH-OPTIONS*
  (dbi-cp:make-dbi-connection-pool :mysql
                                   :database-name "production"
                                   :username "app_user"
                                   :password "password"
                                   :initial-size 5
                                   :max-size 20
                                   :checkout-timeout 30        ; wait up to 30 seconds
                                   :idle-timeout 600           ; remove idle connections after 10 minutes
                                   :max-lifetime 1800          ; recreate connections after 30 minutes
                                   :keepalive-interval 600     ; check validity every 10 minutes
                                   :validation-query "SELECT 1")) ; validation query
```

### Get connection

get database connection from connection pool.

```common-lisp
(dbi-cp:get-connection connection-pool) ;; => dbi-cp.proxy:<dbi-connection-proxy>
```

- connection-pool (dbi-cp:&lt;dbi-connection-pool&gt;)
    - connection pool

```common-lisp
(setf conn (dbi-cp:get-connection *CONNECTION-POOL*))
```

### prepare, execute, fetch ...

#### Prepare

prepare SQL statement.

```common-lisp
(dbi-cp:prepare connection sql) ;; => dbi.driver:<dbi-query>
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection
- sql
    - SQL statement

this function is based on `CL-DBI`

#### Execute

execute SQL.

```common-lisp
(dbi-cp:execute query &optional params) ;; => dbi.driver:<dbi-query>
```

- query (dbi.driver:&lt;dbi-query&gt;)
    - precompiled query (returned by `prepare`)
- params
    - SQL parameters

this function is based on `CL-DBI`

#### Fetch

fetch first row from `query` which is returned by `execute`.

```common-lisp
(dbi-cp:fetch query) ;; => result
```

- query (dbi.driver:&lt;dbi-query&gt;)
    - returned by `execute`

this function is based on `CL-DBI`

### Fetch all

fetch all ret row from `query`.

```common-lisp
(dbi-cp:fetch-all query) ;; => result
```

- query (dbi.driver:&lt;dbi-query&gt;)
    - returned by `execute`

this function is based on `CL-DBI`

#### row count

return the number of counts modified by last execute INSERT/UPDATE/DELETE query.

```common-lisp
(dbi-cp:row-count connection) ;; => number
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection

this function is based on `CL-DBI`

#### do sql

do preparation and execution at once for INSERT, UPDATE, DELETE or DDL.


```common-lisp
(dbi-cp:do-sql connection sql &optional params)
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection
- sql
    - SQL statement
- params
    - SQL parameters

this function is based on `CL-DBI`

### Release connection

Return the connection to the pool. The connection is not closed but returned to the pool for reuse.

```common-lisp
(dbi-cp:disconnect connection)
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection

### Shutdown connection pool

Close all connections and stop the connection pool. Also stops the background reaper thread.

```common-lisp
(dbi-cp:shutdown pool)
```

- pool (dbi-cp:&lt;dbi-connection-pool&gt;)
    - connection pool


### Transaction

#### create transaction block

start a transaction and commit at the end of this block.
if the evaluation `body` is interrupted, the transaction is rolled back automatically.

```common-lisp
(dbi-cp:with-transaction connection &body body)
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection
- body
    - body

this function is based on `CL-DBI`

#### commit

Within `with-transaction`, you can use `commit`.
Outside of `with-transaction`, `commit` does nothing.


```common-lisp
(dbi-cp:commit connection)
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection

this function is based on `CL-DBI`

#### rollback

Like `commit`, `rollback` is also executed within `with-transaction`.


```common-lisp
(dbi-cp:rollback connection)
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection

this function is based on `CL-DBI`

#### savepoint

set a named transaction savepoint with a name of `identifier`.

```common-lisp
(dbi-cp:savepoint connection identifier)
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection
- identifier
    - name of transaction savepoint

this function is based on `CL-DBI`

#### rollback savepoint

rollback a transaction to the named savepoint.

```common-lisp
(dbi-cp:rollback-savepoint connection &optional identifier)
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection
- identifier
    - name of transaction savepoint

this function is based on `CL-DBI`


#### release savepoint

remove the named savepoint. no commit or rollback occurs.

```common-lisp
(dbi-cp:release-savepoint connection &optional identifier)
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection
- identifier
    - name of transaction savepoint

this function is based on `CL-DBI`


## Advanced Connection Pool Features

For production use, the following advanced connection management features are provided.

### checkout-timeout (Connection Acquisition Timeout)

Set the wait time when acquiring a connection from the pool.

- **Default**: 30 seconds
- **Effect**: If all connections are in use, wait for the specified time and retry
- **Purpose**: Prevent errors during temporary high load

```common-lisp
(dbi-cp:make-dbi-connection-pool :mysql
  :database-name "test"
  :username "root"
  :password "password"
  :checkout-timeout 30)  ; wait up to 30 seconds
```

### idle-timeout (Idle Timeout)

Automatically remove unused connections from the pool.

- **Default**: 600 seconds (10 minutes)
- **Effect**: Remove connections that have not been used for the specified time, shrink to `:initial-size`
- **Purpose**: Resource efficiency during low load

```common-lisp
(dbi-cp:make-dbi-connection-pool :mysql
  :database-name "test"
  :username "root"
  :password "password"
  :initial-size 5
  :max-size 20
  :idle-timeout 600)  ; remove after 10 minutes of inactivity
```

### max-lifetime (Maximum Lifetime)

Manage the elapsed time since connection creation and automatically recreate old connections.

- **Default**: 1800 seconds (30 minutes)
- **Effect**: Recreate connections after the specified time has elapsed
- **Purpose**: Handle database-side timeouts, improve stability of long-running applications

```common-lisp
(dbi-cp:make-dbi-connection-pool :mysql
  :database-name "test"
  :username "root"
  :password "password"
  :max-lifetime 1800)  ; recreate after 30 minutes

;; If MySQL wait_timeout is 8 hours
(dbi-cp:make-dbi-connection-pool :mysql
  :database-name "test"
  :username "root"
  :password "password"
  :max-lifetime 28740)  ; 8 hours - 60 seconds
```

**Recommended setting**: Set slightly shorter than the database's `wait_timeout`

### keepalive-interval (Keepalive Interval)

Periodically check connection validity and detect/recreate invalid connections.

- **Default**: 0 (disabled)
- **Recommended**: 600 seconds (10 minutes)
- **Effect**: Check connection validity at specified intervals and recreate if invalid
- **Purpose**: Detect silent disconnections, detect database-side timeouts

```common-lisp
(dbi-cp:make-dbi-connection-pool :mysql
  :database-name "test"
  :username "root"
  :password "password"
  :keepalive-interval 600     ; check every 10 minutes
  :validation-query "SELECT 1")
```

**Note**: `validation-query` must be set to enable `keepalive-interval`.

### validation-query (Validation Query)

Set the SQL query to check connection validity.

- **Default**: Automatically set based on database type (MySQL/PostgreSQL/SQLite3: "SELECT 1")
- **Effect**: Execute this query during keepalive checks to validate the connection
- **Purpose**: Validate connection validity, use with keepalive

```common-lisp
;; Use default value (recommended)
(dbi-cp:make-dbi-connection-pool :mysql
  :database-name "test"
  :username "root"
  :password "password")

;; Specify custom query
(dbi-cp:make-dbi-connection-pool :postgres
  :database-name "test"
  :username "postgres"
  :password "password"
  :validation-query "SELECT 1")

;; Application-specific validation
(dbi-cp:make-dbi-connection-pool :postgres
  :database-name "myapp"
  :username "appuser"
  :password "password"
  :validation-query "SELECT 1 FROM users LIMIT 1")
```

**Best practices**: 
- Use lightweight queries (`SELECT 1` recommended)
- Use queries without side effects (no INSERT/UPDATE/DELETE)
- Use queries that execute quickly

### reaper-interval (Reaper Thread Execution Interval)

Set the execution interval of the reaper thread that runs in the background.

- **Default**: 60 seconds
- **Effect**: Control the reaper thread execution interval
- **Purpose**: Usually no need to change, for advanced tuning

The reaper thread is responsible for:
1. Removing idle connections based on `idle-timeout`
2. Recreating old connections based on `max-lifetime`
3. Checking connection validity based on `keepalive-interval`

```common-lisp
(dbi-cp:make-dbi-connection-pool :mysql
  :database-name "test"
  :username "root"
  :password "password"
  :reaper-interval 60)  ; execute every 60 seconds (default)
```

**Note**: 
- Setting this value too small consumes system resources
- Setting it too large delays the response of idle-timeout and max-lifetime
- The default value (60 seconds) is usually sufficient

### MySQL-Specific Features

When using the MySQL driver, the following automatic adjustment features are enabled:

#### Automatic Retrieval of max_allowed_packet

Automatically retrieve the MySQL server's `max_allowed_packet` setting, which can be referenced when executing large queries.

#### Automatic Adjustment of max-lifetime Based on wait_timeout

Automatically retrieve the MySQL server's `wait_timeout` setting and check if `max-lifetime` is appropriately configured. If `max-lifetime` is longer than `wait_timeout`, a warning is displayed.

```common-lisp
;; If MySQL wait_timeout is 28800 seconds (8 hours)
;; It is recommended to set max-lifetime shorter than wait_timeout
(dbi-cp:make-dbi-connection-pool :mysql
  :database-name "test"
  :username "root"
  :password "password"
  :max-lifetime 28740)  ; 8 hours - 60 seconds
```

This feature allows the application to automatically recreate connections before they timeout on the database side.

### Recommended Configuration

Recommended configuration for production environments:

```common-lisp
(dbi-cp:make-dbi-connection-pool :mysql
  :database-name "production"
  :username "app_user"
  :password "password"
  :initial-size 5              ; minimum number of connections
  :max-size 20                 ; maximum number of connections
  :checkout-timeout 30         ; wait 30 seconds
  :idle-timeout 600            ; remove idle connections after 10 minutes
  :max-lifetime 1800           ; recreate connections after 30 minutes
  :keepalive-interval 600      ; check validity every 10 minutes
  :validation-query "SELECT 1") ; validation query
```

### Compatibility with Other Frameworks

These parameters are compatible with standard parameters from the following frameworks:

- **Spring Boot (HikariCP)**: `connection-timeout`, `idle-timeout`, `max-lifetime`, `keepalive-time`, `connection-test-query`
- **Rails (ActiveRecord)**: `checkout_timeout`, `idle_timeout`, `keepalive`

### Important Notes

#### Relationship between keepalive-interval and validation-query

When enabling `keepalive-interval` (setting a value greater than 0), **you must set `validation-query`**. If `validation-query` is not set, keepalive checks will not be executed and a warning message will be displayed.

```common-lisp
;; ❌ Incorrect example: only setting keepalive-interval
(dbi-cp:make-dbi-connection-pool :mysql
  :database-name "test"
  :username "root"
  :password "password"
  :keepalive-interval 600)  ; warning will be displayed

;; ✓ Correct example: also setting validation-query
(dbi-cp:make-dbi-connection-pool :mysql
  :database-name "test"
  :username "root"
  :password "password"
  :keepalive-interval 600
  :validation-query "SELECT 1")  ; works correctly
```

#### Recommended Parameter Settings

The following relationships exist between parameters:

1. **keepalive-interval < max-lifetime** is recommended
   - Periodically check with keepalive, finally reset with max-lifetime
   - Example: `keepalive-interval=600`, `max-lifetime=1800`

2. **max-lifetime < database wait_timeout** is recommended
   - Recreate on the application side before timing out on the database side
   - Example (when MySQL wait_timeout=28800): `max-lifetime=28740`

3. **reaper-interval usually uses the default value**
   - 60 seconds is sufficient unless there is a special reason


## Example

```common-lisp
;;;
;;; create connection pool
;;;
CL-USER> (defparameter *pool*
           (dbi-cp:make-dbi-connection-pool :mysql
                                            :database-name "test"
                                            :username "root"
                                            :password "password"))
*POOL*

;;;
;;; get database connection
;;;
CL-USER> (defparameter connection (dbi-cp:get-connection *pool*))
CONNECTION
CL-USER> connection
#<DBI-CP.PROXY:<DBI-CONNECTION-PROXY> {1002E23973}>

;;;
;;; execute DDL
;;;
CL-USER> (dbi-cp:do-sql connection "create table person (id integer primary key, name varchar(24) not null)")
; No value

;;;
;;; select
;;;
CL-USER> (let* ((query (dbi-cp:prepare connection "select count(*) from person"))
                (result (dbi-cp:execute query)))
           (format T "~A" (dbi-cp:fetch result)))
(count(*) 0)
NIL

;;;
;;; insert
;;;
CL-USER> (dbi-cp:with-transaction connection
           (let* ((query (dbi-cp:prepare connection "insert into person (id, name) values (?, ?)")))
             (dbi-cp:execute query (list 1 "user1"))
             (dbi-cp:execute query (list 2 "user2"))
             (dbi-cp:execute query (list 3 "user3"))))
#<DBD.MYSQL:<DBD-MYSQL-QUERY> {1004B671F3}>

;;;
;;; select
;;;
CL-USER> (let* ((query (dbi-cp:prepare connection "select * from person"))
                (result (dbi-cp:execute query)))
           (dbi-cp:fetch-all result))
((:|id| 1 :|name| "user1") (:|id| 2 :|name| "user2") (:|id| 3 :|name| "user3"))

;;;
;;; rollback
;;;
CL-USER> (dbi-cp:with-transaction connection
           (dbi-cp:execute (dbi-cp:prepare connection "delete from person"))
           (rollback connection))
0
CL-USER> (dbi-cp:rollback connection)
; No value
CL-USER> (let* ((query (dbi-cp:prepare connection "select count(*) from person"))
                (result (dbi-cp:execute query)))
           (format T "~A" (dbi-cp:fetch result)))
(count(*) 3)
NIL

;;;
;;; release connection
;;;
CL-USER> (dbi-cp:disconnect connection)
NIL

;;;
;;; shutdown connection pool
;;;
CL-USER> (dbi-cp:shutdown *pool*)
NIL
```


## Installation

This library is available on Quicklisp.

```commonlisp
(ql:quickload :cl-dbi-connection-pool)
```


## how to develop

require

- make
- docker

### test

#### prepare

To prepare, create a docker image for testing.

```sh
make setup
```

#### run test

```sh
make test
```

#### swank server

Start the swank server.

```sh
make test.swank
```

connect with SLIME.

```
M-x slime-connect 127.0.0.1 4005
```

create connection pool

```common-lisp
(ql:quickload :cl-dbi-connection-pool)

(defvar *pool-sqlite3* (dbi-cp:make-dbi-connection-pool :sqlite3
                                                        :database-name "/app/volumes/sqlite3-test.db"))

(defvar *pool-mysql* (dbi-cp:make-dbi-connection-pool :mysql
                                                      :database-name "test"
                                                      :username "root"
                                                      :password "password"
                                                      :host "mysql-test"
                                                      :port 3306))

(defvar *pool-postgres* (dbi-cp:make-dbi-connection-pool :postgres
                                                         :database-name "test"
                                                         :username "dbicp"
                                                         :password "password"
                                                         :host "postgresql-test"
                                                         :port 5432))
```


#### stop test containers

```sh
make test.down
```

## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the LLGPL License.
