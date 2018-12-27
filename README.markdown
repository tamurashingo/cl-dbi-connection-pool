# CL-DBI-Connection-Pool - connection pool for CL-DBI

[![Build Status](https://travis-ci.org/tamurashingo/cl-dbi-connection-pool.svg?branch=master)](https://travis-ci.org/tamurashingo/cl-dbi-connection-pool)

This library provides connection pool for CL-DBI.


## Usage

### Create connection pool

make connection pool.

```common-lisp
(dbi-cp:make-db-connection-pool driver-name &key database-name username password (initial-size 10) (max-size 10)) ;; => dbi-cp:<dbi-connection-pool>
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


```common-lisp
(defparameter *CONNECTION-POOL*
  (dbi-cp:make-dbi-connection-pool :mysql :database-name "dbi-cp" :username "root" :password "password"))
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
(dbi-cp:execute query &rest params) ;; => dbi.driver:<dbi-query>
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
(dbi-cp:do-sql connection sql &rest params)
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection
- sql
    - SQL statement
- params
    - SQL parameters

this function is based on `CL-DBI`


### Transaction

#### Start transaction

start a transaction.

```common-lisp
(dbi-cp:begin-transaction connection)
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection

this function is based on `CL-DBI`


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

```common-lisp
(dbi-cp:commit connection)
```

- connection (dbi-cp.proxy:&lt;dbi-connection-proxy&gt;)
    - database connection

this function is based on `CL-DBI`

#### rollback

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
CL-USER> (dbi-cp:begin-transaction connection)
; No value

CL-USER> (let* ((query (dbi-cp:prepare connection "insert into person (id, name) values (?, ?)")))
           (dbi-cp:execute query 1 "user1")
           (dbi-cp:execute query 2 "user2")
           (dbi-cp:execute query 3 "user3"))
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
CL-USER> (dbi-cp:rollback connection)
; No value
CL-USER> (let* ((query (dbi-cp:prepare connection "select count(*) from person"))
                (result (dbi-cp:execute query)))
           (format T "~A" (dbi-cp:fetch result)))
(count(*) 0)
NIL
```


## Installation

This library is available on Quicklisp.

```commonlisp
(ql:quickload :cl-dbi-connection-pool)
```

## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the LLGPL License.
