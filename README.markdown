# CL-DBI-Connection-Pool - connection pool for CL-DBI

[![Build Status](https://travis-ci.org/tamurashingo/cl-dbi-connection-pool.svg?branch=master)](https://travis-ci.org/tamurashingo/cl-dbi-connection-pool)

This library provides connection pool for CL-DBI.


## Usage

### Create connection pool

```common-lisp
(make-dbi-connection-pool :mysql :database-name "dbi-cp" :username "root" :password "password")
```

### Get connection

```common-lisp
(setf conn (get-connection))
```

### Prepare, Execute, Fetch

Those functions are based on CL-DBI.

```common-lisp
(let* ((query (prepare conn "SELECT * FROM person WHERE id = ?"))
       (result (execute query 1)))
  (fetch result))
```

### Commit, Rollback

```common-lisp
(commit conn)

(rollback conn)
```

### Return connection

```common-lisp
(disconnect conn)
```

## Installation

This library will be available on Quicklisp when ready for use.

## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the LLGPL License.
