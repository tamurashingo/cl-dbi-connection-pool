# CL-DBI-Connection-Pool - connection pool for CL-DBI

## Usage

### Create connection pool

```common-lisp
(make-connection-pool :mysql :database-name "cldbi" :username "root" :password "password")
```

### Get connection

```common-lisp
(setf conn (get-connection))
```

### Return connection

```common-lisp
(disconnect conn)
```

## Installation

## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the LLGPL License.
