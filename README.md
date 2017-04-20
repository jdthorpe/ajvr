# ajvr

The `ajvr` library is a very thin wrapper around the awesome
[AJV](https://github.com/epoberezkin/ajv) JSON validation library.  The
essential change between the syntax presented in the official [AJV
Readme](https://github.com/epoberezkin/ajv) is exchanging JavaScripts dot
operator (`.`) with R's dollar-sign operator (`$`).

Note that care must be taken in transforming R objects to JSON, because of
R's everything-is-a-vector philosopy. Hence, for convenience file paths to
valid JSON (`.json`) and YAML (`.yml` or `.yaml`)  may be used wherever an
object is expected.

## Installation

```R
# install.packages("devtools") # (if not already installed)
devtools::install_github("jdthorpe/ajvr")
```

## Getting started

The fastest validation call:

```R
library('ajv')
var ajv = Ajv() # options can be passed, e.g. list(allErrors= TRUE)
var validate = ajv$compile(schema)
var valid = validate(data)
if (!valid) print(validate$errors)
```

or with less code

```R
# ...
var valid = ajv$validate(schema, data)
if (!valid) print(ajv$errors)
# ...
```

or

```R
# ...
ajv$addSchema(schema, 'mySchema')
var valid = ajv$validate('mySchema', data)
if (!valid) print(ajv$errorsText())
# ...
```

Note that in each of these calls, `schema` and `data` arguments may be a
valid JSON string, an R object (i.e. `list(...)`), a connection to a JSON
file, or the file name of JSON or YAML file.  YAML files are parsed via
[js-yaml](https://www.npmjs.com/package/js-yaml)'s `safeLoad()` method.

See [API](https://github.com/epoberezkin/ajv#api) and
[Options](https://github.com/epoberezkin/ajv#options) for more details.

