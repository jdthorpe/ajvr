% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ajv_methods.R
\name{ajv.addSchema}
\alias{ajv.addSchema}
\title{A wrapper for the Ajv.addSchema method}
\usage{
ajv.addSchema(this, schema, key)
}
\arguments{
\item{this}{An AJV instance, provided implicitly when called via \code{my_instance$addSchema(...)}}

\item{schema}{The schema to be added.
\code{schema} may be a  valid JSON string, an R object (i.e. `list(...)`), a
connection to a JSON file, or the name of a JSON or YAML file.  YAML files
are parsed via [js-yaml](https://www.npmjs.com/package/js-yaml)'s
`safeLoad()` method.}

\item{key}{String; the name with which to store the schema}
}
\value{
\code{invisible(NULL)}
}
\description{
The add a schema to an Ajv instance
}
\examples{
\dontrun{
my_ajv_instance = Ajv()
my_ajv_instance$addSchema
}
}
\seealso{
Other AJV.Instance.Methods: \code{\link{ajv.addFormat}},
  \code{\link{ajv.addKeyword}},
  \code{\link{ajv.errorsText}}, \code{\link{ajv.keyword}},
  \code{\link{ajv.removeSchema}},
  \code{\link{ajv.validateSchema}},
  \code{\link{ajv.validate}}
}

