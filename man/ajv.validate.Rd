% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ajv_methods.R
\name{ajv.validate}
\alias{ajv.validate}
\title{A wrapper for the Ajv.validate method}
\usage{
ajv.validate(this, schema, data)
}
\arguments{
\item{this}{An AJV instance, provided implicitly when called via \code{my_instance$validate(...)}}

\item{schema}{The Schema with which to validate the \code{data}.
\code{schema} may be a  valid JSON string, an R object (i.e. `list(...)`), a
connection to a JSON file, or the name of a JSON or YAML file.  YAML files
are parsed via [js-yaml](https://www.npmjs.com/package/js-yaml)'s
`safeLoad()` method.}

\item{data}{The data to be validated.  may be any of the above foremats.}
}
\description{
The equivalent of calling \code{var ajv = new Ajv(); ajv.validate(...)} in javascript.
}
\examples{
\dontrun{
my_ajv_instance = Ajv()
my_ajv_instance$validate
}
}
\seealso{
Other AJV.Instance.Methods: \code{\link{ajv.addFormat}},
  \code{\link{ajv.addKeyword}},
  \code{\link{ajv.addSchema}},
  \code{\link{ajv.errorsText}}, \code{\link{ajv.keyword}},
  \code{\link{ajv.removeSchema}},
  \code{\link{ajv.validateSchema}}
}

