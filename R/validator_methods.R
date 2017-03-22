
# --------------------------------------------------
# METHODS THAT RETURN A VALIDATOR FUNCTION
# --------------------------------------------------

#' The Ajv.compile method
#'
#' Create an Ajv validator function from a schema
#'
#' @param schema The Schema with which to validate the \code{data}.
#'    \code{schema} may be a  valid JSON string, an R object (i.e. `list(...)`), a
#'    connection to a JSON file, or the name of a JSON or YAML file.  YAML files
#'    are parsed via [js-yaml](https://www.npmjs.com/package/js-yaml)'s
#'    `safeLoad()` method.
#' @return an AJV validation function 
ajv.compile <- function(this,schema){
	name <- basename(tempfile("fun_"))
	.eval("%s = %s.compile(%s)", 
		  name,
		  attr(this,"name"), 
		  get_string(schema),
		  raw=TRUE)
	out <- function(x)
		.eval("%s(%s)", 
			  name,
			  get_string(x))
	attr(out,"name") <- "name"
	class(out) <- "AJV-validatefunction"
	out
}

#' The Ajv.compile method
#'
#' Create an Ajv validator function from a schema
#'
#' @param key String; the name of the schema to fetch from the Ajv instancd.
#' @return an AJV validation function 
ajv.getSchema <- function(this,key){
	name <- basename(tempfile("fun_"))
	.eval("%s = %s.getSchema(%s)", 
		  name,
		  attr(this,"name"), 
		  as_literal(key),
		  raw=TRUE)
	out <- function(x)
		.eval("%s(%s)", 
			  name,
			  get_string(x))
	attr(out,"name") <- name
	class(out) <- "AJV-validatefunction"
	out
}

# --------------------------------------------------
# S3 methods for the validator functions
# --------------------------------------------------
#' @export
`$.AJV-validatefunction` <- function(x,name){
	switch(name,
		   "errors"= .eval("%s.errors", attr(x,"name")) ,
		   #"default"=
		   stop(sprintf("AJV-validatefunction has No such method or attribute '%s'",name)) 
		   )
}

#' @export
`print.AJV-validatefunction` <- function(x)cat(sprintf('<AJV-validatefunction "%s">\n',attr(x,"name")))

