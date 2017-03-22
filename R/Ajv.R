
#' Create an Ajv instnace.
#'
#' Create an Ajv instnace (the equivalent of calling \code{new Ajv()} in javascript)
#'
#' @param options Optional; see the \href{https://github.com/epoberezkin/ajv#options}{ajv github page} for details
#' @export
Ajv <- function(options=NULL) {
	if(exists)
	name <- basename(tempfile("inst_"))

	# create the AJV instance in V8
	if(is.null(schema))
		.eval("%s = new Ajv()", name )
	else
		.eval("%s = new Ajv(%s)", name, get_string(schema))

	# return a reference to the AJV instance
	out <- list()
	attr(out, "name") <- name
	class(out) <- "AJV-instance"
	out
}

# --------------------------------------------------
# S3 methods for the AJV objects
# --------------------------------------------------

#' @export
`$.AJV-instance` <- function(x,name){
	switch(name,
		   "compile"		=function(schema)ajv.compile(x,schema),
		   "validate"		=function(...)ajv.validate(x,...),
		   "addSchema"		=function(...)ajv.addSchema(x,...),
		   "validateSchema"	=function(...)ajv.validateSchema(x,...),
		   "getSchema"		=function(...)ajv.getSchema(x,...),
		   "removeSchema"	=function(...)ajv.removeSchema(x,...),
		   "addFormat"		=function(...)ajv.addFormat(x,...),
		   "addKeyword"		=function(...)ajv.addKeyword(x,...),
		   "addKeyword"		=function(...)ajv.addKeyword(x,...), 
		   "errorsText"		=function(...)ajv.errorsText(x,...),
		   #"default"=
		   stop(sprintf("AJV-instance has no such method or attribute '%s'",name)) 
		   )
}

#' @export
`print.AJV-instance` <- function(x)cat(sprintf('<AJV-instance "%s">\n',attr(x,"name")))

