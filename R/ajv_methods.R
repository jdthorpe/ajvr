
# --------------------------------------------------
# METHODS THAT RETURN A BOOLEAN
# --------------------------------------------------

#' The Ajv.validate method
#'
#' The equivalent of calling \code{var ajv = new Ajv(); ajv.validate(...)} in javascript.
#'
#' @param schema The Schema with which to validate the \code{data}.
#'    \code{schema} may be a  valid JSON string, an R object (i.e. `list(...)`), a
#'    connection to a JSON file, or the name of a JSON or YAML file.  YAML files
#'    are parsed via [js-yaml](https://www.npmjs.com/package/js-yaml)'s
#'    `safeLoad()` method.
#' @param data The data to be validated.  may be any of the above foremats. 
#'
ajv.validate <- function(this,schema,data){
	#cat("call to ajv.validate(): ",sprintf("%s.validate(%s,%s)", attr(this,"name"), as_literal(schema),get_string(data)),"\n")
	.eval("%s.validate(%s,%s)", 
		  attr(this,"name"), 
		  as_literal(schema),
		  get_string(data))
}

#-- ajv$validate(fname,list(a="hi"))
#-- ajv$validate(fname,'{ "type":"skew-parameters", "n_to_print": 5, "plot_window": 360, "currency_pair":"USDJPY", "voly_floor":1, "voly_ceiling":1, "upward_coefficeint":1, "downward_coefficient":1, "threshold_coefficient":1 }')

valid_json = '{ "type":"skew-parameters", "n_to_print": 5, "plot_window": 360, "currency_pair":"USDJPY", "voly_floor":1, "voly_ceiling":1, "upward_coefficeint":1, "downward_coefficient":1, "threshold_coefficient":1 }'

#' The Ajv.validateSchema method
#'
#' The validate a json schema
#'
#' @param schema The Schema to be validated. 
#'    \code{schema} may be a  valid JSON string, an R object (i.e. `list(...)`), a
#'    connection to a JSON file, or the name of a JSON or YAML file.  YAML files
#'    are parsed via [js-yaml](https://www.npmjs.com/package/js-yaml)'s
#'    `safeLoad()` method.
#' @return boolean
ajv.validateSchema <- function(this,schema){
	.eval("%s.validateSchema(%s)", 
		  attr(this,"name"), 
		  get_string(schema))
}
#-- ajv$validateSchema(fname)


# --------------------------------------------------
# METHODS THAT RETURN void
# --------------------------------------------------

#' The Ajv.addSchema method
#'
#' The add a schema to an Ajv instance
#'
#' @param schema The schema to be added.
#'    \code{schema} may be a  valid JSON string, an R object (i.e. `list(...)`), a
#'    connection to a JSON file, or the name of a JSON or YAML file.  YAML files
#'    are parsed via [js-yaml](https://www.npmjs.com/package/js-yaml)'s
#'    `safeLoad()` method.
#' @param key String; the name with which to store the schema
#' @return \code{invisible(NULL)}
ajv.addSchema <- function(this,schema,key){
	invisible(.eval("%s.addSchema(%s,%s)", 
						attr(this,"name"), 
						get_string(schema), 
						as_literal(key)))
}

#' The Ajv.removeSchema method
#'
#' The remove a schema from an Ajv instance
#'
#' @param key String; the name with schema to remove
#' @return \code{invisible(NULL)}
ajv.removeSchema <- function(this,key){
	invisible(.eval("%s.removeSchema(%s)", attr(this,"name"), as_literal(key)))
}

#' The Ajv.addFormat method
#'
#' Add a string format to an Ajv instance.
#'
#' @param key String; the name with format to add.
#' @param format the format to be added.  Note that JavaScript object literals
#'   should be enclosed in a call to \code{\link[V8]{JS}}. (i.e.
#'   \code{ajv$addFormat("numbers",JS("/^\\\\d+$/"))})
#' @return \code{invisible(NULL)}
ajv.addFormat <- function(this,key,format){
	invisible(.eval("%s.addFormat(%s,%s)", 
					attr(this,"name"), 
					as_literal(key),
					get_string(format)))
}

#' The Ajv.addFormat method
#'
#' Add a string format to an Ajv instance.
#'
#' @param key String; the name with keyword to add.
#' @param object the format to be added.  Must be enclosed in a call to
#'   \code{\link[V8]{JS}}.
#' @return \code{invisible(NULL)}
ajv.keyword <- function(this,key,object){
	stopifnot(inherits(object,"JS_EVAL"))
	invisible(.eval("%s.keyword(%s,%s)", 
					attr(this,"name"), 
					as_literal(key),
					object))
}


ajv.errorsText <- function(this){
	.eval("%s.errorsText()", attr(this,"name"), raw=TRUE)
}

