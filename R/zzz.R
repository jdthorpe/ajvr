
# --------------------------------------------------
# helper functions for moving between R and V8
# --------------------------------------------------

as_literal <- function(x){
	if(inherits(x,"JS_EVAL")){
		# PASS
	}else if( is.character(x) && (length(x) == 1L)){
		if(!grepl("^\\s*(?:'(.[^']|\\\\.)*'|\"(.[^\"]|\\\\.)*\")\\s*$",x)){
			# it's not a quoted string:
			x <- sprintf('"%s"',gsub('"','\\"',x))
		}
	}else{
		x <- get_string(x);
	}
	x
}

get_string <- function(x){
	if(inherits(x,"JS_EVAL")){
		# PASS
	}else if(inherits(x,"connection")){
		x <- paste(readLines(x), collapse = "\n")
	}else if(inherits(x,"character")){
		stopifnot(length(x) != 0L) 
		if( (length(x) == 1) && file.exists(x)){
			if(grepl("\\.ya?ml$",x)){
				x <- .env$ct$eval(sprintf('JSON.stringify(jsyaml.load("%s"))',
										 paste0(gsub('["\\\\]','\\\\"',readLines(x)),collapse='\\n')))
			}else{
				x <- paste(readLines(x), collapse = "\n")
			}
		}else{
			x <- paste(x, collapse="\n")
		}
	}else{
		x <- RJSONIO::toJSON(x,collapse="",pretty=FALSE)
	}
	x
}

.is_object_ref <- function(x)
	grepl("\\[object \\w+\\]",x)

.eval <- function(x,...,raw=FALSE) {
	if(!exists("ct",.env))
		stop("ajvr must be loaded before use.")
	ret <- .env$ct$eval(sprintf(x,...))
	if(raw)
		return(ret)
	if(ret == "undefined"){
		return(NULL)
	}else if(ret == "true"){
		return(TRUE)
	}else if(ret == "false"){
		return(FALSE)
	}else if(ret == "null"){
		return(NULL)
	}else if(!grepl("^\\s*$",ret) && (.env$ct$eval(sprintf("isNaN(%s)",ret))=="false")){
		return(as.numeric(ret))
	}else{
		return(tryCatch({
			return(RJSONIO::fromJSON(I(ret)));
		},error = function(err){
			if(err$message != "invalid JSON input") 
				stop(err)
			return(V8::JS(ret))
		}))
	}
}

.env <- new.env(parent=emptyenv())
.onLoad <- function(libname, pkgname) {
	root <- system.file(package=.packageName)
	.env$ct <- V8::v8()
	for(js_file in c("ajv.js","js-yaml.js")){
		cat(sprintf("loading %s... ",js_file))
		if(.env$ct$source(file.path(root,"ajv.js")) == "true") 
			cat("Done\n") 
		else 
			stop(sprintf("failed to load",js_file))
	}
}


