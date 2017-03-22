
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

.eval <- function(x,...) {
	if(!exists("ct",.env))
		stop("ajvr must be loaded before use.")
	ret <- .env$ct$eval(sprintf(x,...))
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
#-- 	cat("hi there\n")
	root <- system.file(package=.packageName)
#-- 	cat(root,"\n")
#-- 	root <- "C:\\Users\\MPGWRK-006\\temp\\ajvr\\inst"
	.env$ct <- V8::v8()
	.env$ct$source(file.path(root,"ajv.js"))
	.env$ct$source(file.path(root,"js-yaml.js"))
}


