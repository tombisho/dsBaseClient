#' @title Passes a string to a server-side environment
#' @description Passes a string to a server side environment and prints the string returned from the server. 
#' @details This is a function to test writing new functions and the communication between the client and the server. 
#' 
#' By default, objects in DataSHIELD's Active Serverside Analytic Environment (\code{.GlobalEnv})
#' will be listed. This is the environment that contains all of the objects that server-side DataSHIELD 
#' is using for the main analysis or has written out to the server-side during the process
#' of managing or undertaking the analysis (variables, scalars, matrices, data frames, etc).
#'  
#' The environment to explore is specified by the argument \code{env.to.search} (i.e. environment
#' to search) to an integer value. The default environment 
#' which R names as \code{.GlobalEnv} is set by specifying \code{env.to.search = 1} or \code{1L}
#' (\code{1L} is just an explicit way of writing the integer \code{1}). 
#' 
#' If the \code{search.GlobalEnv} argument is set to TRUE the \code{env.to.search} parameter  
#' is set to \code{1L} regardless of what value it is set in the call
#' or if it is set to NULL. 
#' So, if \code{search.GlobalEnv} is set to TRUE, \code{ds.ls} will automatically
#' search the \code{.GlobalEnv} R environment on the server-side which contains all of the
#' variables, data frames and other objects read in at the start of the analysis,
#' as well as any new objects of any sort created using DataSHIELD assign functions. 
#' 
#' Other server-side environments contain other
#' objects. For example, environment \code{2L} contains the functions loaded via the native R
#' stats package and \code{6L} contains the standard list of datasets built into R. By default
#' \code{ds.ls} will return a list of ALL of the objects in the environment specified by the
#' \code{env.to.search} argument but you can specify search filters including \code{*} wildcards
#' using the \code{search.filter} argument.
#' 
#' In \code{search.filter} you can use the symbol \code{*} to find all the object that contains
#' the specified characters. For example, \code{search.filter = "Sd2*"} 
#' will list the names of all objects in the specified
#' environment with names beginning capital S, lower case d and number 2. 
#' Similarly, \code{search.filter="*.ID"} will return all objects with names ending with \code{.ID},
#' for example \code{Study.ID}. 
#' If a value is not specified for the \code{search.filter} argument or it is set as NULL, the names of 
#' all objects in the specified environment will be returned.
#' 
#' Server function called: \code{lsDS}. 
#' 
#' @param search.filter character string (potentially including \code{*} symbol) specifying the filter 
#' for the object name that you want to find in the enviroment. For more information see \strong{Details}. 
#' @param env.to.search an integer (e.g. in \code{2} or \code{2L} format) specifying the position
#' in the search path of the environment to be explored. \code{1L} is the current active analytic
#' environment on the server-side and is the default value of \code{env.to.search}.
#' For more information see \strong{Details}.
#' @param search.GlobalEnv Logical. If TRUE, \code{ds.ls} will list all objects
#' in the \code{.GlobalEnv} R environment on the server-side. If FALSE and if \code{env.to.search} is also
#' set as a valid integer, \code{ds.ls} will list all objects in the server-side R environment
#' identified by \code{env.to.search} in the search path. 
#' For more information see \strong{Details}.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return \code{ds.ls} returns to the client-side a list containing: \cr
#' (1) the name/details of the server-side R environment which \code{ds.ls} has searched;\cr
#' (2) a vector of character strings giving the names of
#' all objects meeting the naming criteria specified by the argument \code{search.filter} in this
#' specified R server-side environment;\cr
#' (3) the nature of the search filter string as it was applied. 
#' @author DataSHIELD Development Team
#' @examples
#' \dontrun{
#'
#'   ## Version 6, for version 5 see the Wiki
#'   
#'   # connecting to the Opal servers
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   #Example 1: Obtain the list of  all objects on a server-side environment
#'   
#'   ds.retStr('hello')
#'   
#' }
#'
#' @export
ds.retStr <- function(search.filter=NULL, env.to.search=1L, search.GlobalEnv=TRUE, datasources=NULL)
{
	
	  transmit.object.final = search.filter	

	  # call the server side function
	  cat("On client side: \n")
	  # overwriting transmit.object.final
	  # transmit.object.final = "Thisisaninputstringfromtheclient"
	  # transmit.object.final = "D$cens"
	  # transmit.object.final = "EVENT"
	  calltext <- call("retStrDS",search.filter=transmit.object.final, env.to.search)

	  cat("\n Class of calltext\n")
	  #cat(calltext)
	  cat(class(calltext))
	  cat("\n What is in calltext ? \n")
	  cat(as.character(calltext))
	  cat("\n End of function \n")	

	  output <- datashield.aggregate(datasources, calltext)

	  return(output)
}
#ds.retStr


