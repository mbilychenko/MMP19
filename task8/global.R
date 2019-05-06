if (!interactive()){
    options(shiny.error=function(){
        stop("An error has occured")
    })
}