# httpd.R: A generic and basic example non-websockets web service.
library('websockets')

httpd = function(socket, header) {
  basedir = "."
  tryCatch({
    if(is.null(header$RESOURCE)) return(websockets:::.http_400(socket))
    if(header$RESOURCE == "/favicon.ico") 
      return(http_response(socket, 200, "image/x-icon",websockets:::.html5ico))
    res = strsplit(header$RESOURCE,split="\\?")[[1]]
    fn = paste(basedir,gsub("\\.\\.","",res[[1]]),sep="")
    if(fn=="./") fn = "./index.html"
    vars = http_vars(socket, header)
    f = file(fn,open="rb")
    file_content = readBin(f,raw(),n=1000000L)
    close(f)
    http_response(socket,200,content=file_content)
  },
  error=function(e) {print(e); websockets:::.http_400(socket)},
  warning=function(e){print(e); websockets:::.http_400(socket)})
}

w = createContext(webpage=httpd)
cat("Direct your local web browser to http://localhost:7681\n")
while(TRUE) service(w)
