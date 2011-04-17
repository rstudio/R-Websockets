# This example is intended to run from http://illposed.net/rwebsockjson.html,
# although the interaction between the scripts and R runs on the user's
# local machine.
library('websockets')
library('RJSONIO')

w = createContext()
f = function(DATA,WSI,COOKIE)
{
  n = as.numeric(rawToChar(DATA))
  if(is.na(n)) n = 5
  n = max(n, 2)
  A = rbind(1:n, runif(n))
  opt = list(lines=list(show='true'),
             color='#0000FF',
             title='This data is from your local R/Wesockets service',
             points=list(show='true'),
             mouse=list(track='true')  )
  J = toJSON(list(series=list(list(data=apply(A,2,list)),list()), options=opt))
  websocket_write(J,WSI)
}
setCallback("receive",f, w)

cat("\n\n------------------------------------------------------------------------------\n","Open a browser to http://illposed.net/rwebsockjson.html\n\n","The service will stop after five minutes or until <CTRL>+C is pressed\n","------------------------------------------------------------------------------\n")
startTime = proc.time()[3]
while(proc.time()[3] - startTime < 300) {
  service(w)
  Sys.sleep(0.05)
}
rm(w)
