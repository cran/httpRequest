## To make those tests you have to have an Apache installed and running
## and the file param_list.cgi in the cgi-bin folder.



##==============================================================
## PostToHost
##==============================================================

port <- 80
test2 <- list(
            "fruit"="apple"
           ,"dat_defs"="20021204/F113213.dat"
           ,"upsa"="test"
           )
           print(test2)
res<-postToHost("www.molgen.mpg.de","/~wolski/test.php4",test2,referer="www.test.pl",port=port)
cat(res)

##==============================================================
## SimplePostToHost
##==============================================================


port <- 80
data = "pid=14&poll_vote_number=2";
simplePostToHost("www.molgen.mpg.de","/~wolski/test.php4","",data,port=port)




##==============================================================
## getToHost.
##==============================================================
port <- 80

getToHost("www.molgen.mpg.de","/~wolski/test.php4?test=test1&test2=test2&test3=3","www.test.pl",port=port)

