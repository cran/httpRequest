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
PostToHost("127.0.0.1","/cgi-bin/param_list.pl",test2,referer="www.test.de",port=port)


##==============================================================
## SimplePostToHost
##==============================================================


port <- 80
data = "pid=14&poll_vote_number=2";
SimplePostToHost("127.0.0.1","/cgi-bin/param_list.pl","http://www.linux.com/polls/index.phtml?pid=1415",data,port=port)


##==============================================================
## GetToHost.
##==============================================================
port <- 80
GetToHost("www.spiegel.de","index.html","www.test.pl",port=port)
