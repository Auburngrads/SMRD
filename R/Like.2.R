Like.2 <-
function(theta,d) {
    d1<-d[1];d2<-d[2];d3<-d[3];d4<-d[4];d5<-d[5];d6<-d[6];d7<-d[7];d8<-d[8]
    F<-c(rep(NA,1))
    
    F<-log(exp((-0/   theta))-exp((-100/ theta)))*d1+
        log(exp((-100/ theta))-exp((-300/ theta)))*d2+
        log(exp((-300/ theta))-exp((-500/ theta)))*d3+
        log(exp((-500/ theta))-exp((-700/ theta)))*d4+
        log(exp((-700/ theta))-exp((-1000/theta)))*d5+
        log(exp((-1000/theta))-exp((-2000/theta)))*d6+
        log(exp((-2000/theta))-exp((-4000/theta)))*d7+
        log(exp((-4000/theta))-exp((-Inf/ theta)))*d8
    
    return(-(F))
}
