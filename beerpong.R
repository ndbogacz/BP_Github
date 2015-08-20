beerpong <- function(aa,ab,ba,bb,times,debug) {
    
    teama <- c(aa,ab)
    teamb <- c(ba,bb)
    results <- c(0,0)
    if (times>1) {debug <- 0}
    
    for (i in 1:times) {
        results <- results+onegame(teama,teamb,debug)        
    }
    
    results
    
}

onegame <- function(teama,teamb,debug) {
    
    results <- oneset(teama,teamb,10,1,debug)
    
    while (results[1]==0) {
        
        results <- oneset(teama,teamb,3,results[2],debug)
        
    }
    
    winner <- c(0,0)
    winner[results[1]] <- winner[results[1]]+1
    winner
    
}    
    
oneset <- function(teama,teamb,num,start,debug) {    
        
    if (start==1) {players <- teama} else {players <- teamb}
    cups <- c(num,num)
    if (runif(1)<players[1]) {cups[start] <- cups[start]-1}
    team <- 3-start
    over <- 0
    
    while (over==0) {
        
        if (team==1) {players <- teama} else {players <- teamb}
        shots <- c(runif(1),runif(1))
        made <- sum(shots<players)
        if (made==2) {made <- made+(runif(1)<players[1])}
        cups[team] <- cups[team]-made
        if (debug==1) {write.table(team);write.table(cups)}
        
        if ((cups[team]<1)&(cups[3-team]>1)) {
            
            if (team==1) {players <- teamb} else {players <- teama}
            miss <- 0
            player <- 1
            while (miss==0) {
                if (runif(1)<players[player]) {
                    cups[3-team] <- cups[3-team]-1
                    player <- 3-player
                } else {miss <- 1}
            }
            if (cups[3-team]<1) {win <- 0} else {win <- team}
            over <- 1
            
        } else if ((cups[team]==0)&(shots[2]<players[2])) {
            
            if (team==1) {players <- teamb} else {players <- teama}
            shots <- c(runif(1),runif(1))
            made <- sum(shots<players)
            if (made==0) {win <- team} else {win <- 0}
            over <- 1
            
        } else if (cups[team]<1) {
            
            if (team==1) {players <- teamb} else {players <- teama}
            if (runif(1)<players[1]) {win <- 0} else {win <- team}            
            over <- 1
            
        } else {team <- 3-team}
        
        
    }
    
    c(win,team)
    
}