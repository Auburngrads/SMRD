fix.old.sim.post <-
function (sim.post) 
{
    if (is.matrix(sim.post$prior)) {
        sim.post$prior <- list(prior = sim.post$prior)
        return(sim.post)
    }
    else {
        return(sim.post)
    }
}
