make.small.posterior.object <-
function (posterior.object, number.rows = 10) 
{
    posterior.object$prior <- posterior.object$prior[1:number.rows, 
        ]
    posterior.object$post <- posterior.object$post[1:number.rows, 
        ]
    return(posterior.object)
}
