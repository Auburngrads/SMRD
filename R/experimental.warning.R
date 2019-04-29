experimental.warning <-
function () 
{
    warning("Some of the procedures launched in this dialog are\nexperimental and may fail to converge, take a long time to run (use\nEsc to break out), or give error messages for other reasons that are\nnot yet possible to control in the SMRD GUI. Solutions (e.g.,\nalternative parameterization or better starting values) may be\navailalbe by working at the programming/command level of\nSplus/SMRD.", 
        prefix.message = "Disclamer:")
}
