GetSMRDDefault <-
  function (property)
  {
    exists.in <-
      function (the.element, the.list)
      {
          if (exists(the.list)) {
            if (!is.null(get(envir = .frame0, the.list)[[the.element]]))
              return(TRUE)
            else return(FALSE)
          } else return(FALSE)
      }

    if (!is.character(property)) {
      warning("property must be a character string")
      print(property)
    }

    LocalSMRDOptionsDefaults <- SMRDOptionsDefaults()
    if (!is.null(LocalSMRDOptionsDefaults[[property]]))
      return(unlist(LocalSMRDOptionsDefaults[[property]]))
    else stop(paste(property, "is not in SMRDOptionsDefaults"))
  }
