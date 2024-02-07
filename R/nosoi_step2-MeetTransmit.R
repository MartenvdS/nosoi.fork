#' @title Meet Transmit function
#'
#' @description
#' Perform the tasks related to meeting and transmission events.
#'
#' @param res an object of class \code{nosoiSimOne}.
#' @inheritParams newLine
#' @inheritParams newLineState
#'
#' @return A table with active transmission events
#'
#' @keywords internal
##
meetTransmit <- function (res, pres.time, positions, nContactParsed, pTransParsed)
{
  hosts.ID <- NULL
  active.hosts <- res$table.hosts[["active"]]
  df.meetTransmit <- res$table.hosts[active.hosts, c("hosts.ID",
                                                     positions), with = FALSE]
  setnames(df.meetTransmit, "active.hosts", "hosts.ID", skip_absent = TRUE)
  if (res$prefix.host == "M") {
    df.meetTransmit$number.contacts <- as.numeric(runif(sum(active.hosts)) <
                                                    0.7)
  }
  else if (res$prefix.host == "Haha, Does not work :-(") {
    df.meetTransmit$number.contacts <- lapply(1:sum(active.hosts),
                                              as.list(environment(nContactParsed$vect))$pFunc)
  }
  else {
    df.meetTransmit$number.contacts <- applyFunctionToHosts(res,
                                                            pres.time, nContactParsed, active.hosts)
  }
  haveContact <- df.meetTransmit[["number.contacts"]] > 0
  df.meetTransmit <- df.meetTransmit[haveContact]
  active.hosts[active.hosts] <- haveContact
  if (nrow(df.meetTransmit) > 0) {
    if (res$prefix.host == "H") {
      temp.vec <- pres.time - res$table.hosts$inf.time[res$table.hosts$hosts.ID %in%
                                                         df.meetTransmit$hosts.ID]
      df.meetTransmit[, "Ptransmit"] <- temp <- sapply(temp.vec,
                                                       as.list(environment(pTransParsed$vect))$pFunc)
    }
    else if (res$prefix.host == "M") {
      df.meetTransmit[, "Ptransmit"] <- as.numeric((pres.time -
                                                      res$table.hosts$inf.time[res$table.hosts$hosts.ID %in%
                                                                                 df.meetTransmit$hosts.ID]) > res$table.hosts$t_EIP[res$table.hosts$hosts.ID %in%
                                                                                                                                      df.meetTransmit$hosts.ID])
    }
    else {
      df.meetTransmit[, "Ptransmit"] <- applyFunctionToHosts(res,
                                                             pres.time, pTransParsed, active.hosts)
    }
    df.meetTransmit <- df.meetTransmit[df.meetTransmit[["Ptransmit"]] >
                                         0]
    if (nrow(df.meetTransmit) > 0) {
      df.meetTransmit <- df.meetTransmit[rep(seq(1, nrow(df.meetTransmit)),
                                             df.meetTransmit$number.contacts)]
      df.meetTransmit[, "Trans"] <- drawBernouilli(df.meetTransmit[["Ptransmit"]])
      df.meetTransmit <- df.meetTransmit[df.meetTransmit[["Trans"]]]
    }
  }
  return(df.meetTransmit)
}


#' @title Write newly infected function
#'
#' @description
#' Perform the tasks related to creating new lines of hosts in relevant tables.
#'
#' @param df.meetTransmit table with active transmission events, coming from \code{meetTransmit}
#' @param res an object of class \code{nosoiSimOne}.
#' @inheritParams newLine
#' @inheritParams newLineState
#'
#' @return The modified object res
#'
#' @keywords internal
##

writeInfected <- function (df.meetTransmit, res, pres.time, ParamHost)
{
  if (nrow(df.meetTransmit) > 0) {
    res$N.infected <- nrow(df.meetTransmit) + res$N.infected
    table.temp <- vector("list", nrow(df.meetTransmit))
    tempdf <- data.frame(hosts.ID = paste(res$prefix.host,
                                          res$N.infected - nrow(df.meetTransmit) + (1:nrow(df.meetTransmit)),
                                          sep = "-"), inf.by = df.meetTransmit$hosts.ID, inf.time = pres.time,
                         out.time = NA_integer_, active = TRUE)
    if (!is.null(ParamHost$t_EIP)) {
      tempdf <- cbind(tempdf, data.frame(t_EIP = unlist(ParamHost$t_EIP(nrow(df.meetTransmit)))))
    }
    res$table.hosts <- as.data.table(rbind(res$table.hosts,
                                           tempdf))
    data.table::setkey(res$table.hosts, hosts.ID)
  }
  return(res)
}
