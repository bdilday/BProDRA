

#' @import RMySQL
#'
#' @importFrom readr read_csv

get_events_data <- function(year_id) {
  db.connection.info = read_csv('D:/one-offs/mlb/makeHOFData/secret.txt')
  conn = dbConnect(MySQL(),
                   user=db.connection.info$user,
                   password=db.connection.info$password,
                   dbname=db.connection.info$dbname)

  query1 <- paste("select * from retrosheet.events where "
                  ," (event_cd<=3 or event_cd>=20 or (event_cd>=14 and event_cd<=16)) "
                  ," and year_id=", year_id)

  rs <- dbSendQuery(conn, query1)
  res1 = dbFetch(rs, n=-1)
}

export_events_data <- function(year_id, .data=NULL) {
  if (is.null(.data)) {
    .data <- get_events_data(year_id)
  }
  df_name <- sprintf("dra_events_%d", year_id)
  ofile <- sprintf("data/%s.RData", df_name)
  assign(df_name, .data)
  save(list=df_name, file=ofile)
}

