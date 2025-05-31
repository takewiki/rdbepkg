#' 数据库选择器
#'
#' @param FAppId 程序ID
#' @param FType 类型
#' @param FRunEnv 运行虞翊时尚发型
#'
#' @return 返回值
#' @import tsda
#' @export
#'
#' @examples
#' dbConfig()
dbConfig <- function(FAppId='vmdms',
                     FType='ERP' ,
                     FRunEnv='PRD') {
sql = paste0("
             select  FToken from t_db_config
            where FAppId='",FAppId,"' and FType='",FType,"' and FRunEnv='",FRunEnv,"'")
data = sql_select(conn_rds('willingox'),sql)
ncount = nrow(data)
if (ncount){
  res = data$FToken
}else{
  res = NULL
}
return(res)


}
