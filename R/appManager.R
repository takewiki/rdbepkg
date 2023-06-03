#' 检验程序是否存在
#'
#' @param app_name 程序名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' app_isNew()
app_isNew  <- function(app_name ='appTpl') {
  conn = tsda::sql_getConn(token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE')
  sql <- paste0("SELECT 1  as FCount FROM t_md_userRight where FappId ='",app_name,"'")
  data = tsda::sql_select(conn,sql)
  ncount = nrow(data)
  if(ncount >0){
    res <- FALSE
  }else{
    res <- TRUE
  }

}

#' 检验程序是否存在
#'
#' @param app_name 程序名
#' @param token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' app_isNew2()
app_isNew2  <- function(app_name ='appTpl',token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE') {
  #conn = tsda::sql_getConn(token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE')
  sql <- paste0("SELECT 1  as FCount FROM t_md_userRight where FappId ='",app_name,"'")
  data = tsda::sql_select2(token = token,sql = sql)
  ncount = nrow(data)
  if(ncount >0){
    res <- FALSE
  }else{
    res <- TRUE
  }

}

#' 创建程序
#'
#' @param app_name 程序名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' app_create()
app_create <- function(app_name ='appTpl') {
  flag_new = app_isNew(app_name = app_name)
  if(flag_new){
    #创建默认用户
    conn = tsda::sql_getConn(token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE')
    sql_user =  paste0("insert into t_md_userRight
SELECT [Fuser]
      ,[Fpassword]
      ,[Fpermissions]
      ,[Fname]
      ,'",app_name,"' as [FappId]
      ,[Fdeleted]
      ,[FSesstionCount]
  FROM  t_md_userRight
  where FappId ='appTpl'")
    tsda::sql_update(conn,sql_user)
    #创建默认模块
    sql_obj <- paste0("  insert into t_md_objectRight
  select [Fshow]
      ,[Fname]
      ,[Fid]
      ,[Ficon]
      ,[Fpermissions]
      ,[Ftype]
      ,[FparentId]
      ,[Findex]
      ,'",app_name,"' as [FappId] from t_md_objectRight
  where FappId ='appTpl'")
    tsda::sql_update(conn,sql_obj)

    res = TRUE
  }else{
    res = FALSE
  }

}


#' 创建程序
#'
#' @param app_name 程序名称
#' @param token  口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' app_create2()
app_create2 <- function(app_name ='appTpl',token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE') {
  flag_new = app_isNew2(app_name = app_name,token = token)
  if(flag_new){
    #创建默认用户
    #conn = tsda::sql_getConn(token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE')
    sql_user =  paste0("insert into t_md_userRight
SELECT [Fuser]
      ,[Fpassword]
      ,[Fpermissions]
      ,[Fname]
      ,'",app_name,"' as [FappId]
      ,[Fdeleted]
      ,[FSesstionCount]
  FROM  t_md_userRight
  where FappId ='appTpl'")
    tsda::sql_insert2(token = token,sql_str = sql_user)
    #创建默认模块
    sql_obj <- paste0("  insert into t_md_objectRight
  select [Fshow]
      ,[Fname]
      ,[Fid]
      ,[Ficon]
      ,[Fpermissions]
      ,[Ftype]
      ,[FparentId]
      ,[Findex]
      ,'",app_name,"' as [FappId] from t_md_objectRight
  where FappId ='appTpl'")
    tsda::sql_insert2(token = token,sql_str = sql_obj)

    res = TRUE
  }else{
    res = FALSE
  }

}

