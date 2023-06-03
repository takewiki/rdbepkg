#' 重置用户密码
#'
#' @param token 口令
#' @param FappId 程序ID
#' @param Fuser 用户名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' userPassword_reset()
userPassword_reset <- function(token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE',FappId ='jhdms',Fuser ='马斌') {

  sql <- paste0("update a set a.Fpassword='faf6db350ba0347de5915f134dd6df2'    FROM t_md_userRight a
where FappId ='",FappId,"' and Fuser ='",Fuser,"'")
  tsda::sql_update2(token =token ,sql_str = sql)


}



#' 用户禁用
#'
#' @param token  口令
#' @param FappId 程序ID
#' @param Fuser 用户名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' user_disable()
user_disable <- function(token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE',FappId ='jhdms',Fuser ='马斌') {

  sql <- paste0("update a set Fdeleted = 1  FROM t_md_userRight a
where FappId ='",FappId,"' and Fuser ='",Fuser,"'")
  tsda::sql_update2(token = token,sql_str = sql)

}


#' 用户取消禁用
#'
#' @param token  口令
#' @param FappId 程序ID
#' @param Fuser 用户名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' user_disable()
user_enable <- function(token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE',FappId ='jhdms',Fuser ='马斌') {

  sql <- paste0("update a set Fdeleted = 0  FROM t_md_userRight a
where FappId ='",FappId,"' and Fuser ='",Fuser,"'")
  tsda::sql_update2(token = token,sql_str = sql)

}
