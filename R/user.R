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

  sql <- paste0("update a set a.Fpassword='5faf6db350ba0347de5915f134dd6df2'    FROM t_md_userRight a
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



#' 查询用户的查看数据范围
#'
#' @param token 口令
#' @param FappId 程序
#' @param Fuser 用户
#'
#' @return 返回值
#' @export
#'
#' @examples
#' userDataRange_query()
userDataRange_query <- function(token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE',FappId ='jhdms',Fuser ='马斌') {

  sql <- paste0("select  Fuser,Femail,Fphone,FBusinessMan,Fcompany,Fdepartment,FBusinessGroup,FCompanyScope,FDataScope  from rds_vw_md_userInfo
where FappId ='",FappId,"' and Fuser ='",Fuser,"'")
  userInfo = tsda::sql_select2(token = token,sql = sql)
  ncount =nrow(userInfo)
  if(ncount>0){
    sql2 <- paste0("select  Fuser,Femail,Fphone,FBusinessMan,Fcompany,Fdepartment,FBusinessGroup,FCompanyScope,FDataScope from rds_vw_md_userInfo
where FappId ='",FappId,"'")
    if(userInfo$FCompanyScope == '所有公司'){
      sql_Fcompany = " "

    }else{
      sql_Fcompany = paste0("  and  Fcompany ='",userInfo$Fcompany,"' ")
    }

    if(userInfo$FDataScope == '本人'){
      sql_data = paste0(" and Fuser ='",Fuser,"'")
    }

    if(userInfo$FDataScope == '本业务组'){
      sql_data = paste0(" and FBusinessGroup ='",userInfo$FBusinessGroup,"'")
    }

    if(userInfo$FDataScope == '本部门'){
      sql_data = paste0(" and Fdepartment ='",userInfo$Fdepartment,"'")
    }

    if(userInfo$FDataScope == '所有部门'){
      sql_data = paste0(" ")
    }

    sql_all = paste0(sql2,sql_Fcompany,sql_data)
    print(sql_all)

    res = tsda::sql_select2(token =token,sql = sql_all )


  }else{
    res = userInfo
  }

  return(res)



}


#' 插入用户数据判断
#'
#' @param token 口令
#' @param FappId 程序ID
#' @param Fuser 用户
#'
#' @return 返回列表
#' @export
#'
#' @examples
#' userType_check()
userType_check <- function(token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE',FappId ='rdsdms',Fuser ='朱琼'){

  sql <- paste0("select Fpermissions,Fcompany,FCompanyScope,FDataScope  from rds_vw_md_userInfo2
where FappId = '",FappId,"' and Fuser='",Fuser,"'")
  data = tsda::sql_select2(token = token,sql = sql)
  ncount = nrow(data)
  if(ncount){
    if(tsdo::left(data$Fpermissions,2) == '客户'){
      res = list(status='customer',data=data$Fcompany,dataScope = data$FDataScope)
    }else{
      res = list(status='rds',data='1')
    }
  }else{
     res = list(status='rds',data='-1')
}
  return(res)

}



#' 用户格式化列表
#'
#' @param token 口令
#' @param FappId 程序
#' @param groups 分组
#'
#' @return 返回列表
#' @export
#'
#' @examples
#' userGroup_formatter()
userGroup_formatter <- function(token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE',FappId ='rdsdms',groups){

  group_sql = tsdo::sql_str(groups)
  sql <- paste0("select  Fuser,Fpermissions  from rds_vw_md_userInfo2
where FappId ='",FappId,"' and Fuser in (",group_sql,")")
  data = tsda::sql_select2(token = token,sql = sql)
  ncount = nrow(data)
  if(ncount){
    data$srcImg <- 'rds.png'
    data[tsdo::left(data$Fpermissions,2) =='客户','srcImg'] <-'customer.png'
    res = data.frame(id=data$Fuser,content=paste0("<img src = '",data$srcImg,"',height='20',width='20'>",data$Fuser,"</img>"))
    return(res)
     }


}






#' 查询用户邮箱
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
userMail_query <- function(token = '36F0DB19-AC55-4062-B2DA-39DC39B297BE',FappId ='jhdms',Fuser ='马斌') {

  sql <- paste0("select Fvalue from t_md_userInfo where Fkey='Femail' and FappId='",FappId,"' and Fuser='",Fuser,"'
and Fvalue <>'dms@jaour.com'")
  data = tsda::sql_select2(token =token ,sql = sql)
  ncount = nrow(data)
  if (ncount){
    res = data$Fvalue[1]
  }else{
    res = NULL
  }
  return(res)


}
