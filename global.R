## global.R ##
# 加载R包-----
enableBookmarking(store = "url")
library(shiny);
library(shinydashboard);
library(tsda);
library(tsdo);
library(tsui);
library(DTedit)

# 设置引入页-----
source('00_data.R',encoding = 'utf-8');
source('topbarMenu.R',encoding = 'utf-8');
source('sideBarSetting.R',encoding = 'utf-8');
source('01_row_body.R',encoding = 'utf-8');
source('02_column_body.R',encoding = 'utf-8');
source('03_book_body.R',encoding = 'utf-8');
source('04_series_body.R',encoding = 'utf-8');
source('05_majority_body.R',encoding = 'utf-8');
source('06_tutor_body.R',encoding = 'utf-8');
source('99_sysSetting_body.R',encoding = 'utf-8');
source('workAreaSetting.R',encoding = 'utf-8');

conn <-conn_rds('nsic')
getBooks <- function(table='t_tsp_ques') {
  sql <- sql_gen_select(conn,table = table)
  books <-sql_select(conn,sql)
  #针对进行格式化处理
  #如果出来新的数据类型，需要添加格式化函数
  #请修改format_to_dtedit  --formatter.R
  fieldList <-sql_fieldInfo(conn,table)
  for (i in 1:ncol(books)){
    type <-fieldList[i,'FTypeName']
    books[,i] <-format_to_dtedit(type)(books[,i])
    
  }
  
  return(books)
}
# 定义加急处理
getBooks1 <- function(table='t_tsp_ques') {
  sql <- sql_gen_select(conn,table = table)
  #添加内容
  sql <- paste0(sql,'  where FPushStatus = 0 and FPriorCount >0
order by FPriorCount desc')
  books <-sql_select(conn,sql)
  #针对进行格式化处理
  #如果出来新的数据类型，需要添加格式化函数
  #请修改format_to_dtedit  --formatter.R
  fieldList <-sql_fieldInfo(conn,table)
  for (i in 1:ncol(books)){
    type <-fieldList[i,'FTypeName']
    books[,i] <-format_to_dtedit(type)(books[,i])
    
  }
  
  return(books)
}

#普通处理------
getBooks2 <- function(table='t_tsp_ques') {
  sql <- sql_gen_select(conn,table = table)
  #添加内容
  sql <- paste0(sql,'  where FPushStatus = 0 and FPriorCount = 0
')
  books <-sql_select(conn,sql)
  #针对进行格式化处理
  #如果出来新的数据类型，需要添加格式化函数
  #请修改format_to_dtedit  --formatter.R
  fieldList <-sql_fieldInfo(conn,table)
  for (i in 1:ncol(books)){
    type <-fieldList[i,'FTypeName']
    books[,i] <-format_to_dtedit(type)(books[,i])
    
  }
  
  return(books)
}

#今日处理------
getBooks3A <- function(table='t_tsp_ques') {
  sql <- sql_gen_select(conn,table = table)
  #添加内容,主管已提供答案，但是客服还没有
  sql <- paste0(sql,"  where FPushStatus = 1 and FPullStatus =0   and  FTspDate = '",as.character(Sys.Date()),"'"
)
  books <-sql_select(conn,sql)
  #针对进行格式化处理
  #如果出来新的数据类型，需要添加格式化函数
  #请修改format_to_dtedit  --formatter.R
  fieldList <-sql_fieldInfo(conn,table)
  for (i in 1:ncol(books)){
    type <-fieldList[i,'FTypeName']
    books[,i] <-format_to_dtedit(type)(books[,i])
    
  }
  
  return(books)
}
getBooks3B <- function(table='t_tsp_ques') {
  sql <- sql_gen_select(conn,table = table)
  #添加内容,主管已提供答案，但是客服还没有
  sql <- paste0(sql,"  where FPushStatus = 1   and  FTspDate = '",as.character(Sys.Date()),"'"
  )
  books <-sql_select(conn,sql)
  #针对进行格式化处理
  #如果出来新的数据类型，需要添加格式化函数
  #请修改format_to_dtedit  --formatter.R
  fieldList <-sql_fieldInfo(conn,table)
  for (i in 1:ncol(books)){
    type <-fieldList[i,'FTypeName']
    books[,i] <-format_to_dtedit(type)(books[,i])
    
  }
  
  return(books)
}




#全部处理查询------
getBooks4 <- function(table='t_tsp_ques') {
  sql <- sql_gen_select(conn,table = table)
  #添加内容
  sql <- paste0(sql,"  where FPushStatus = 1 ")
  books <-sql_select(conn,sql)
  #针对进行格式化处理
  #如果出来新的数据类型，需要添加格式化函数
  #请修改format_to_dtedit  --formatter.R
  fieldList <-sql_fieldInfo(conn,table)
  for (i in 1:ncol(books)){
    type <-fieldList[i,'FTypeName']
    books[,i] <-format_to_dtedit(type)(books[,i])
    
  }
  
  return(books)
}




getMax_id <-function(conn,table='t_tsp_ques',id_var='FId'){
  sql <- sql_gen_select(conn,table,id_var)
  #print(sql)
  r <-sql_select(conn,sql)
  res <- max(as.integer(r[,id_var]))+1
  return(res)
}

##### Callback functions.


books.insert.callback <- function(data, row ,table='t_tsp_ques',f=getBooks,id_var='FId') {
  sql_header <- sql_gen_insert(conn,table)
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-nrow(fieldList)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Insert <-fieldList[i,'FFieldName']
    type <-fieldList[i,'FTypeName']
    if(col_Insert == id_var){
      res[i] <-paste0(' ',getMax_id(conn,table,id_var),' ')
    }else{
      res[i] <- format_to_sqlInsert(type)(data[row,col_Insert])
    }
    
  }
  sql_body <- paste0(res,collapse = ',')
  query <-paste0(sql_header,sql_body,")")
  
  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}

books.insert.callback1 <- function(data, row ,table='t_tsp_ques',f=getBooks1,id_var='FId') {
  sql_header <- sql_gen_insert(conn,table)
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-nrow(fieldList)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Insert <-fieldList[i,'FFieldName']
    type <-fieldList[i,'FTypeName']
    if(col_Insert == id_var){
      res[i] <-paste0(' ',getMax_id(conn,table,id_var),' ')
    }else{
      res[i] <- format_to_sqlInsert(type)(data[row,col_Insert])
    }
    
  }
  sql_body <- paste0(res,collapse = ',')
  query <-paste0(sql_header,sql_body,")")
  
  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}
books.insert.callback2 <- function(data, row ,table='t_tsp_ques',f=getBooks2,id_var='FId') {
  sql_header <- sql_gen_insert(conn,table)
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-nrow(fieldList)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Insert <-fieldList[i,'FFieldName']
    type <-fieldList[i,'FTypeName']
    if(col_Insert == id_var){
      res[i] <-paste0(' ',getMax_id(conn,table,id_var),' ')
    }else{
      res[i] <- format_to_sqlInsert(type)(data[row,col_Insert])
    }
    
  }
  sql_body <- paste0(res,collapse = ',')
  query <-paste0(sql_header,sql_body,")")
  
  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}

books.insert.callback3A <- function(data, row ,table='t_tsp_ques',f=getBooks3,id_var='FId') {
  sql_header <- sql_gen_insert(conn,table)
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-nrow(fieldList)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Insert <-fieldList[i,'FFieldName']
    type <-fieldList[i,'FTypeName']
    if(col_Insert == id_var){
      res[i] <-paste0(' ',getMax_id(conn,table,id_var),' ')
    }else{
      res[i] <- format_to_sqlInsert(type)(data[row,col_Insert])
    }
    
  }
  sql_body <- paste0(res,collapse = ',')
  query <-paste0(sql_header,sql_body,")")
  
  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}

books.insert.callback3B <- function(data, row ,table='t_tsp_ques',f=getBooks3,id_var='FId') {
  sql_header <- sql_gen_insert(conn,table)
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-nrow(fieldList)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Insert <-fieldList[i,'FFieldName']
    type <-fieldList[i,'FTypeName']
    if(col_Insert == id_var){
      res[i] <-paste0(' ',getMax_id(conn,table,id_var),' ')
    }else{
      res[i] <- format_to_sqlInsert(type)(data[row,col_Insert])
    }
    
  }
  sql_body <- paste0(res,collapse = ',')
  query <-paste0(sql_header,sql_body,")")
  
  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}
books.insert.callback4 <- function(data, row ,table='t_tsp_ques',f=getBooks4,id_var='FId') {
  sql_header <- sql_gen_insert(conn,table)
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-nrow(fieldList)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Insert <-fieldList[i,'FFieldName']
    type <-fieldList[i,'FTypeName']
    if(col_Insert == id_var){
      res[i] <-paste0(' ',getMax_id(conn,table,id_var),' ')
    }else{
      res[i] <- format_to_sqlInsert(type)(data[row,col_Insert])
    }
    
  }
  sql_body <- paste0(res,collapse = ',')
  query <-paste0(sql_header,sql_body,")")
  
  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}


books.update.callback <- function(data, olddata, row,
                                  table='t_tsp_ques',
                                  f=getBooks,
                                  edit.cols = c('FQues','FAnsw'),
                                  id_var='FId') 
{
  sql_header <- sql_gen_update(table);
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-length(edit.cols)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Update <-edit.cols[i]
    #col_Insert <-fieldList[fieldList$,'FFieldName']
    type <-fieldList[fieldList$FFieldName == col_Update,'FTypeName']
    res[i] <- paste0(' ',col_Update,' = ',format_to_sqlUpdate(type)(data[row,col_Update]))
    
    
  }
  sql_body <- paste0(res,collapse = ',')
  sql_tail <-paste0(' where ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_body,sql_tail)
  
  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}

#紧急处理
books.update.callback1 <- function(data, olddata, row,
                                  table='t_tsp_ques',
                                  f=getBooks1,
                                  edit.cols = c('FQues','FAnsw'),
                                  id_var='FId') 
{
  sql_header <- sql_gen_update(table);
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-length(edit.cols)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Update <-edit.cols[i]
    #col_Insert <-fieldList[fieldList$,'FFieldName']
    type <-fieldList[fieldList$FFieldName == col_Update,'FTypeName']
    res[i] <- paste0(' ',col_Update,' = ',format_to_sqlUpdate(type)(data[row,col_Update]))
    
    
  }
  sql_body <- paste0(res,collapse = ',')
  sql_tail <-paste0(' where ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_body,sql_tail)
  
  print(query) # For debugging
  sql_update(conn, query)
  
  #处理状态为1及为当天日期
  sql_status <-paste0("update  t_tsp_ques set FPushStatus =1 ,FTspDate ='",as.character(Sys.Date()),"'  where FId  = ",data[row,id_var]," and FPushStatus = 0 ")
  #print(sql_status)
  sql_update(conn,sql_status)
  return(f())
}


books.update.callback2 <- function(data, olddata, row,
                                   table='t_tsp_ques',
                                   f=getBooks2,
                                   edit.cols = c('FQues','FAnsw'),
                                   id_var='FId') 
{
  sql_header <- sql_gen_update(table);
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-length(edit.cols)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Update <-edit.cols[i]
    #col_Insert <-fieldList[fieldList$,'FFieldName']
    type <-fieldList[fieldList$FFieldName == col_Update,'FTypeName']
    res[i] <- paste0(' ',col_Update,' = ',format_to_sqlUpdate(type)(data[row,col_Update]))
    
    
  }
  sql_body <- paste0(res,collapse = ',')
  sql_tail <-paste0(' where ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_body,sql_tail)
  
  print(query) # For debugging
  sql_update(conn, query)
  #处理状态为1及为当天日期
  sql_status <-paste0("update  t_tsp_ques set FPushStatus =1 ,FTspDate ='",as.character(Sys.Date()),"'  where FId  = ",data[row,id_var]," and FPushStatus = 0 ")
  #print(sql_status)
  sql_update(conn,sql_status)
  return(f())
}

books.update.callback3A <- function(data, olddata, row,
                                   table='t_tsp_ques',
                                   f=getBooks3,
                                   edit.cols = c('FQues','FAnsw'),
                                   id_var='FId') 
{
  sql_header <- sql_gen_update(table);
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-length(edit.cols)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Update <-edit.cols[i]
    #col_Insert <-fieldList[fieldList$,'FFieldName']
    type <-fieldList[fieldList$FFieldName == col_Update,'FTypeName']
    res[i] <- paste0(' ',col_Update,' = ',format_to_sqlUpdate(type)(data[row,col_Update]))
    
    
  }
  sql_body <- paste0(res,collapse = ',')
  sql_tail <-paste0(' where ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_body,sql_tail)
  
  print(query) # For debugging
  sql_update(conn, query)
  #更新状态
  sql_status <-paste0("update  t_tsp_ques set  FUpdateStatus = 1   where FId  = ",data[row,id_var]," and FPushStatus = 1 ")
  print(sql_status)
  sql_update(conn,sql_status)
  return(f())
}

books.update.callback3B <- function(data, olddata, row,
                                   table='t_tsp_ques',
                                   f=getBooks3,
                                   edit.cols = c('FQues','FAnsw'),
                                   id_var='FId') 
{
  sql_header <- sql_gen_update(table);
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-length(edit.cols)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Update <-edit.cols[i]
    #col_Insert <-fieldList[fieldList$,'FFieldName']
    type <-fieldList[fieldList$FFieldName == col_Update,'FTypeName']
    res[i] <- paste0(' ',col_Update,' = ',format_to_sqlUpdate(type)(data[row,col_Update]))
    
    
  }
  sql_body <- paste0(res,collapse = ',')
  sql_tail <-paste0(' where ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_body,sql_tail)
  
  print(query) # For debugging
  sql_update(conn, query)
  #更新状态
  sql_status <-paste0("update  t_tsp_ques set  FUpdateStatus = 1   where FId  = ",data[row,id_var]," and FPushStatus = 1 ")
  print(sql_status)
  sql_update(conn,sql_status)
  return(f())
}


books.update.callback4 <- function(data, olddata, row,
                                   table='t_tsp_ques',
                                   f=getBooks4,
                                   edit.cols = c('FQues','FAnsw'),
                                   id_var='FId') 
{
  sql_header <- sql_gen_update(table);
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-length(edit.cols)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Update <-edit.cols[i]
    #col_Insert <-fieldList[fieldList$,'FFieldName']
    type <-fieldList[fieldList$FFieldName == col_Update,'FTypeName']
    res[i] <- paste0(' ',col_Update,' = ',format_to_sqlUpdate(type)(data[row,col_Update]))
    
    
  }
  sql_body <- paste0(res,collapse = ',')
  sql_tail <-paste0(' where ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_body,sql_tail)
  
  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}

books.delete.callback <- function(data, row ,table ='t_tsp_ques',f=getBooks,id_var='FId') {
  sql_header <- sql_gen_delete(table);
  sql_tail <-paste0('  ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_tail)
  
  #query <- paste0("DELETE FROM  ",table,"  WHERE id = ", data[row,]$id)
  print(query)
  sql_update(conn, query)
  return(f())
}

books.delete.callback1 <- function(data, row ,table ='t_tsp_ques',f=getBooks1,id_var='FId') {
  sql_header <- sql_gen_delete(table);
  sql_tail <-paste0('  ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_tail)
  
  #query <- paste0("DELETE FROM  ",table,"  WHERE id = ", data[row,]$id)
  print(query)
  sql_update(conn, query)
  return(f())
}

books.delete.callback2 <- function(data, row ,table ='t_tsp_ques',f=getBooks2,id_var='FId') {
  sql_header <- sql_gen_delete(table);
  sql_tail <-paste0('  ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_tail)
  
  #query <- paste0("DELETE FROM  ",table,"  WHERE id = ", data[row,]$id)
  print(query)
  sql_update(conn, query)
  return(f())
}
books.delete.callback3A <- function(data, row ,table ='t_tsp_ques',f=getBooks3,id_var='FId') {
  sql_header <- sql_gen_delete(table);
  sql_tail <-paste0('  ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_tail)
  
  #query <- paste0("DELETE FROM  ",table,"  WHERE id = ", data[row,]$id)
  print(query)
  sql_update(conn, query)
  return(f())
}


books.delete.callback3B <- function(data, row ,table ='t_tsp_ques',f=getBooks3,id_var='FId') {
  sql_header <- sql_gen_delete(table);
  sql_tail <-paste0('  ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_tail)
  
  #query <- paste0("DELETE FROM  ",table,"  WHERE id = ", data[row,]$id)
  print(query)
  sql_update(conn, query)
  return(f())
}

books.delete.callback4 <- function(data, row ,table ='t_tsp_ques',f=getBooks4,id_var='FId') {
  sql_header <- sql_gen_delete(table);
  sql_tail <-paste0('  ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_tail)
  
  #query <- paste0("DELETE FROM  ",table,"  WHERE id = ", data[row,]$id)
  print(query)
  sql_update(conn, query)
  return(f())
}











