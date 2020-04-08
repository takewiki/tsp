

#shinyserver start point----
 shinyServer(function(input, output,session) {
   
   books <- getBooks()
   books1 <- getBooks1()
   books2 <- getBooks2()
   books3A <- getBooks3A()
   books3B <- getBooks3B()
   books4 <- getBooks4()
   #print(books)
   dtedit2(input, output,
           name = 'tcp_prior1',
           thedata = books1,
           edit.cols = c('FQues','FAnsw'),
           edit.label.cols = c('问题','答案'),
           input.types = c(FAnsw='textAreaInput'),
           #input.choices = list(fname = unique(unlist(books$fname))),
           view.cols = c('FId','FPriorCount','FQues','FAnsw'),
           view.captions = c('序号','催单次数','问题','答案'),
           show.insert = F,
           show.copy = F,
           show.delete = F,
           title.edit = '加急处理',
           label.edit = '加急处理',
           callback.update = books.update.callback1,
           callback.insert = books.insert.callback1,
           callback.delete = books.delete.callback1)
   dtedit2(input, output,
           name = 'tcp_prior2',
           thedata = books2,
           edit.cols = c('FQues','FAnsw'),
           edit.label.cols = c('问题','答案'),
           input.types = c(FAnsw='textAreaInput'),
           #input.choices = list(fname = unique(unlist(books$fname))),
           view.cols = c('FId','FQues','FAnsw'),
           view.captions = c('序号','问题','答案'),
           show.insert = F,
           show.copy = F,
           show.delete = F,
           title.edit = '普通处理',
           label.edit = '普通处理',
           callback.update = books.update.callback2,
           callback.insert = books.insert.callback2,
           callback.delete = books.delete.callback2)
   dtedit2(input, output,
           name = 'tcp_prior3A',
           thedata = books3A,
           edit.cols = c('FQues','FAnsw'),
           edit.label.cols = c('问题','答案'),
           input.types = c(FAnsw='textAreaInput'),
           #input.choices = list(fname = unique(unlist(books$fname))),
           view.cols = c('FId','FQues','FAnsw'),
           view.captions = c('序号','问题','答案'),
           show.insert = F,
           show.copy = F,
           show.delete = F,
           show.update=T,
           title.edit = '修改窗口',
           label.edit = '修改',
           callback.update = books.update.callback3,
           callback.insert = books.insert.callback3,
           callback.delete = books.delete.callback3)
   dtedit2(input, output,
           name = 'tcp_prior3B',
           thedata = books3B,
           edit.cols = c('FQues','FAnsw'),
           edit.label.cols = c('问题','答案'),
           input.types = c(FAnsw='textAreaInput'),
           #input.choices = list(fname = unique(unlist(books$fname))),
           view.cols = c('FId','FQues','FAnsw'),
           view.captions = c('序号','问题','答案'),
           show.insert = F,
           show.copy = F,
           show.delete = F,
           show.update=F,
           title.edit = '修改窗口',
           label.edit = '修改',
           callback.update = books.update.callback3,
           callback.insert = books.insert.callback3,
           callback.delete = books.delete.callback3)
   dtedit2(input, output,
           name = 'tcp_prior4',
           thedata = books4,
           edit.cols = c('FQues','FAnsw'),
           edit.label.cols = c('问题','答案'),
           input.types = c(FAnsw='textAreaInput'),
           #input.choices = list(fname = unique(unlist(books$fname))),
           view.cols = c('FId','FQues','FAnsw'),
           view.captions = c('序号','问题','答案'),
           show.insert = F,
           show.copy = F,
           show.delete = F,
           show.update=F,
           callback.update = books.update.callback4,
           callback.insert = books.insert.callback4,
           callback.delete = books.delete.callback4)
   
  
})
