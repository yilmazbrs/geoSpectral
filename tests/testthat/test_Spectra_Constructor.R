library(geoSpectral)
context("Constructor Function for Spectra class")

sp <- spc.example_spectra()

test_that("Output of Spectra() is of type 'Spectra' ", {
  expect_is(sp,"Spectra")
  expect_equal(nrow(sp), 26)
})

test_that("Conversion to/from data.frame", {
  expect_is(as(sp, "data.frame"),"data.frame")
  expect_is(as(as(sp, "data.frame"), "Spectra"), "Spectra")
})
dim(sp)
c=dim(sp)
a=spc.getwavelengths(sp)
test_that("Dimension is integer or null or equal to numbers of row and column", {
  expect_is(dim(sp),"integer")
  expect_is(sp@header, "SpcHeader" )
  expect_equal(c[1]+c[2], ncol(sp)+nrow(sp))
  expect_equal(length(a),ncol(sp))
  })
nc=dim(sp)
test_that("nuber of row and column are equal to output of ncol and nrow", {
  expect_equal(ncol(sp),nc[2] )
  expect_equal(nrow(sp),nc[1] )
  expect_is(ncol(sp),"integer")
  expect_is(nrow(sp),"integer")
})

test_that("Test for names() ", {
  
  expect_is(names(sp),"character")
  expect_equal(length(names(sp)),512)
})

hd=head(sp,7)
test_that("Tests for head()", {
  expect_equal(length(hd[,1]),7)
  expect_equal(dim(hd)[2],ncol(hd))
  expect_equal(dim(sp)[2],ncol(hd))
  expect_equal(dim(hd)[2],ncol(sp))          
  expect_is(hd,"matrix")
})

test_that("test for spc.colnames()", {
  expect_is(spc.colnames(sp),"character")
  expect_equal(length(spc.colnames(sp)),ncol(sp))
  a = "anap_300" %in% spc.colnames(sp)
  expect_equal(a,TRUE)
})

test_that("rbind test for Spectral object" ,{
  expect_equal(length(spc.rbind(sp,sp)),length(sp)*2)
})

test_that("Show Spectra",{
  expect_output(show(sp),"501 spectral channels in columns and 26 observations in rows")
  
  })

test_that("test for $ and [] ",{
  expect_is(sp$anap_300,"numeric")
  expect_output(show(sp[,"anap_300"]), "Spectra Columns:  anap_300 ...")
})

test_that("test for spc.colnames()",{
  expect_true(all("anap_409"%in%spc.colnames(sp)))
  
})

test_that("test for spc.rbinds()",{
  sp2=spc.rbind(sp,sp)
  expect_equal(nrow(sp2),(nrow(sp)*2))
  
})

test_that("test for spc.getwavelengh()",{
  wl=spc.getwavelengths(sp)
  expect_equal(length(wl), ncol(sp))
  
})


test_that("test for spc.setwavelengh()",{
  
  expect_error(spc.setwavelengths(sp) <- num <- (ncol(sp)*2))
  
})

test_that("test for spc.cname.construct()",{
  a=spc.cname.construct(sp,"Newvar")
  expect_match(a[1],"Newvar_300")
  
})

test_that("test for spc.getheader ()",{
  a=sp@header
  expect_true(all(a[1]%in%spc.getheader(sp)))
  
})


 test_that("test for spc.setheader() and guve number and new header , check it",{
          a=new("SpcHeader")
          a$Longitude=123
          spc.setheader(sp,"Station") <- a 
          expect_equal((as.numeric(sp@header[4])),123)
            
     })

 test_that("test for spc.updateheader() and give number to header and check it",{
   a=new("SpcHeader")
   a$Longitude=123
   spc.updateheader(sp,"Station")<- 11
   expect_equal((as.numeric(sp@header[1])),11)
   
 })


 test_that("test for spc.data2header() and give number to header and check it",{
   sp=spc.example_spectra()
    sp=spc.data2header(sp,"CAST")
    sp@header
    sp=spc.data2header(sp,"CAST","ProjectCast")
    sp@header
    sp$CAST=rep(33, nrow(sp))
    sp=spc.data2header(sp,"CAST","ProjectCast", compress=T)
    
     expect_match(names(sp@header[6]),"ProjectCast",ignore.case = FALSE)
     expect_equal(as.numeric(sp@header[6]),33)
 })
 
 test_that("test for spc.plot() when we put Spclist object",{
   spl=SpcList(sp@Wavelengths)
   expect_error(spc.plot(spl))
   expect_error(spc.plot.depth(spl))
   expect_error(spc.plot.time(spl))
   expect_error(spc.plot.grid(spl))
   expect_error(spc.plot.plotly(spl))
   expect_error(spc.plot.overlay(spl))
   expect_error(spc.plot.plotly(spl))
   
   
 })



 test_that("test for head()",{
   expect_equal(ncol(sp),ncol(head(sp)))
   expect_equal(sp@Spectra[1:6,3],head(sp)[,3])
   # names(sp@spectra) and names(head(sp)) are NULL why?
 })
 
 #spc.plot.grid(sp)
 #Show Traceback
 
 #Rerun with Debug
 #Error in (function (classes, fdef, mtable)  : 
 
 #       spc.plot.overlay(sp)
 #Error in (function (classes, fdef, mtable)  :    
 #Error in (function (classes, fdef, mtable)  : 
 #
 
 test_that("test for subset()",{
   expect_is(subset(sp,DEPTH<=30),"Spectra")
   expect_equal(ncol(sp),ncol(subset(sp,DEPTH<=30)))
   
 })
 
 test_that("test for spc.lapply()",{
   sp=spc.example_spectra() 
   BL = spc.makeSpcList(sp,"STATION")
   BL2=spc.lapply(BL, function(x) x=x+1)
   expect_is(BL2,"SpcList")
   
   
 })
 
 test_that("test for spc.getwavelenghts()",{
   
   expect_equal(length(spc.getwavelengths(sp)),ncol(sp))
   
   
 })
 
 
 test_that("test for spc.update()",{
   spc.updateheader(sp,"Station")<-11
   expect_equal(as.numeric(sp@header[1]),11)
   
   
 })
 
 
 test_that("test for spc.gethader()",{
   aa=sp@header
   expect_equal(names(aa),names(spc.getheader(sp)))
   #expect_output(names(aa),names(spc.getheader(sp)))
   
 })
 
 
 test_that("test for spc.getinvalid()",{
   abc=spc.getinvalid.idx(sp)
   expect_true(all(abc))
   
   
 })
 
 test_that("test for spc.plot()",{
   
   expect_error( spc.plot(sp@Spectra))
   
   
 })
 

 
 test_that("test for spc.makeSpcList()",{
   BL=spc.makeSpcList(sp,"STATION")
   expect_equal( sum(sapply(BL,nrow)),nrow(sp))
   
   
 })
 
 test_that("test for spc.setinvalid.idx()",{
   vld = rep(TRUE,26)
   vld[1:5]<-FALSE
   expect_false( all(spc.setinvalid.idx(sp)<-vld))
   
   
 })


 test_that("test for spc.import.text()",{
   sp=spc.example_spectra()
   spc.export.text(sp,filename="anap.txt")
   aa=spc.import.text("anap.txt")
   expect_true(class(aa)=="Spectra")
   
   
 }) 
 
 test_that("test for spc.plot.time()",{
  
   expect_warning(spc.plot.time(sp,maxSp=50), NA)
   expect_warning(spc.plot.time(sp,maxSp=500), NA) 
   
 }) 
 
   test_that("test for spc.plot.time()",{
   
   expect_warning(spc.plot.depth(sp,maxSp=11), NA)
   expect_warning(spc.plot.depth(sp,maxSp=100), NA) 
   
  }) 
 
 
  # test_that("test for spc.plot.time.plotly()",{
   #  
    # expect_massage(spc.plot.time.plotly(sp,plot.max = 5),NA)
     #expect_massage(spc.plot.time.plotly(sp,plot.max = 45),NA) 
    #
   #}) 
 
   test_that("test for names() in SpcList",{
     sp <- spc.example_spectra()
     BL = spc.makeSpcList(sp,"STATION")
     expect_equal(length(levels(sp$STATION)), length(names(BL)))
     
     
   }) 
   
 
 
 
 
 
 
 
 
