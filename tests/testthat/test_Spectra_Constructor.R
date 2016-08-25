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
 
 




 #' sp@header
 #' sp$CAST=rep(33, nrow(sp))
 #' sp=spc.data2header(sp,"CAST","ProjectCast", compress=T)







