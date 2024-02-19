library(rvest)
library(ggplot2)
library(ggrepel)

workdir="E:\\MIUPDATE\\"

fmtnum=function(xi,dig=2){
  return(paste0(paste0(rep("0",dig-nchar(xi)),collapse = ""),xi))
}

get_update_info=function(pname="k60p",purl="https://miuiver.com/tag/socrates-stable-rom/"){
  webpage=read_html(purl) %>% html_node("table")
  
  webtable=webpage %>% html_table(convert=F,fill=T)
  webtable$tablelink=webpage %>% html_elements("a") %>% html_attr("href")
  print(webtable)
  updata=data.frame()
  print(paste0(nrow(webtable)," updates."))
  for(xi in 1:nrow(webtable)){
    iweb=webtable[xi,]
    wtime=runif(1,min=3,max=6)
    print(paste0(iweb$ROM版本,"->",iweb$tablelink,", waiting ",wtime,"s."))
    
    tupdata=data.frame()
    tmpfile=paste0(workdir,"tmp\\",pname,"_",fmtnum(xi),".Rds")
    if(file.exists(tmpfile)){
      tupdata=readRDS(file=tmpfile)
      if(tupdata$INFO==""){
        print("Remove empty file.")
        file.remove(tmpfile)
        #Sys.sleep(wtime)
        iwpg=read_html(iweb$tablelink) 
        itbl=iwpg %>% html_node("table") %>% html_table()
        upinfo=as.vector(iwpg %>% html_elements("ul") %>% html_text())
        upinfo=upinfo[c(-1,-2)]
        if(length(upinfo)>2) upinfo=upinfo[c(-3)]
        upinfo=upinfo[-length(upinfo)]
        upinfo=paste(upinfo,collapse="；")
        tupdata=data.frame(PHONE=itbl$X2[1],OSVER=itbl$X2[2],MIUIVER=itbl$X2[3],ANDROIDVER=itbl$X2[4],RELEASETIME=itbl$X2[7],INFO=upinfo)
        saveRDS(file=tmpfile,tupdata)
      }
    }else{
      try({
        #Sys.sleep(wtime)
        iwpg=read_html(iweb$tablelink) 
        itbl=iwpg %>% html_node("table") %>% html_table()
        upinfo=as.vector(iwpg %>% html_elements("ul") %>% html_text())
        upinfo=upinfo[c(-1,-2)]
        if(length(upinfo)>2) upinfo=upinfo[c(-3)]
        upinfo=upinfo[-length(upinfo)]
        upinfo=paste(upinfo,collapse="；")
        tupdata=data.frame(PHONE=itbl$X2[1],OSVER=itbl$X2[2],MIUIVER=itbl$X2[3],ANDROIDVER=itbl$X2[4],RELEASETIME=itbl$X2[7],INFO=upinfo)
        saveRDS(file=tmpfile,tupdata)
      })
    }
    
    updata=rbind(updata,tupdata)
  }
  
  write.csv(file=paste0(workdir,"MIUPDATE_",toupper(pname),".csv"),updata,fileEncoding = "GB2312")
}

update_times_vs_price=function(){
  pricedata=read.csv(paste0(workdir,"PRICE.csv"),header=T,sep="\t")
  pricedata$UPDATETIME=0
  for(xi in 1:nrow(pricedata)){
    updatetime=nrow(read.csv(file=paste0(workdir,"MIUPDATE_",pricedata$TYPE[xi],".csv"),header=T,fileEncoding = "GB2312"))
    pricedata$UPDATETIME[xi]=updatetime
  }
  
  p=ggplot()
  p=p+geom_smooth(aes(x=PRICE,y=UPDATETIME),pricedata,linewidth=1,color="pink")
  p=p+geom_point(aes(x=PRICE,y=UPDATETIME,size=PRICE/UPDATETIME),pricedata,color="orange")
  p=p+geom_text_repel(aes(x=PRICE,y=UPDATETIME,label=TYPE,max.overlaps=5),pricedata,color="black",size=4)
  p=p+theme_bw()
  p=p+xlab("起售价")+ylab("更新次数")
  p
  
}

#K
get_update_info("k40g","https://miuiver.com/tag/ares-stable-rom/")
get_update_info("k40p","https://miuiver.com/tag/haydn-stable-rom/")
get_update_info("k50p","https://miuiver.com/tag/matisse-stable-rom/")
get_update_info("k50u","https://miuiver.com/tag/diting-stable-rom/")
get_update_info("k60p","https://miuiver.com/tag/socrates-stable-rom/")


#NOTE
get_update_info("n09p","https://miuiver.com/tag/gauguin-stable-rom/")
get_update_info("n10p","https://miuiver.com/tag/chopin-stable-rom/")
get_update_info("n11p","https://miuiver.com/tag/pissarro-stable-rom/")
get_update_info("n12p","https://miuiver.com/tag/ruby-stable-rom/")
get_update_info("n13p","https://miuiver.com/tag/zircon-stable-rom/")

#数字
get_update_info("m06n","https://miuiver.com/tag/sagit-stable-rom/")
get_update_info("m08n","https://miuiver.com/tag/dipper-stable-rom/")
get_update_info("m08trans","https://miuiver.com/tag/ursa-stable-rom/")
get_update_info("m09n","https://miuiver.com/tag/cepheus-stable-rom/")
get_update_info("m10p","https://miuiver.com/tag/cmi-stable-rom/")
get_update_info("m11n","https://miuiver.com/tag/venus-stable-rom/")
get_update_info("m12n","https://miuiver.com/tag/cupid-stable-rom/")
get_update_info("m12p","https://miuiver.com/tag/zeus-stable-rom/")
get_update_info("m12p_mtk","https://miuiver.com/tag/daumier-stable-rom/")
get_update_info("m12s","https://miuiver.com/tag/mayfly-stable-rom/")
get_update_info("m12sp","https://miuiver.com/tag/unicorn-stable-rom/")
get_update_info("m13n","https://miuiver.com/tag/fuxi-stable-rom/")
get_update_info("m13p","https://miuiver.com/tag/nuwa-stable-rom/")

#ultra
get_update_info("m10u","https://miuiver.com/tag/cas-stable-rom/")
get_update_info("m11u","https://miuiver.com/tag/star-stable-rom/")
get_update_info("m12u","https://miuiver.com/tag/thor-stable-rom/")
get_update_info("m13u","https://miuiver.com/tag/ishtar-stable-rom/")
#fold
get_update_info("mxf1","https://miuiver.com/tag/cetus-stable-rom/")
get_update_info("mxf2","https://miuiver.com/tag/zizhan-stable-rom/")
get_update_info("mxf3","https://miuiver.com/tag/babylon-stable-rom/")

#civi
get_update_info("civi1","https://miuiver.com/tag/mona-stable-rom/")
get_update_info("civi1s","https://miuiver.com/tag/zijin-stable-rom/")
get_update_info("civi2","https://miuiver.com/tag/ziyi-stable-rom/")
get_update_info("civi3","https://miuiver.com/tag/yuechu-stable-rom/")
get_update_info("cc9p","https://miuiver.com/tag/tucana-stable-rom/")

