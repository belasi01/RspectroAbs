# process.replicates.R is a routine that average the replicates
# of Ap or Anap that were obtained using two different volumes.
# Three methods are used to calculate the beta factor.
# The routine produces three plots:
#         1. A plot of all replicates with three a_spectra for each beta methods
#         2. A plot showing the average af replicate A and B for each methods
#         3. A plot showing the average of all replicates and methods (N=6)
#             with an eveloppe corresponding to 1.96*s.dev.
#         4. A plot showing Ap, Anap and Aph based on the averages of plot #3
#
#   The data in the third plot and Aph are saved in an RData file, which give on file
#   per discrete water sample.
#
#

process.replicate <- function(path, path.png, ID, Station, SpecType, Repl, Depth){

  nRepl = length(Repl)
  Ap_list = list()
  for (i in 1:nRepl) {
    basename = paste(ID,"_" , Repl[i] ,"_", SpecType, sep="")
    filen = paste(path,basename,".RData", sep="")
    print(filen)
    load(filen)
    Ap_list[[i]] <- Ap
  }

  Wl = Ap_list[[1]]$Lambda

  # Average the replicate
  ap.RG.rep = matrix(ncol=nRepl, nrow=length(Wl))
  ap.Stramski.rep = matrix(ncol=nRepl, nrow=length(Wl))
  ap.4.5.rep = matrix(ncol=nRepl, nrow=length(Wl))

  for (i in 1:nRepl){
    ap.RG.rep[,i] = Ap_list[[i]]$Ap_RG
    ap.Stramski.rep[,i] = Ap_list[[i]]$Ap_Stramski
    ap.4.5.rep[,i] =  Ap_list[[i]]$Ap_4.5
  }

  Ap.RG.mean = apply(ap.RG.rep,1,mean)
  Ap.RG.sd = apply(ap.RG.rep,1,sd)

  Ap.Stramski.mean = apply(ap.Stramski.rep,1,mean)
  Ap.Stramski.sd = apply(ap.Stramski.rep,1,sd)

  Ap.4.5.mean = apply(ap.4.5.rep,1,mean)
  Ap.4.5.sd = apply(ap.4.5.rep,1,sd)


  ############ PLot data

  Df = as.data.frame(cbind(Wl, ap.Stramski.rep,  Ap.Stramski.mean ))
  names(Df) <- c("Wavelength", Repl, "Mean")

  Dfm = melt(Df, "Wavelength")
  names(Dfm) <- c("Wavelength", "Replicate", "values")

  png(file=paste(path.png,ID,"_" , SpecType,".png", sep=""), width = 800, height = 600, units = "px", pointsize = 6)
  p1 <- ggplot(data=Dfm, aes(x=Wavelength, y=values, color=Replicate)) + geom_line(size=2)
  p1 <- p1 + scale_x_continuous(limits = c(300, 750))
  p1 <- p1 + scale_color_manual(values=c(rainbow.modified(nRepl),1))
  p1 <- p1 + labs(x="Wavelength (nm)", y=expression(paste("a"["p"]," (m"^"-1",")")))
  p1 <- p1 + theme(axis.text=element_text(size=16),
                   axis.title=element_text(size=16,face="bold"),
                   legend.title = element_text(size=16,face="bold") )
  p1 <- p1 + ggtitle("Beta factor from Stramski et al. 2015")
  print(p1)
  dev.off()
  #print(p1)

  return(list(ID = ID,
              Station=Station,
              SpecType=SpecType,
              Repl = Repl,
              Depth = Depth,
              Lambda = Wl,
              Ap.Stramski.mean=Ap.Stramski.mean,
              Ap.RG.mean=Ap.RG.mean,
              Ap.4.5.mean =Ap.4.5.mean,
              Ap.Stramski.sd=Ap.Stramski.sd,
              Ap.RG.sd=Ap.RG.sd,
              Ap.4.5.sd =Ap.4.5.sd
              ))

}


