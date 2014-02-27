HMR<-function(filename,series=NA,dec='.',sep=';',JPG=FALSE,PS=FALSE,PHMR=FALSE,npred=500,LR.always=FALSE,FollowHMR=FALSE,ngrid=1000)
{
  ## Input
  ## -----
  ## filename : En tekststreng indeholdende filnavnet. Det forudsættes, at datamappen i forvejen er sat med "setwd".
  ##            Outputtet fra HMR gemmes i en tekstfil med 'HMR - ' foranstillet.
  ## series   : En vektor indeholdende navnene på de serier i datafilen, for hvilke der ønskes en HMR-analyse. Hvis "series=NA",
  ##            eller "series" indeholder et "NA", analyseres hele datafilen.
  ## dec      : Decimaltegn på datafilen, '.' eller ','. Default: '.'.
  ## sep      : Kolonneseparatoren på datafilen, ';' eller ','. Default: ';'.
  ## JPG      : Skal plottene med modelfit eksporteres som jpg-filer? Default: FALSE.
  ## PS       : Skal plottene med modelfit eksporteres som ps-filer? Default: FALSE.
  ## PHMR     : Hvis TRUE, udskrives "npred" prædikterede værdier til en CSV-fil, for de dataserier hvor brugeren har valgt
  ##            HM-analyse. Værdierne vælges jævnt fordelt over målingernes tidsinterval. Navnet på CSV-filen er lig
  ##            datafilens navn med 'PHMR - ' foranstillet. Default: FALSE.
  ## npred    : Se beskrivelsen af "PHMR". Default: 500.
  ## LR.always: Hvis TRUE, udføres altid LR i tilføjelse til den analyse, brugeren har valgt. Default: FALSE.
  ## FollowHMR: Hvis TRUE, annulleres brugerens valg af analyse, og HMR's anbefalinger følges. Default: FALSE.
  ## ngrid    : Antal punkter i gittersøgninger. Skal være mindst 100. Default: 1000.
  
  ## Parametre - man pt. ikke kan ændre
  ## ----------------------------------
  ## MSE.zero          : Bagatelgrænse for MSE. Default er 10 gange regnenøjagtigheden.
  ## bracketing.tol    : Konvergenskriterium i søgningen efter det maksimale kappa-interval. Default: 1e-7.
  ## bracketing.maxiter: Ditto. Default: 1000.
  ## xtxt              : Tekst ved x-aksen.
  ## ytxt              : Tekst ved y-aksen.
  ## sep               : Søjleseparator-tegnet på datafilen.
  MSE.zero<-10*max(.Machine$double.eps,.Machine$double.neg.eps)
  bracketing.tol<-1e-7
  bracketing.maxiter<-1000
  xtxt<-'Time since deployment'
  ytxt<-'Chamber concentration'

  ## Funktion til tjek for "NA", "-Inf" eller "Inf" i talvektorer
  xOK<-function(x) { # Returnerer "TRUE", hvis "x" ikke indeholder "NA", "-Inf" eller "Inf"; ellers "FALSE"
    OK<-TRUE;
    # if (sum(is.na(x))>0) {OK<-FALSE} else {if (max(abs(x))==Inf) {OK<-FALSE}}
    if (sum(is.na(x))>0) {
      OK<-FALSE
    } else {
      if (max(abs(x))==Inf) {
        OK<-FALSE
      }
    }
    OK
  }

## Min version af "sprintf"
  mysprintf<-function(x) {
    if (!is.na(x)) {
      d<-unlist(strsplit(x=sprintf("%.4E",x),split='.',fixed=TRUE));
      dum<-paste(d[1],d[2],sep=dec)
    } else {
      dum<-'NA'
    }
    dum
  }

  ## Kontrollerer for fejl i input
  ##   1. "filename" skal være en tekststreng af længde én. Om den peger på en eksisterende fil, overlades til R.
  ##   2. "series" skal være en ikke-tom tekststreng eller "NA".
  ##   3. "dec" skal være "." eller ",".
  ##   4. "sep" skal være ";" eller ",". 
  ##   5. "dec" og "sep" må ikke begge være ",".
  ##   6. "ngrid" og "npred" skal være heltal, og "ngrid" skal være mindst 100.
  ##   7. "LR.always", "JPG", "PS", "PHMR" og "FollowHMR" skal være "TRUE" eller "FALSE".
  ##   8. "xtxt" og "ytxt" skal bare ikke være "NULL".

  # Kontrollerer "filename"
  if ((length(filename)!=1)|(!is.character(filename))) {FATAL<-TRUE} else
  {
    # Kontrollerer "series"
    if (!(((sum(is.na(series))==1)&(length(series)==1))|((length(series)>0)&(is.character(series))))) {FATAL<-TRUE} else
    {
      # Kontrollerer "dec" og "sep" - 1. gang
      if (!((is.character(dec))&(length(dec)==1)&(is.character(sep))&(length(sep)==1))) {FATAL<-TRUE} else
      {
        # Kontrollerer "dec" og "sep" - 2. gang
        if (!(((dec=='.')|(dec==','))&((sep==';')|(sep==',')))) {FATAL<-TRUE} else
        {
          # Kontrollerer "dec" og "sep" - 3. gang
          if ((dec==',')&(sep==',')) {FATAL<-TRUE} else
          {
            # Kontrollerer "ngrid" og "npred" - 1. gang
            if (!(is.numeric(ngrid)&(length(ngrid)==1)&is.numeric(npred)&(length(npred)==1))) {FATAL<-TRUE} else
            {
              # Kontrollerer "ngrid" og "npred" - 2. gang
              if (!((xOK(ngrid))&(ngrid>=100)&(ngrid==floor(ngrid))&(ngrid==ceiling(ngrid))&(xOK(npred))&(npred>0)&(npred==floor(npred))&(npred==ceiling(npred)))) {FATAL<-TRUE} else
              {
                # Kontrollerer "LR.always", "JPG", "PS", "PHMR" og "FollowHMR"
                if (!(is.logical(FollowHMR)&is.logical(PHMR)&is.logical(LR.always)&is.logical(JPG)&is.logical(PS))) {FATAL<-TRUE} else
                {
                  # Kontrollerer "xtxt" og "ytxt"
                  if (is.null(xtxt)|is.null(ytxt)) {FATAL<-TRUE} else
                  {
                    # Input-parametre er OK!
                    FATAL<-FALSE
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  ## Kan vi fortsætte?
  if (FATAL)
  {
    Comment<-'Error in input parameters'
  } else
  {
    ## Dataindlæsning
    oldOutDec<-getOption("OutDec"); options(OutDec=dec)
    testread<-.HMR.read(filename=filename,dec=dec,sep=sep)
    options(OutDec=oldOutDec)
    if (testread$FATAL) # Data kunne ikke indlæses
    {
      FATAL<-TRUE
      Comment<-'Data file could not be read'
    } else # Data kunne indlæses
    {
      # Alle dataserier eller en delmængde?
      dataserier<-rep(NA,testread$nserier); for (i in 1:testread$nserier) dataserier[i]<-testread$HMRdata[[i]]$serie
      if ((sum(is.na(series))==1)&(length(series)==1)) {userie<-dataserier} else {userie<-unique(series)}
      # Findes "userie" i datafilen og i givet fald hvor?
      # "status=0": Hvis serien ikke findes på datafilen, eller hvis "HMR.read" har fundet fejl i dataserien.
      # "status=1": Dataserien findes på datafilen, og "HMR.read" har ikke fundet fejl i den.
      nserie<-length(userie)
      HMRdata<-vector(mode='list',length=nserie)
      nJA<-0
      for (i in 1:nserie)
      {
        if (sum(dataserier==userie[i])>0)
        {
          datai<-min((1:length(dataserier))[dataserier==userie[i]]) # "min" er sikkert overflødig
          if (testread$HMRdata[[datai]]$status>0) {nJA<-nJA+1; HMRdata[[i]]<-testread$HMRdata[[datai]]} else {HMRdata[[i]]<-list(serie=userie[i],status=0)}
        } else {HMRdata[[i]]<-list(serie=userie[i],status=0)}
      }
      # Er der nogle data til analyse?
      if (nJA<1)
      {
        FATAL<-TRUE
        Comment<-'Errors in all selected data series'
      } else
      # Analyserer fundne data
      {
        # Starter/tømmer grafisk vindue
        frame()
        # Gemmer "par" options
        oldmfrow<-par("mfrow"); oldoma<-par("oma"); oldbty<-par("bty"); oldpty<-par("pty")
        # Data frames til output
        if (LR.always)
        {
          OUTPUT<-data.frame(Series='',f0=0,f0.se=0,f0.p=0,f0.lo95=0,f0.up95=0,Method='',Warning='',
          LR.f0=0,LR.f0.se=0,LR.f0.p=0,LR.f0.lo95=0,LR.f0.up95=0,LR.Warning='',stringsAsFactors=FALSE)
          colnames(OUTPUT)<-c("Series","f0","f0.se","f0.p","f0.lo95","f0.up95","Method","Warning",
          "LR.f0","LR.f0.se","LR.f0.p","LR.f0.lo95","LR.f0.up95","LR.Warning")
        } else
        {
          OUTPUT<-data.frame(Series='',f0=0,f0.se=0,f0.p=0,f0.lo95=0,f0.up95=0,Method='',Warning='',stringsAsFactors=FALSE)
          colnames(OUTPUT)<-c("Series","f0","f0.se","f0.p","f0.lo95","f0.up95","Method","Warning")
        }
        if (PHMR) {PHMROUT<-data.frame(Series='',Time=0,HMR=0,stringsAsFactors=FALSE); colnames(PHMROUT)<-c("Series","Time","HMR")}
        STOP<-FALSE
        for (i in 1:nserie) if (!STOP)
        {
          if (HMRdata[[i]]$status>0)
          # Dataanalyse
          {
            oHMR<-.HMR.fit1(tid=HMRdata[[i]]$tid,konc=HMRdata[[i]]$konc,A=HMRdata[[i]]$A,V=HMRdata[[i]]$V,serie=HMRdata[[i]]$serie,
                ngrid=ngrid,LR.always=LR.always,FollowHMR=FollowHMR,JPG=JPG,PS=PS,PHMR=PHMR,npred=npred,xtxt=paste(xtxt),ytxt=paste(ytxt),
                pcttxt=paste(' (',round(100*i/nserie,0),'%)',sep=''),MSE.zero=MSE.zero,
                bracketing.tol=bracketing.tol,bracketing.maxiter=bracketing.maxiter);
            if (LR.always)
              OUTPUT<-rbind(OUTPUT,c(HMRdata[[i]]$serie,mysprintf(oHMR$f0),mysprintf(oHMR$f0.se),
                    mysprintf(oHMR$f0.p),mysprintf(oHMR$f0.lo95),mysprintf(oHMR$f0.up95),oHMR$method,oHMR$warning,
                    mysprintf(oHMR$LR.f0),mysprintf(oHMR$LR.f0.se),mysprintf(oHMR$LR.f0.p),mysprintf(oHMR$LR.f0.lo95),
                    mysprintf(oHMR$LR.f0.up95),oHMR$LR.warning, mysprintf(oHMR$QR.C0), mysprintf(oHMR$QR.f0), mysprintf(oHMR$QR.f0.se),
                    mysprintf(oHMR$QR.f0.p), mysprintf(oHMR$QR.f0.lo95), mysprintf(oHMR$QR.f0.up95), oHMR$QR.f0.warning))
            else
              OUTPUT<-rbind(OUTPUT,c(HMRdata[[i]]$serie,mysprintf(oHMR$f0),mysprintf(oHMR$f0.se),mysprintf(oHMR$f0.p),
                    mysprintf(oHMR$f0.lo95),mysprintf(oHMR$f0.up95),oHMR$method,oHMR$warning))
                if (PHMR) 
                  if (oHMR$method=='HMR') 
                    for (p in 1:npred) 
                      PHMROUT<-rbind(PHMROUT,c(oHMR$serie,oHMR$PHMR.ptid[p],oHMR$PHMR.pkonc[p]))
                        if (oHMR$warning=='Cancelled') {
                          STOP<-TRUE
                        }
          } else { # Ingen dataanalyse
            if (LR.always)
              OUTPUT<-rbind(OUTPUT,c(HMRdata[[i]]$serie,NA,NA,NA,NA,NA,'None','Data error',NA,NA,NA,NA,NA,NA))
            else
              OUTPUT<-rbind(OUTPUT,c(HMRdata[[i]]$serie,NA,NA,NA,NA,NA,'None','Data error'))
          }
        }
        # Reset "par"
        par(mfrow=oldmfrow,oma=oldoma,bty=oldbty,pty=oldpty)
      }
    }
  }
  
  ## Output
  if (FATAL) {
    Comment
  } else {
    oldOutDec<-getOption("OutDec"); 
    options(OutDec=dec);
    OUTPUT<-OUTPUT[-1,];
    rownames(OUTPUT)<-paste(1:dim(OUTPUT)[1],sep='');
    write.table(x=OUTPUT,file=paste('HMR-',filename,sep=''),append=FALSE,quote=FALSE,dec=dec,sep=sep,
        row.names=FALSE,col.names=TRUE);
    if (PHMR)
      if (dim(PHMROUT)[1]>1) {
        PHMROUT<-PHMROUT[-1,]
          rownames(PHMROUT)<-paste(1:dim(PHMROUT)[1],sep='')
          write.table(x=PHMROUT,file=paste('PHMR - ',filename,sep=''),append=FALSE,quote=FALSE,dec=dec,sep=sep,row.names=FALSE,col.names=TRUE)
      }
    options(OutDec=oldOutDec);
    OUTPUT;
  }
}
