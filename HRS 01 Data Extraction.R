
data_h <- read_dta('H_HRS_c.dta')
data_r <- read_dta('randhrs1992_2022v1.dta')

subdat_demo <- subset(data_r,
                   select = c(inw6,r10iwstat,r11iwstat,r12iwstat,r13iwstat,r14iwstat,r15iwstat,r16iwstat,r6wtresp,hhid,hhidpn,
                              rabyear, r6agey_m,ragender,raracem, rahispan,raedyrs,raedegrm,raeduc,
                              r6mstath,r6lbrf,r6cenreg,h6atotb,h6hhres,rameduc,rafeduc,
                              radyear,radmonth,raddate,r6iwbeg,r10iwbeg,r11iwbeg,r12iwbeg,r13iwbeg,r14iwbeg,r15iwbeg,r16iwbeg,reiwmid, r10lblonely3))
subdat_h <- subset(data_h,
                   select = c(hhid,hhidpn,raeducl,h6rural,r6email,r7email,r8email,r9email,r10email))

data01 <- merge(subdat_demo, subdat_h,by = c("hhid","hhidpn"))

names(data01) <- c("hhid","hhidpn","inw6","inw10","inw11","inw12","inw13","inw14","inw15","inw16","r6weight",
                   "birth_year","age","gender","race","ethnicity","education_year","highest_degree","education_cate",
                   "marital_status","employment_status","residence_region","wealth","living_with_others",
                   "education_mother_year","education_father_year","death_year","death_month","death_date",
                   "inv_date6","inv_date10","inv_date11","inv_date12","inv_date13","inv_date14","inv_date15","inv_date16","exit_date","loneliness","H_education","residence_rural",
                   "w6","w7","w8","w9","w10")

subdat_dise <- subset(data_r,
                      select = c(hhid,hhidpn,r6depres,r10depres,r6cesd,r10cesd, r11cesd,r12cesd,r13cesd,r14cesd,
                              r10hearte,r11hearte,r12hearte,r13hearte,r14hearte,
                              r10stroke,r11stroke,r12stroke,r13stroke,r14stroke))
                              
subdat_adl <- subset(data_r,
                   select = c(hhid,hhidpn,r10walkra, r11walkra, r12walkra, r13walkra, r14walkra, r10dressa, r11dressa, r12dressa, r13dressa, r14dressa, r10batha, r11batha, r12batha, r13batha, r14batha, r10eata, r11eata, r12eata, r13eata, r14eata,r10beda, r11beda, r12beda, r13beda, r14beda, r10toilta, r11toilta, r12toilta, r13toilta, r14toilta))

subdat_chr <- subset(data_r, select = c(hhid,hhidpn,r10conds,r11conds,r12conds,r13conds,r14conds))

data01 <- merge(data01, subdat_dise, by = c("hhid","hhidpn"))
data01 <- merge(data01, subdat_adl, by = c("hhid","hhidpn"))
data01 <- merge(data01, subdat_chr, by = c("hhid","hhidpn"))


