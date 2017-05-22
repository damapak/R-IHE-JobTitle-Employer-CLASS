# classify 


#get data frame name and title field
#df <- get(winDialogString("enter the name of your data frame:",""))

print(names(df))
redef_field <- winDialogString("what is the field name containing the university/employer?","")
df[, redef_field] <- toupper(df[, redef_field])
df$university_employed <- ""


#the replacement list
empl_abbr_replace <- list ("CALIFORNIA STATE UNIVERSITY" = c("(?<=^| )CSU.+","(?<=^| )CAL.+ ST(?=\\.| |A).+U",
                                                             "(?<=^| )CAL STATE","HUMBOLDT ST","SAN DIEGO ST",
                                                             "SAN JOSE ST(?=\\.|A| )","SAN FRANCISC.+ST(?=\\.|A| )",
                                                             "(?<=^| )CAL-STATE","CAL.+POLY"),
                      "UNIVERSITY OF CALIFORNIA" = c("(?<=^| )UC","(?<=^| )U\\.C",
                                                     "^(?!.*POLY|DOMINICAN).*UNIV.+ OF CA(?=\\.|L| )"),
                      "CLAREMONT CONSORTIUM" = c("PITZER","C.+ MCKENNA","MUDD","SCRIPPS COL","POMONA COL",
                                "(?<=^| )HMC(?! ARCH)","(?<=^| )CMC","(?<=^| )CGU", "CLAREMONT.+GR.*D.+TE",
                                "CLAREMONT.+CO(?=LL|NS)"),
                      "FOR PROFIT" = c("UNIV.+ PHO.+X","GRAND.+C.+Y.+N.+UNIV"),
                      "COMMUNITY COLLEGE" = c("^((?<!UNIVERSITY).)*(COMMUN.*Y.* COLLEGE|JUNIOR COLLEGE|CITY COLLEGE)",
                                              "CHAFFEY.*COLLEGE","CITRUS COLLEGE","SANTA ANA COLLEGE","CERRITOS COLLEGE",
                                              "M.*T.+SAN .*O.+COLLEGE","WHITTIER COLLEGE","SANTA MONICA COLLEGE",
                                              "CRAFTON HILLS COLL","NORCO COLLEGE","CYPRESS COLLEGE","RIO HONDO COLL",
                                              ". VALL.*Y COLLEGE"),
                      "CA PRIVATE UNIVERSITY" = c("(?<=^| )USC(?= |\\.|$)", "(?<=^| )U.S.C(?= |\\.|$)",
                                       "UNIV.*OF SO.*CA(?=\\.|L|$)","STANFORD.+UNIV","AZUSA PAC",
                                       "U.*OF LA.*VERNE","LOY.*LA.* MARYM","PEP.*ERDINE","CAL.*BAPTIST.*UNIV",
                                       "UNIV.*OF REDLANDS","LOMA.*LINDA.* UN","BIOLA.* UNIV","UNIV.* OF SAN.*DIEGO",
                                       "CHAPMAN UNIV"),
                      "OTHER COLLEGE OR UNIVERSITY" = c("COLLEGE(?!.*PREP|.*ACADEMY|.*CHARTER|.*HIGH|.*MIDDLE|.*ELEM|.*READI|.*SCHOOL)",
                                                        "UNIVERSITY","UNIV\\.")
)

#step 1:  replacements - cal state, UC, 5C
#  loop through categories for matches, only assign if meet criteria AND blank
for (i in seq(1:length(empl_abbr_replace))) {
  df[ grepl(paste(empl_abbr_replace[[i]], collapse="|"), df[,redef_field],perl=T) & df[,"university_employed"] == "", "university_employed"] = names(empl_abbr_replace[i])
}

#step 2
