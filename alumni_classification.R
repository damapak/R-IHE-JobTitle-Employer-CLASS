#needs the following named files to be in the same directory:
#   job_levels.txt - one job level per line
#   job_titles.txt - one job title per line
#   abbreviation_replacement_list.R - coded list
#   university_employer_classify.R - code to classify employer
#   industry_map.csv - first col is original industry, also contains named cols "SOC2","new_industry","AO"
#                     could change named refs above to col number if needed (line 166).
#                     To update this, add original industry description, SOC code, SOC classification
#                     and AO classification.  Can add as many rows as needed.
#   industry_keywords.R - coded list of lists, with industry category and associated keywords
#                         code will run without this, just won't do industry


#add required packages - none needed


#  START FUNCTIONS   +=+=+=+=+=+=+   =+=+=+=+=+=+   =+=+=+=+=+=+   =+=+=+=+=+=+

#function to print list of needed files
printNeeded <- function() {
  need_files <- c("job_levels.txt", 
                  "job_titles.txt",
                  "abbreviation_replacement_list.R",
                  "university_employer_classify.R",
                  "industry_map.csv")
  writeLines("The following files are required (and need to be in same directory/folder):")
  writeLines(need_files)
}

#function to load data
loadfile = function(){
  fname <- file.choose()
  dframe = read.csv(fname, header=TRUE, as.is=T, sep=",")
  return(dframe)
}

#function to add fields
pre_process <- function(dfin, options="") {
  dfin$job_title_orig.noabbr <- ""
  dfin$job_title <- ""
  dfin$job_level <- ""
  dfin$job_spec <- ""
  dfin$job_industry <- ""
  dfin$placeholder <- ""
  dfin$processed <- 0
  dfin$mult_titles <- 0
  dfin$rhs_placeholder <- ""
  dfin$and_placeholder <- ""
  dfin$job_industry.ti <- ""
  dfin$job_industry.tiAlt <- ""
  if (options=="all") {
  }
  return(dfin)
}

#add function to check if a column exists and add _1 to end of name
#  if ("text" in colnames(df)){paste("text","_1",sep="")}


#  END FUNCTIONS   +=+=+=+=+=+=+   =+=+=+=+=+=+   =+=+=+=+=+=+   =+=+=+=+=+=+


#SELECT FOLDER CONTAINING R CLASSIFIER NEEDED FILES
printNeeded()
#select workspace containing the needed files listed from printNeeded() function
wd = choose.dir(default = "", caption = "select dir that contains R Classifier files (view R console for printed list of files)")
#import needed lists (checks for existence of job_titles.txt, remaining are assumed to be there)
if (file.exists (paste(wd,"/job_titles.txt", sep=""))) {
  titlez = scan(paste(wd,"/job_titles.txt", sep=""),what="",sep="\n")
  titlez = toupper(titlez)
  levelz = scan(paste(wd,"/job_levels.txt", sep=""),what="",sep="\n")
  levelz = toupper(levelz)
} else {
  stop("needed files do not exist.  Please check selected directory")
}

writeLines("\n.csv format is required.  cancel the first dialog box if you need to convert, then re-run")

#run loadfile
df = loadfile()

#select the title field
print(names(df))


#if choose to recode job title  =============================================================
if(winDialog(type="yesno", "Re-code job title data?") == "YES"){

titlefield <- winDialogString("what is the field name containing the job title?\n(case sensitive, list of fields printed in R console)","")
df[, titlefield] <- toupper(df[, titlefield])
#if job_title column already exists, rename to job_title_orig before job_title is recalcd in script
if(titlefield == "job_title") {
  df$job_title_orig <- df[, titlefield]
  titlefield <- "job_title_orig"
}

#run pre_process (add fields)
df = pre_process(df)

#calc placeholder field
df$placeholder <- df[, titlefield]



#step 1:  abbreviation replacements
source(paste(wd,"/abbreviation_replacement_list.R",sep=""))
for (i in seq(1:length(abbr_replace))) {
  df$placeholder <- gsub(paste(abbr_replace[[i]], collapse="|"), names(abbr_replace[i]), df$placeholder,
                         perl = TRUE)
}
df$placeholder <- gsub("\\s+", " ", df$placeholder) #remove extra spaces
df$job_title_orig.noabbr <- df$placeholder #for industry classification - want all words


#step 2 of processing:  flag 'and' titles
#       Place left of AND in placeholder, right of and in and_placeholder field.
#       Content to left of and typically primary info, contains info for classification
df[grepl(". AND ", toupper(df$placeholder)), "mult_titles"] = 1
df[grepl(". & ", toupper(df$placeholder)), "mult_titles"] = 1
df$and_placeholder = as.character(lapply(strsplit(df$placeholder, split=" AND "), "[", 2))
df$placeholder = as.character(lapply(strsplit(df$placeholder, split=" AND "), "[", 1))


#step 3:  split RHS titles (those that have the word 'TO' or 'UNDER')
#  do not want assistant to VP to be the same as VP
#  possibly add "FOR" to this section?
df$rhs_placeholder = as.character(lapply(strsplit(df$placeholder, split=" TO "), "[", 2))
df$placeholder = as.character(lapply(strsplit(df$placeholder, split=" TO "), "[", 1))


#step 4:  process project, account, teaching assistant
#         (want project manager or account manager to be PROJECTS or ACCOUNTS, not MANAGER)
acct_proj_levels <- c("SENIOR","JUNIOR","ASSISTANT","ASSOCIATE","EXECUTIVE", "ANALYST","MANAGER",
                      "PLANNER","REPRESENTATIVE","COORDINATOR","SPECIALIST","DIRECTOR","SERVICES","SPECIALIST","STRATEGIST",
                      "SUPERVISOR","REGIONAL")

df[grep("ACCOUNT(?!A|I|E)", df$placeholder, perl=T), "job_title"] = "ACCOUNTS"
df[grep("PROJECT(?!O|I|E)", df$placeholder, perl=T), "job_title"] = "PROJECTS"
for (term in acct_proj_levels) {
  df[ grepl(term, df[,"placeholder"]) & ( df[, "job_title"] == "ACCOUNTS" | df[, "job_title"] == "PROJECTS" ) , "job_level"] = 
    paste(df[ grepl(term, df[,"placeholder"]) & ( df[, "job_title"] == "ACCOUNTS" | df[, "job_title"] == "PROJECTS" ) , "job_level"], term)
}
#step 4b:  teaching assistant
df[ grepl("TEACH.+ASSISTANT", df[,"placeholder"]) & df[, "job_title"] == "", "job_title"] = "TEACHING ASSISTANT"


#step 5:  research jobs (want Research associate to be RESEARCH, not ASSOCIATE (Associate is level)
df[grepl("RESEARCH (?!PROF|METH|FACU)|RESEARCHER|RES([a-zA-Z]+)CHER", df$placeholder, perl=T) & df[,"job_title"] == "", "job_title"] = "RESEARCH"
rsrch_levels <- c("JUNIOR", "SENIOR","ASSISTANT","ASSOCIATE","PRINCIPAL","ADVISOR","SPECIALIST",
                  "ENGINEER","STUDENT","SCIENTIST","FELLOW","SCHOLAR","COORDINATOR","MANAGER","SUPERVISOR",
                  "ANALYST","PSYCHOLOGIST")
for (term in rsrch_levels) {
  df[ grepl(term, df[,"placeholder"]) & df[, "job_title"] == "RESEARCH", "job_level"] = 
    paste(df[ grepl(term, df[,"placeholder"]) & df[, "job_title"] == "RESEARCH", "job_level"], term)
}

#step 6:  owner/founder (Want to do this prior to C-Level, do not want someone who
#         lists CEO / Owner to be a C-level.  They should be founder or owner)
df[grep("(?<!CO|CO.)OWNER|PROPRIETOR", df$placeholder, perl=T), "job_title"] = "OWNER"
df[grep("(?<=CO|CO.)OWNER", df$placeholder, perl=T), "job_title"] = "CO-OWNER"
df[grep("(?<!CO|CO.)FOUNDER", df$placeholder, perl=T), "job_title"] = "FOUNDER"
df[grep("(?<=CO|CO.)FOUNDER", df$placeholder, perl=T), "job_title"] = "CO-FOUNDER"

#step 7: c-level  (remove those also containing Intern, Assistant, To, or For)
df[grepl("CHIEF.+OFFICER", df[,"placeholder"]) & df[, "job_title"] == "", "job_title"] = "C-LEVEL EXECUTIVE"
df[grepl("^(?!.*INTERN|ASSISTANT|TO|FOR).*(?<!A|E|I|O|U)C.O(?!A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z).*$", 
                perl=T, df[,"placeholder"]) & df[, "job_title"] == "", "job_title"] = "C-LEVEL EXECUTIVE"
#7b:  board members
df[grepl("BOARD.+MEMBER|MEMBER.+BOARD", df[,"placeholder"]) & df[, "job_title"] == "", "job_title"] = "BOARD MEMBER"


#step 8:  all the rest  (goes through job_titles.txt)
#REVISE to allow for looser match (2 changes)
for (term in titlez) {
  grepsearch = paste(term,"(?=$|!|,|/|-| )",sep="")
  df[ grepl(grepsearch, df[,"placeholder"], perl=T) & df[, "job_title"] == "" , "job_title"] = term
}
#step 8b:  run through and_placeholder for those that did not get titles from placeholder field
for (term in titlez) {
  grepsearch = paste(term,"(?= |$|!|,|/)",sep="")
  df[ grepl(grepsearch, df[,"and_placeholder"], perl=T) & df[, "job_title"] == "" , "job_title"] = term
}


###    INDUSTRY  - possibly separate out into own block  #######
#step 8.5:  industry based on job title (for those that can be determined by title)
df[ grep("NIOR HIGH", df$placeholder), "job_industry"] <- "K-12 EDUCATION"
#df[ df$job_title == "TEACHER", "job_industry"] <- "EDUCATION"
df[ df$job_title == "PROFESSOR", "job_industry"] <- "HIGHER EDUCATION"
df[ grep( "RESEARCH|SCIENTIST", df$job_title), "job_industry"] <- "RESEARCH"
#step 8.5b:  assign industry based on category/keywords
#  only run if both needed files are present
if (file.exists( paste (wd, "/industry_keywords.R", sep="") ) & 
    file.exists( paste (wd, "/strMatchScore.R", sep="" ) ) ) {
  source( paste (wd, "/industry_keywords.R", sep="") )  #creates industry_kw
  source( paste (wd, "/strMatchScore.R", sep="" ) )  #imports strMatchScore function for use
  df$job_industry.ti = sapply(df$job_title_orig.noabbr, strMatchScore ,  industry_kw , 1)
  df$job_industry.tiAlt = sapply(df$job_title_orig.noabbr, strMatchScore ,  industry_kw , 2)
  # move job_industry.ti to job_industry field (only if job_industry is blank)
  df[ df$job_industry == "", "job_industry"] <- df[ df$job_industry == "", "job_industry.ti"]
}



#step 9:  levels  (search through placeholder field for job level terms in job_levels.txt)
#remove junior/senior high as do not want it to show up in levels
df$placeholder <- gsub("JUNIOR HIGH|SENIOR HIGH", "", df$placeholder)
for (term in levelz) {
  df[ grepl(term, df[,"placeholder"]) & df[, "job_level"] == "" , "job_level"] = term
}


#step 10:  specializations


#step 10a: professor specializations
df[ df$job_title =="PROFESSOR", "professor_processed"] <- 0
#add needed fields before subsetting/splitting df
df$professor_placeholder <- ""
df$specialization <- ""
#separate df so can process professor records
df.prof <- subset(df, professor_processed==0)
df.noprof <- subset(df, is.na(professor_processed))
#operations to run on professor records
df.prof$professor_placeholder = as.character(lapply(strsplit(df.prof$placeholder, split=" OF "), "[", 2))
df.prof[ grep("[A-Za-z]", df.prof$professor_placeholder), "professor_processed" ] <- 1
df.prof[ df.prof$professor_processed == 0, "professor_placeholder"] = as.character(lapply(strsplit(df.prof[ df.prof$professor_processed == 0, "placeholder"], split=","), "[", 2))
df.prof[ grep("[A-Za-z]", df.prof$professor_placeholder), "professor_processed" ] <- 1
df.prof[ df.prof$professor_processed == 0, "professor_placeholder"] = as.character(lapply(strsplit(df.prof[ df.prof$professor_processed == 0, "placeholder"], split=" IN "), "[", 2))
df.prof[ grep("[A-Za-z]", df.prof$professor_placeholder), "professor_processed" ] <- 1
df.prof[ df.prof$professor_processed == 0, "professor_placeholder"] = as.character(lapply(strsplit(df.prof[ df.prof$professor_processed == 0, "placeholder"], split="-(?!TIME|CALL|DEG)", perl=T), "[", 2))
df.prof[ grep("[A-Za-z]", df.prof$professor_placeholder), "professor_processed" ] <- 1
df.prof$professor_placeholder <- as.character(lapply(strsplit(df.prof$professor_placeholder, split=",|;|\\.", perl=T), "[", 1))
df.prof$professor_placeholder <- sub("^\\s+", "", df.prof$professor_placeholder)
#re-combine separated df's into original df
df <- rbind( df.prof, df.noprof)


#step 10b:  analyst specializations
source(paste(wd,"/specialization_keywords.R",sep="")) #loads list into 'spec_kw'




#step 11:  calc empty job titles with a level equal to their level
# if level is adjunct, set job title equal to instructor
df[ df$job_title == "" & df[,"job_level"] == "ADJUNCT", "job_title" ] = "INSTRUCTOR"
df[ df$job_title == "" & df[,"job_level"] != "" , "job_title" ] =
  df[ df$job_title == "" & df[,"job_level"] != "" , "job_level" ]


#step 12: if level = job title, clear level.  
df$job_level <- gsub("^\\s+|\\s+$", "", df$job_level) #remove leading, trailing spaces
df$job_level <- gsub("\\s+", " ", df$job_level)  #remove extra spaces
df[ df$job_title == df$job_level, "job_level" ] <- ""


#FINAL CLEANUP
df$job_title <- gsub("\\s+", " ", df$job_title)  #remove extra spaces
df$job_title <- gsub("^\\s+|\\s+$", "", df$job_title)  #remove leading, trailing space


}
#
# #
# # # #
#         end re-code job title if block    # # # # #
# # # #
# #
#


#####  EMPLOYER (UNIVERSITY IDENTIFICATION)  ########
#proceed to employer classification - university_employer_classify.R
if(winDialog(type="yesno", "Do you have an employer name field?") == "YES"){
  source(paste(wd,"/university_employer_classify.R",sep=""))
  #update job_industry field for university
  df[ which( nchar (df$university_employed) > 2 ), "job_industry" ] <- "HIGHER EDUCATION"
}


#####   INDUSTRY (LINKED IN RE-CLASSIFICATION)   ######
#industry reclassification - SOC and AO codes from LinkedIn
if(winDialog(type="yesno", "Re-classify linkedIn industry data?") == "YES"){
  reclassify_industry <- function(dfin){
    imap <- read.csv(paste(wd,"/industry_map.csv",sep=""), as.is=T,header=T)
    LI_industry <- winDialogString("what is the field name containing the LinkedIn Industry Data?","")
    #for row in imap, if df field = row value, populate fields equal to SOC and AO
    # fixed=TRUE option will do exact match, including () and other special characters
    for(row in 1:nrow(imap)){
      dfin[ grepl(toupper(imap[row,1]), toupper(df[,LI_industry]), fixed=TRUE), "indstr_SOC_code"] <- imap[row,"SOC2"]
      dfin[ grepl(toupper(imap[row,1]), toupper(df[,LI_industry]), fixed=TRUE), "indstr_SOC"] <- imap[row, 5] #5=new industry column
      dfin[ grepl(toupper(imap[row,1]), toupper(df[,LI_industry]), fixed=TRUE), "indstr_AO"] <- imap[row, 6] #6=AO industry column
    }
    #df[, redef_field] <- toupper(df[, redef_field])
    return(dfin)
  }
  df <- reclassify_industry(df)
}


########    SAVE /  OUTPUT   DATA  ##########
#output
writefile <- function(dfin){
  output_dir <- choose.dir(default = "", caption = "select a folder to save output file:")
  output_name <- winDialogString("please enter name for your file (no extension needed)","")
  write.csv(dfin, paste(output_dir,"\\",output_name,".csv",sep=""), na="", row.names=F)
}

if(winDialog(type="yesno", "Export data?") == "YES"){
  writefile(df)
} else{
  writeLines("\nData not saved outside of R. To export, use write.csv or write.table functions.\n(for help, type ?write.table in the console)")
}

