# Functions to recode the Mexican mortality database
# with the CDC's injury mortality matrix and be able
# classify the type of injury to firearm, poisoning,
# etc. 

ICDSeq <- function(start, end) {
  ##Produce a sequence of icd codes
  ##start = A1 end= A9 outputs A1, A2, A3... A9
  ##start and end should begin with the same letter
  letter <- str_sub(start, 1, 1)
  number1 <-  as.numeric(str_sub(start, 2, length(str) + 2))
  number2 <- as.numeric(str_sub(end, 2, length(str) + 2))
  str_c(letter, gsub(" ", "0",
                     format(number1:number2, width = 2)))
}



MortalityMatrix <- function(df, codes, name) {
  #Recode the Mexican Mortality Database with the
  ##injury mortality matrix from the CDC
  ##http://www.cdc.gov/nchs/data/ice/icd10_transcode.pdf
  ##
  ##df - the database
  ##codes - the sequence of icd codes that belong to each cause of injury
  ##name - the name of the the cause of injury (firearm, cut/pierce, etc)
  str <- codes
  str <- str_replace_all(str, "\\*", "")
  str <- str_replace_all(str, "\\.", "")
  l <-  str_split(str, ", ")

  sequence <- c();Cdeaths <- c(); Ccausadef <- c()

  for (i in l[[1]]) {
    if(str_detect(i, "-")) {
      x <- str_split(i, "-")
      sequence <- c(sequence, ICDSeq(x[[1]][1], x[[1]][2]))
    } else {
      if(str_length(i) == 4) {
        Cdeaths <-  c(Cdeaths, i)
      }
      if(str_length(i) == 3) {
        Ccausadef <- c(Ccausadef, i)
      }
    }
  }
 #print(name)
  #print(sequence)
  if(length(sequence))
    try(df[df$CAUSADEF %in% sequence,]$CAUSE <-  name, silent = TRUE)
  if(length(Cdeaths))
   try(df[df$CDEATH %in% Cdeaths,]$CAUSE <-  name, silent = TRUE)
  if(length(Ccausadef))
    try(df[df$CAUSADEF %in% Ccausadef,]$CAUSE <- name, silent = TRUE)
  return(df)
}

#The list of icd codes from:
#http://www.cdc.gov/nchs/data/ice/icd10_transcode.pdf
mmatrixcodes <- c("W25-W29, W45, X78, X99, Y28, Y35.4",
                  "W65-W74, X71, X92, Y21",
                  "W00-W19, X80, Y01, Y30",
                  "X00-X19, X76-X77, X97-X98, Y26-Y27, Y36.3, *U01.2",
                  "W32-W34, X72-X74, X93-X95, Y22-Y24, Y35.0, *U01.4",
                  "W24, W30-W31",
                  "V01-V99, X82, Y03, Y32, Y36.1, *U01.1",
                  "W42, W43, W53-W64, W92-W99, X20-X39, X51-X57",
                  "X50",
                  "X40-X49, X60-X69, X85-X90, Y10-Y19, Y35.2, *U01.6, *U01.7)",
                  "W20-W22, W50-W52, X79, Y00, Y04, Y29, Y35.3",
                  "W75-W84, X70, X91, Y20",
                  "W23, W35-W41, W44, W49, W85-W91, Y85, X75, X81, X96, Y02, Y05-Y07, Y25, Y31, Y35.1, Y35.5, Y36.0, Y36.2, Y36.4, Y36.5, Y36.6, Y36.7, Y36.8, Y36.4",
                  "X58, Y86, X83, Y87.0, Y08, Y87.1, Y33, Y87.2, Y35.6, Y89.0, Y89.1, *U01.8, *U02",
                  "X59, X84, Y09, Y34, Y89.9, Y35.7, Y36.9, *U01.9, *U03.9",
                  "Y40-Y59, Y60-Y84, Y88")

#The cause of injury correponding to the codes above
death.types <- c("Cut/pierce",
                 "Drowning",
                 "Fall",
                 "Fire/ hot object or substance",
                 "Firearm",
                 "Machinery",
                 "All Transport",
                 "Natural /environmental",
                 "Overexertion",
                 "Poisoning",
                 "Struck by or against",
                 "Suffocation",
                 "Other specified, classifiable",
                 "Other specified, nec",
                 "Unspecified",
                 "Adverse effects")



deaths$CAUSE <- NA
for(i in 1:length(mmatrixcodes)) {
  deaths <- MortalityMatrix(deaths, mmatrixcodes[i], death.types[i])
}
