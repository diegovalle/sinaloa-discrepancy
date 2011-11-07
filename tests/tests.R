
#Test the function ICDseq
expect_that(ICDseq(start = "A1", end = "A3"), matches(c("A01", "A02", "A03")))
expect_that(ICDseq(start = "Z1", end = "Z6"), matches(c("Z01", "Z02", "Z03",
                                   "Z04", "Z05", "Z06")))

#Test that the mortality database is coded correctly
expect_that(deaths[deaths$CAUSADEF %in% ICDseq("W25", "W29"),]$CAUSE,
            matches("Cut/pierce"))

expect_that(deaths[deaths$CAUSADEF %in% ICDseq("W32", "W34"),]$CAUSE,
            matches("Firearm"))

expect_that(deaths[deaths$CAUSADEF %in% "X59",]$CAUSE,
            matches("Unspecified"))

expect_that(deaths[deaths$CAUSADEF %in% ICDseq("W65", "W74"),]$CAUSE,
            matches("Drowning"))


