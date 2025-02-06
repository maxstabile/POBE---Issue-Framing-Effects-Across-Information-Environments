
# # -----------------------------------------------------------------------
# TV DATA (JN)
# # -----------------------------------------------------------------------


jntv<-read.csv("data/tv/SSR_JN_content_analysis.csv", header=T)

time_to_seconds <- function(time_string) {
  # Split the time_string into minutes and seconds parts
  parts <- strsplit(time_string, "'")[[1]]
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(sub("''", "", parts[2]))
  
  # Calculate total seconds
  total_seconds <- minutes * 60 + seconds
  return(total_seconds)
}

# SURVEY 1: ZERO MINUTES AND MULTIPLE MAIN TOPICS (9H OF JN)
times1 <- c(
  "45'34''", "34'59''", "41'12''",
  "51'51''", "48'07''", "33'46''",
  "46'29''", "46'44''", "45'15''",
  "50'22''", "50'39''", "49'34''"
); times1 <- sum(sapply(times1, time_to_seconds))/60/60; print(times1)

# SURVEY 2: 21'37 SSR (17H OF JN)
times2 <- c(
  "45'34''", "34'59''", "41'12''",
  "51'51''", "48'07''", "33'46''",
  "46'29''", "46'44''", "45'15''",
  "50'22''", "50'39''", "49'34''",
  "47'13''", "48'35''", "41'00''",
  "39'28''", "49'29''", "55'55''",
  "55'18''", "42'53''",
  "42'56''", "52'52''", "41'11''"
); times2 <- sum(sapply(times2, time_to_seconds))/60/60; print(times2)

# SURVEY 3: 39'21 SSR (9H OF JN)
times3 <- c(
  "41'48''", "56'31''", "45'22''", "53'13''", "46'05''",
  "41'12''", "41'06''", "46'50''", "31'40''", "62'54''",
  "45'49''", "44'32''"
); times3 <-sum(sapply(times3, time_to_seconds))/60/60; print(times3)

# SURVEY 4: 4'36 SSR (8H)
times4 <- c(
  "44'34''", "44'13''", "41'51''", "48'48''", "63'20''",
  "43'48''", "43'05''", "31'20''", "42'17''", "44'26''",
  "42'59''", "42'21''"); times4 <-sum(sapply(times4, time_to_seconds))/60/60; print(times4)

# SURVEY 5: ZERO MINUTES AND MAIN TOPIC RUSSIAN INVASION TO UKRAINE EVERYDAY (11H OF JN)
times5 <- c(
  "73'15''", "56'59''", "61'47''", "52'57''", "54'42''",
  "54'40''", "52'49''", "53'53''", "52'55''", "54'00''",
  "50'38''", "54'31''"
); times5<-sum(sapply(times5, time_to_seconds))/60/60

print(times1+times2+times3+times4+times5)