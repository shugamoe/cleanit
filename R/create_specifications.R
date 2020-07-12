source("R/functions.R")

dataCS <- getDataCS()
# Filter out ridiculous normmu values
dataPB <- getDataPB() %>%
  filter(newlb + newub < 100)

# Find Emails that were in the fall12, or spring13, or both clean sample periods
# Use min and group by email to count people as
emailFall12Key <- dataCS %>%
  filter(period == "fall12") %>%
  select(email, freshdum, treatme, offered) %>%
  group_by(email, freshdum) %>%
  summarise(treatme = min(sum(treatme), 1), offered = mean(offered)) %>%
  distinct()

emailSpring13Key <- dataCS %>%
  filter(period == "spring13") %>%
  select(email, freshdum, treatme, offered) %>%
  group_by(email, freshdum) %>%
  summarise(treatme = min(sum(treatme), 1), offered = mean(offered)) %>%
  distinct()

# Every specifications for (A-J)(1-2)
# Not super efficient implementation. Thanks small data
# Vim practice
fullSpecKey <- list(
  # A
  # In Fall 2012 Clean Sample	In Spring 2013 Clean Sample	Inexperienced	Control
  # A1 Should be nobody since all of spring13's clean sample have offered == 1
  A1 = dataPB %>%
    filter(period == "fall12") %>%
    inner_join(
      semi_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    # Use clean sample freshman
    filter(freshdum.cs == 1, offered == 0, treatme == 0) %>%
    mutate(group = "A1")
  ,
  A2 = dataPB %>%
    filter(period == "spring13") %>%
    inner_join(
      semi_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 1, offered == 0, treatme == 0) %>%
    mutate(group = "A2")
  ,
  # B
  # In Fall 2012 Clean Sample	In Spring 2013 Clean Sample	Inexperienced	Treated
  B1 = dataPB %>%
    filter(period == "fall12") %>%
    inner_join(
      semi_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 1, offered == 1, treatme == 1) %>%
    mutate(group = "B1")
  ,
  B2 = dataPB %>%
    filter(period == "spring13") %>%
    inner_join(
      semi_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 1, offered == 1, treatme == 1) %>%
    mutate(group = "B2")
  ,
  # In Fall 2012 Clean Sample	In Spring 2013 Clean Sample	Experienced	Control
  C1 = dataPB %>%
    filter(period == "fall12") %>%
    inner_join(
      semi_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 0, offered == 0, treatme == 0) %>%
    mutate(group = "C1")
  ,
  C2 = dataPB %>%
    filter(period == "spring13") %>%
    inner_join(
      semi_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 0, offered == 0, treatme == 0) %>%
    mutate(group = "C2")

  ,
  # In Fall 2012 Clean Sample	In Spring 2013 Clean Sample	Experienced	Treated
  D1 = dataPB %>%
    filter(period == "fall12") %>%
    inner_join(
      semi_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 0, offered == 1, treatme == 1) %>%
    mutate(group = "D1")
  ,
  D2 = dataPB %>%
    filter(period == "spring13") %>%
    inner_join(
      semi_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 0, offered == 1, treatme == 1) %>%
    mutate(group = "D2")
  ,
  # In Fall 2012 Clean Sample	Not In Spring 2013 Clean Sample	Inexperienced	Control
  E1 = dataPB %>%
    filter(period == "fall12") %>%
    inner_join(
      anti_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 1, offered == 0, treatme == 0) %>%
    mutate(group = "E1")
  ,
  E2 = dataPB %>%
    filter(period == "spring13") %>%
    inner_join(
      anti_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 1, offered == 0, treatme == 0) %>%
    mutate(group = "E2")
  ,
  # In Fall 2012 Clean Sample	Not In Spring 2013 Clean Sample	Inexperienced	Treated
  F1 = dataPB %>%
    filter(period == "fall12") %>%
    inner_join(
      anti_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 1, offered == 1, treatme == 1) %>%
    mutate(group = "F1")
  ,
  F2 = dataPB %>%
    filter(period == "spring13") %>%
    inner_join(
      anti_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 1, offered == 1, treatme == 1) %>%
    mutate(group = "F2")
  ,
  # In Fall 2012 Clean Sample	Not In Spring 2013 Clean Sample	Experienced	Control
  G1 = dataPB %>%
    filter(period == "fall12") %>%
    inner_join(
      anti_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 0, offered == 0, treatme == 0) %>%
    mutate(group = "G1")
  ,
  G2 = dataPB %>%
    filter(period == "spring13") %>%
    inner_join(
      anti_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 0, offered == 0, treatme == 0) %>%
    mutate(group = "G2")
  ,
  # In Fall 2012 Clean Sample	Not In Spring 2013 Clean Sample	Experienced	Treated
  H1 = dataPB %>%
    filter(period == "fall12") %>%
    inner_join(
      anti_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 0, offered == 1, treatme == 1) %>%
    mutate(group = "H1")
  ,
  H2 = dataPB %>%
    filter(period == "spring13") %>%
    inner_join(
      anti_join(emailFall12Key,
           emailSpring13Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 0, offered == 1, treatme == 1) %>%
    mutate(group = "H2")
  ,
  # Not In Fall 2012 Clean Sample	In Spring 2013 Clean Sample	Inexperienced	Only Control Groups
  I1 = dataPB %>%
    filter(period == "fall12") %>%
    inner_join(
      anti_join(emailSpring13Key,
           emailFall12Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 1, offered == 0, treatme == 0) %>%
    mutate(group = "I1")
  ,
  I2 = dataPB %>%
    filter(period == "spring13") %>%
    inner_join(
      anti_join(emailSpring13Key,
           emailFall12Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 1, offered == 0, treatme == 0) %>%
    mutate(group = "I2")
  ,
  # Not In Fall 2012 Clean Sample	In Spring 2013 Clean Sample	Experienced	Only Control Groups
  J1 = dataPB %>%
    filter(period == "fall12") %>%
    inner_join(
      anti_join(emailSpring13Key,
           emailFall12Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 0, offered == 0, treatme == 0) %>%
    mutate(group = "J1")
  ,
  J2 = dataPB %>%
    filter(period == "spring13") %>%
    inner_join(
      anti_join(emailSpring13Key,
           emailFall12Key, by = "email"),
      by = "email",
      suffix = c(".pb", ".cs")
    ) %>%
    filter(freshdum.cs == 0, offered == 0, treatme == 0) %>%
    mutate(group = "J2")
)

# Create Specification 1
spec1InexperControl <- bind_rows(
 fullSpecKey$A1,
 fullSpecKey$E1
) %>%
  mutate(spec = 1,
         name = "Inexperienced Control"
         )

spec1InexperTreated <- bind_rows(
 fullSpecKey$B2,
 fullSpecKey$F1
) %>%
  mutate(spec = 1,
         name = "Inexperienced Treated"
         )


spec1ExperControl <- bind_rows(
 fullSpecKey$C1,
 fullSpecKey$C2,
 fullSpecKey$G1
) %>%
  mutate(spec = 1,
         name = "Experienced Control"
         )


spec1ExperTreated <- bind_rows(
 fullSpecKey$D1,
 fullSpecKey$D2,
 fullSpecKey$H1,
 fullSpecKey$H2
) %>%
  mutate(spec = 1,
         name = "Experienced Treated"
         )

spec1Full <- bind_rows(spec1ExperControl,
                       spec1ExperTreated,
                       spec1InexperControl,
                       spec1InexperTreated)

# Create Specification 2
spec2InexperControl <- bind_rows(
 fullSpecKey$A1,
 fullSpecKey$A2,
 fullSpecKey$E1,
 fullSpecKey$E2,
 fullSpecKey$I1,
 fullSpecKey$I2,
) %>%
  mutate(spec = 2,
         name = "Inexperienced Control"
         )

spec2InexperTreated <- bind_rows(
 fullSpecKey$B1,
 fullSpecKey$B2,
 fullSpecKey$F1,
 fullSpecKey$F2
) %>%
  mutate(spec = 2,
         name = "Inexperienced Treated"
         )


spec2ExperControl <- bind_rows(
 fullSpecKey$C1,
 fullSpecKey$C2,
 fullSpecKey$G1,
 fullSpecKey$G2,
 fullSpecKey$J1,
 fullSpecKey$J2
) %>%
  mutate(spec = 2,
         name = "Experienced Control"
         )


spec2ExperTreated <- bind_rows(
 fullSpecKey$D1,
 fullSpecKey$D2,
 fullSpecKey$H1,
 fullSpecKey$H2
) %>%
  mutate(spec = 2,
         name = "Experienced Treated"
         )

spec2Full <- bind_rows(spec2ExperControl,
                       spec2ExperTreated,
                       spec2InexperControl,
                       spec2InexperTreated)

specBothFull <- bind_rows(spec1Full, spec2Full)
