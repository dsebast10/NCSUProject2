m <- lm(data = df_means, `Rate of Home Recoveries`~.-Abbr-State)
m
summary(m)
m_null <- lm(data = df_means, `Rate of Home Recoveries`~1)
M_step <- step(object=m_null, scope = list(lower=m_null, upper = m), direction = "forward")

m2 <- lm(formula = `Rate of Home Recoveries` ~ `Assault Weapons Ban` + 
     `50 Caliber Ban` + `Silencers Prohibited` + `Initial Permit Cost` + 
     `Registration of Firearms` + `Stricter Minimum Age` + `Training or Testing Requirement for Carry Permit` + 
     `Waiting Period`, data = df_means)

m2
summary(m2)


%>% select(`Assault Weapons Ban`,`50 Caliber Ban`, `Silencers Prohibited`,
           `Initial Permit Cost`, `Registration of Firearms`,
           `Stricter Minimum Age`, `Training or Testing Requirement for Carry Permit`,
           `Waiting Period`)))