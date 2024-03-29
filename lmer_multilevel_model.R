library(lme4)


#### Notes for tomorrow ####

# need to check if interaction can be included as random slope in lmer formula
# if it can then switch all tests for this here instead of lme 
# otherwise need to solve lme error on policy_support_tot
# need to sort out colours




nullmodel <- lmer(policy_support_tot
                  ~ risk_perception_tot
                  + narcissism_admiration_tot
                  + narcissism_rivalry_tot
                  + collective_narcissism_tot
                  + conspiracy_tot
                  + national_ID_tot
                  ### add predictors
                  + moral_identity_internal
                  + moral_identity_symbolic
                  + moral_circle
                  + political_ideology
                  ### add interactions
                  + (moral_identity_internal + moral_identity_symbolic) * (moral_circle+political_ideology)
                  + (1|country), data = df, REML = FALSE)

summary(nullmodel)

rm(labels)


u0 <- ranef(nullmodel, condVar = TRUE) 
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])

countries <- (rownames(u0[[1]]))
u0tab <- cbind(countries, u0[[1]], u0se)
colnames(u0tab) <- c("countries", "u0", "u0se")

u0tab <- u0tab[order(u0tab$u0), ]
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
colnames(u0tab)[4] <- "u0rank"
u0tab


labels_p <- as.vector((u0tab$countries))
breaks_p <- as.numeric(u0tab$u0rank)

ggplot(u0tab,aes(u0,as.numeric(u0rank), color=countries)
       )+
  geom_point(size=.5
  )+
  geom_errorbar(
    aes(xmin=u0-u0se, xmax=u0+u0se), width=.5
  )+
  geom_vline(xintercept = 0, linetype="dashed"
  )+
  scale_y_continuous("Countries",breaks= 1:length(breaks_p)
                     ,labels = labels_p
                     ,sec.axis = dup_axis()
                     )+
  theme_apa(
  )+
  theme(
    panel.grid.minor.y = element_blank()
    ,panel.grid.major.y = element_line()
    ,axis.text=element_text(#family="Times",
      colour = "black",
      size=8
    )
  )

ggplot(u0tab,aes(u0,countries, color=countries)
)+
  geom_point(size=.5
  )+
  geom_errorbar(
    aes(xmin=u0-u0se, xmax=u0+u0se), width=.5
  )+
  geom_vline(xintercept = 0, linetype="dashed"
  )+
  # scale_y_continuous("Countries",breaks= 1:length(breaks_p)
  #                    ,labels = labels_p
  #                    ,sec.axis = dup_axis()
  # )+
  theme_apa(
  )+
  theme(
    panel.grid.minor.y = element_blank()
    ,panel.grid.major.y = element_line()
    ,axis.text=element_text(#family="Times",
      colour = "black",
      size=8
    )
  )




plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional
modes of r.e. for country")
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 
           1.96*u0tab$u0se)
points(u0tab$u0rank,  u0tab$u0, col = 1:67)
#legend(0,0,legend = u0tab$countries, col = 1:67)
abline(h = 0, col = "red")




##### Policy Support #####
# Andy Field's book p. 878
# start building the model
## first test if country should be included

# no country
interceptOnly <-gls(policy_support_tot ~ 1
                    , data = df
                    , method = "ML"
                    , na.action = na.exclude)
summary(interceptOnly)

# country included
randomInterceptOnly <-lme(policy_support_tot ~ 1
                          , data = df
                          , random = ~1|country
                          , method = "ML"
                          , na.action = na.exclude)
summary(randomInterceptOnly)

# check the log likelihoods:
# (smaller is a better fit)
logLik(interceptOnly)*-2
logLik(randomInterceptOnly)*-2
# including country looks like a better fit
# is it significant?
anova(interceptOnly, randomInterceptOnly)
# yes! so we keep country in

# now we start building the model
# I'm going to do this from scratch
# start with control variables model1
# add variables of interest model2
# add the interaction terms model 3

model1 <- lme(policy_support_tot
              ~ risk_perception_tot
              + narcissism_admiration_tot
              + narcissism_rivalry_tot
              + collective_narcissism_tot
              + conspiracy_tot
              + national_ID_tot
              , data = df
              , random = ~1|country
              , method = "ML"
              , na.action = na.exclude)
summary(model1)
model2 <- lme(policy_support_tot
              ~ risk_perception_tot
              + narcissism_admiration_tot
              + narcissism_rivalry_tot
              + collective_narcissism_tot
              + conspiracy_tot
              + national_ID_tot
              ### add predictors
              + moral_identity_internal
              + moral_identity_symbolic
              + moral_circle
              + political_ideology
              , data = df
              , random = ~1|country
              , method = "ML"
              , na.action = na.exclude)
summary(model2)
model3 <- nlme::lme(policy_support_tot
              ~ risk_perception_tot
              + narcissism_admiration_tot
              + narcissism_rivalry_tot
              + collective_narcissism_tot
              + conspiracy_tot
              + national_ID_tot
              ### add predictors
            #  + moral_identity_internal
            #  + moral_identity_symbolic
            #  + moral_circle
            #  + political_ideology
              ### add interactions
              + (moral_identity_internal + moral_identity_symbolic) * (moral_circle+political_ideology)
              , data = df
              , random = ~1|country
              , method = "ML"
              , na.action = na.exclude)
summary(model3)

logLik(model1)*-2
logLik(model2)*-2
logLik(model3)*-2

anova(model1,model2,model3)

#### END ####