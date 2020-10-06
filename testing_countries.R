x <- df[which(df$country=="SWE"),]

sweden <- lm(policy_support_tot
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
              + (moral_identity_internal + moral_identity_symbolic) : (moral_circle+political_ideology)
              , data = x
              )
summary(sweden)

x <- df[which(df$country=="USA"),]

USA <- lm(policy_support_tot
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
             + (moral_identity_internal + moral_identity_symbolic) : (moral_circle+political_ideology)
             , data = x
)
summary(USA)


x <- df[which(df$country=="IRL"),]

IRL <- lm(policy_support_tot
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
          + (moral_identity_internal + moral_identity_symbolic) : (moral_circle+political_ideology)
          , data = x
)
summary(IRL)

x <- df %>% group_by(country) %>% do(data = (.)) %>% with( set_names(data, country) )

lm_fun <- function(x){
  lm(policy_support_tot
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
          + (moral_identity_internal + moral_identity_symbolic) : (moral_circle+political_ideology)
          , data = x
     )
}


all_lms <- lapply(x, lm_fun)
lapply(all_lms, summary)


levels(df$country)
table(df$country)

loop_sort <- function(ppn){
  corrplot(z[[ppn]], title = paste(names(z[ppn])))

  }
lapply(1:14, loop_sort)

lapply(16:66, loop_sort)


# levels(df$country)
# table(df$country)
# 
# loop_sort <- function(ppn){
#   corrplot(z[[ppn]], title = paste(names(z[ppn])))
#   
#   }
# lapply(1:14, loop_sort)
#  
# lapply(16:66, loop_sort)