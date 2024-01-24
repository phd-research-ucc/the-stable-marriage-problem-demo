# Meta Data ---------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-16
# Updated on:   2024-01-16
#
# Description:  The script is created for trying 
#               new and old concepts of 
#               The Stable Marriage Problem
#
# Location:     script/01_quick_start.R
#


# Setup -------------------------------------------------------------------


source('script/_load_packages.R')


# Script ------------------------------------------------------------------

mate_pool <- data.frame(
  sex = c( 'm','m','m','m','f','f','f','f' ),
  name = c(
    'Max',
    'Mason',
    'Mitchell',
    'Maximilian',
    'Fara',
    'Fiona',
    'Felicity',
    'Florentina'
  ),
  is_engaged = c( NA, NA, NA, NA, NA, NA, NA, NA ),
  order = c( NA, NA, NA, NA, NA, NA, NA, NA ),
  preference = c( NA, NA, NA, NA, NA, NA, NA, NA ),
  priority = c( NA, NA, NA, NA, NA, NA, NA, NA ),
  state = c( NA, NA, NA, NA, NA, NA, NA, NA ),
  is_picked = c( NA, NA, NA, NA, NA, NA, NA, NA )
)



# Setup Preferences -------------------------------------------------------

mates_with_preferences <- mate_pool


# Basic Gale-Shapley Algorithm --------------------------------------------

stable_merriages <- mates_with_preferences
m_index <- 1

repeat {
  
  current_preference <- stable_merriages |> 
    filter(
      sex == 'm',
      is_engeged != TRUE,
      order == m_index
    )
  
  stable_marriages <- stable_merriages |> 
    mutate(
      
    )
  
  # check whether some man is free start
  is_free_man <- stable_merriages |> 
    filter(
      sex == 'm',
      is_picked != TRUE
    ) |> 
    nrow() == 0
  
  if (is_free_man) {
    break
  }
  # check whether some man is free end.
  
  i <- i + 1
}



# Alternative Approach ----------------------------------------------------

males <- c(
  'Max',
  'Mason',
  'Mitchell',
  'Maximilian'
)
females <- c(
  'Fara',
  'Fiona',
  'Felicity',
  'Florentina'
)
male_preferences <- matrix(1:4, nrow = 4, ncol = 4)
female_preferences <- matrix(1:4, nrow = 4, ncol = 4)

male_engagments <- c(NA, NA, NA, NA)
female_engagments <- c(NA, NA, NA, NA)




males <- data.frame(
  m_name = factor( c('A', 'A', 'A', 'B', 'B', 'B', 'C', 'C', 'C') ), 
  f_name = factor( c('x', 'y', 'z', 'x', 'y', 'z', 'x', 'y', 'z') ),
  m_pref = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  row.names = NULL, 
  check.rows = FALSE, 
  check.names = TRUE
)

females <- data.frame(
  f_name = factor( c('x', 'x', 'x', 'y', 'y', 'y', 'z', 'z', 'z') ), 
  m_name = factor( c('A', 'B', 'C', 'A', 'B', 'C', 'A', 'B', 'C') ),
  f_pref = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  row.names = NULL, 
  check.rows = FALSE, 
  check.names = TRUE
)

engagements <- males |> 
  right_join(
    females,
    by = join_by(m_name, f_name)
  ) |> 
  mutate (
    is_engaged = FALSE,
    was_proposed = FALSE,
    was_declined = FALSE,
    was_changed = FALSE
  ) |> 
  arrange(m_pref)
  

total_steps <- length(engagements)
step <- 0
repeat{
  if ( all(engagements$is_engaged) ) {
    break
  }
  
  step <- (step + 1) %% total_steps
  pare <- engagements[step, ]
  
  if (pare$is_engaged) {
    next
  }
  
  if (pare$was_proposed) {
    next
  }
  
  pare$is_proposed <- TRUE
  is_women_engaged <- any(
    engagements$is_engaged[engagements$f_name == pare$f_name]
  )
  if (!is_women_engaged) {
    engagements[step]$is_engaged <- TRUE
    next
  }
  
  previous_pare <- engagements[
    engagements$m_name == pare$m_name,
    engagements$f_name == pare$f_name
  ]
  if (pare$f_pref < previous_pare$f_pref) {
    engagements[step]$is_engaged <- TRUE
    engagements$was_changed[
      engagements$m_name == pare$m_name,
      engagements$f_name == pare$f_name
    ] <- TRUE
    engagements$is_engaged[
      engagements$m_name == pare$m_name,
      engagements$f_name == pare$f_name
    ] <- FALSE
    next
  }
  
  engagements[step]$was_declined <- TRUE
}


for (step in 1:num_females){
  male_pool <- male_df |>
    
    filter(
      rank == step
    ) |> 
    
    arrange(
      is_engaged
    )
    
  
  for (proposal in 1:num_males) {
    if (male_pool&is_engaged[proposal]) {
      next
    }
    
    current_engagement <- male_pool[proposal]
    selection_rank <- female_preferences_df$rank[
      female_preferences_df$name == current_engagement$preference &
      female_preferences_df$preference == current_engagement$name
    ]
    previous_engagement <- male_df[
      male_df$is_engaged == TRUE,
      male_df$preference == selection
    ]
    
    if ( !is.na(previous_engagement) ) {
      
    }
  }
}


# proposals_df <- data.frame(
#     step = rep(1:num_males, each = num_females)
#   ) |> 
    
#   mutate(
#     female = male_df$preference[male_df$rank == step],
#     male = male_df$name[male_df$rank == step],
#     male_rank = female_df$rank[
#       female_df$name == female &
#       female_df$preference == male
#     ]
#   ) |> 
  
#   group_by(
#     step,
#     female
#   ) |> 
  
#   summarise(
#     top_male = max(male)
#   )

for (m in unique(males$m_name)) {
  w <- engagements[
    engagements$m_name == m & 
      !engagements$was_proposed
  ][1]
  engagements$was_proposed[
    engagements$m_name == m & 
      engagements$f_name == w$f_name
  ][1] <- TRUE
  
  if (!w$is_engaged) {
    engagements$is_engaged[
      engagements$m_name == m & 
        engagements$f_name == w$f_name
    ] <- TRUE
    next
  }
  
  mf <- engagements[
    engagements$f_name == w$f_name & 
      engagements$is_engaged
  ]
  if (w$f_pref < mf$f_pref) {
    engagements$is_engaged[
      engagements$m_name == m & 
        engagements$f_name == w$f_name
    ] <- TRUE
    engagements$is_engaged[
      engagements$m_name == mf$m_name & 
        engagements$f_name == w$f_name
    ] <- FALSE
    engagements$was_changed[
      engagements$m_name == mf$m_name & 
        engagements$f_name == w$f_name
    ] <- TRUE
    next
  }
  engagements$is_declined[
    engagements$m_name == m & 
      engagements$f_name == w$f_name
  ] <- TRUE
}
