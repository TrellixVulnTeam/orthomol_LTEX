library(googleAuthR)
library(googleAnalyticsR)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

gar_auth_service('auth/service_account.json')

view_id <- '246084583'

start_date <- '2021-07-01'

end_date <- '2022-06-30'


ga_natal <- google_analytics(view_id,
                             date_range = c(start_date, end_date),
                             metrics = c('users' ,'sessions',  'goal4Completions', 'goal5Completions', 'goal6Completions'),
                             dimensions = c('date', 'campaign', 'source', 'medium'),
                             anti_sample = TRUE) %>% 
  rename(jetzt_bestellen = goal4Completions, kennenlernset_anfordern = goal5Completions, natal_dossier_herunterladen = goal6Completions)

rm(start_date, end_date, view_id)


glimpse(ga_natal)

ga_natal %>% 
  group_by(medium) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  select(medium, users) %>% 
  View()
# Starke linksschiefe Verteilung >> Anders gruppieren bevor wir damit rechnen



ga_natal %>% 
  filter(campaign != '(not set)') %>% 
  mutate(CR = (jetzt_bestellen/users)*100,
         CR = round(CR, 2)) %>% 
  select(1:7, CR)

ga_natal %>% 
  filter(campaign != '(not set)',
         medium == 'display') %>% 
  group_by(campaign, source, medium) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  arrange(desc(users)) %>% 
  mutate(CR_jetzt_bestellen = (jetzt_bestellen/users)*100,
         CR_jetzt_bestellen = round(CR_jetzt_bestellen, 2)) %>% 
  select(-natal_dossier_herunterladen) %>%  # keine Conversions erfasst
  View()


ga_natal %>% 
  filter(campaign == 'DE | DO (Brand) | Search | Orthomol Natal & Natal pre') %>% 
  summary()



ga_natal %>% 
  filter(campaign == 'DE | DO (Brand) | Search | Orthomol Natal & Natal pre') %>% 
  ggplot(aes(date, jetzt_bestellen)) +
  geom_line()



ga_natal %>% 
  filter(campaign != '(not set)') %>% 
  group_by(source, medium) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  View()

ga_natal %>% 
  filter(campaign != '(not set)') %>% 
  group_by(campaign, medium) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  View()

ga_natal %>% 
  filter(campaign == 'orthomol-natal') %>% 
  ggplot(aes(date, jetzt_bestellen)) +
  geom_line()


ga_natal %>% 
  filter(campaign == 'orthomol-natal') %>% 
  ggplot(aes(date, users)) +
  geom_line()


ga_natal %>% 
  filter(campaign == 'orthomol-natal') %>% 
  select(1:7) %>% 
  mutate(CR = (jetzt_bestellen/users)*100,
         CR = round(CR, 2)) 
# Instagram Kampagnen haben eine sehr hohe Conversion Rate


ga_natal %>% 
  filter(medium == 'display') %>% 
  group_by(campaign) %>% 
  summarise(users = sum(users)) %>% 
  arrange(desc(users))


ga_natal %>% 
  filter(medium == 'display',
         campaign != 'DE | Display | Awareness | Orthomol Immun | F11') %>% # Hat sehr viele User an einem Tag auf die Seite gebracht
  ggplot(aes(x = date, y = users)) +
  geom_line() +
  labs(title = 'display campaigns over time')
# Verschiedene Phasen in denen Display geschaltet wurde


ga_natal %>% 
  filter(medium == 'display',
         campaign != 'DE | Display | Awareness | Orthomol Immun | F11') %>% 
  group_by(campaign) %>% 
  summarise(users = sum(users, na.rm = TRUE), 
            jetzt_bestellen = sum(jetzt_bestellen, na.rm = TRUE)) %>% 
  arrange(desc(users))


ga_natal %>% 
  filter(str_detect(campaign, 
                    'DE \\| Taboola \\| Conversion \\| Orthomol arthroplus \\| Probe'
                    )
         ) %>% # Hat sehr viele User an einem Tag auf die Seite gebracht
  ggplot(aes(x = date, y = users)) +
  geom_line()


ga_natal %>% 
  filter(str_detect(campaign, 
                    'DE \\| Taboola \\| Conversion \\| Orthomol Immun \\| Probe'
  )
  ) %>% # Hat sehr viele User an einem Tag auf die Seite gebracht
  ggplot(aes(x = date, y = users)) +
  geom_line()








