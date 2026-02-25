res_m_a %>% 
  select(a1) %>% 
  to_factor()

res_m_a %>% 
  select(a1) %>% 
  to_character() %>% 
  iconv(from = "latin1", to ="UTF-8")
