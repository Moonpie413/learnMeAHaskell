data List = Empty | Cron Int List
data List' t = Empty | C t (List' t)
