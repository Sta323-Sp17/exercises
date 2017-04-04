# Lets try to create a table that has a new column - abv_avg which 
# contains how much more (or less) than the average, for their 
# department, each person is paid.

library(dplyr)

employees = data.frame(name   = c("Alice","Bob","Carol","Dave","Eve","Frank"),
                       email  = c("alice@company.com", "bob@company.com",
                                  "carol@company.com", "dave@company.com",
                                  "eve@company.com",   "frank@comany.com"),
                       salary = c(52000, 40000, 30000, 33000, 44000, 37000),
                       dept   = c("Accounting", "Accounting","Sales",
                                  "Accounting","Sales","Sales"),
                       stringsAsFactors = FALSE)

employees %>% group_by(dept) %>% summarize(avg_salary = mean(salary)) %>%
  left_join(employees, .) %>% mutate(abv_avg = salary - avg_salary)



"SELECT *, (salary - avg_salary) AS abv_avg  FROM employees NATURAL LEFT JOIN (SELECT dept, AVG(salary) as avg_salary FROM employees GROUP BY dept);"
