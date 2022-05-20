home_index <- read.csv("https://storage.googleapis.com/kagglesdsdata/datasets/1947423/3502038/ZHVI.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20220519%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20220519T013616Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=17805c099a80e40d5d0d2c8e49744485c75f1f73a583369ba7bc463105b8f14e317cf99157c65cb1af7e09b574a029b6280c28feda772bd0b21dccf00ea1a5da333acbbdf7d9c3bb8c573a12ee93454d77fecce047ab87537632c3f8e74b90da9b448765f61b6e79e90b57d9f1ceb012feb093ba9652abc08bc556ceb4ea2e1c25cf795dc95cf74657add57af4f49720fd202b7ace3a2ef70b3207ae7d61e755074ef32c9d582d35574deda9fee9f62b987fecb63a8419a029a553f2bd62cc09f6818cc309ef4de6040e4710f7e9a3967f152b82aca59b5ea139898589b307294ea1c9adbc7bd46a86d073da0d2256f3896256e0806116c86af4e28b1e461823")
living_wage <-read.csv("livingwage (2).csv.xls")
kids_spending <- read.csv("https://storage.googleapis.com/kagglesdsdata/datasets/1236613/2063138/State-by-State%20Spending%20on%20Kids.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20220519%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20220519T013918Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=9b8e9e857ee2654c27065b2628e796ce6f65852f7d58e2df8344adb94627bb27d3e34790d31590f6e73c630447659027860ad0944b047058e75028150fea7a91673142395982fa3565022b245cb8067bad779c97addc7a3838212aadd20b26983ee40520d3665bcb6863c696701b2ca75b17ecbfd4bf3356c8876128debe05c7d7314c6fc9e7025bad7203fa2aee98a0499848da4220ff14492e9a38f5d0a372405f31fa6a1c238a631984a06ad2408221a0e42d6ab48dc938738aca29a67a091618d79254796cd84c9b56266998a16e84b8d67d7124b01eb2652908dfdcfe80a7fa54731e24922d3d74bdfc77a42f3d8efb45fbab3ffbb3dccc09850bb5db87")

library(dplyr)
library(ggplot2)

grouped_states_spending <- kids_spending %>%
  select(state, X2010) %>%
  group_by(state) %>%
  summarise("kids_spending_2010" = sum(X2010))

grouped_states_pop <- living_wage %>%
  select(state, population_2010, ) %>%
  group_by(state) %>%
  summarise("state_populations_2010" = sum(population_2010))

kids_spending_and_living_wage <- merge(grouped_states_pop, grouped_states_spending, by = "state", all.x = TRUE)

pop_vs_kids_spending_2010 <- kids_spending_and_living_wage %>%
  select(state, kids_spending_2010, state_populations_2010)%>%
  arrange(-state_populations_2010)%>%
  filter(state_populations_2010 > 2500000 | state_populations_2010 < 500000)


ggplot(pop_vs_kids_spending_2010, aes(x=kids_spending_2010, y = state_populations_2010, format(scientific = FALSE))) +
  geom_point(aes(col = state)) + 
  ylab("State Population") +
  xlab("Spending on Kids") +
  ggtitle("Populations and Kids Spending in 2010")
  