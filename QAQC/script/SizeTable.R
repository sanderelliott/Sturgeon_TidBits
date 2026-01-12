## Table for sturgeon data reporting, pulled from EncounterFormQAQC.qmd

sizetable <- enc_IC1 %>%
  group_by(System, Species) %>%
  summarise(
    Captured = n(),
    Tagged = sum(TagManufacturer == "Innovosea", na.rm = TRUE),
    min_TL = round(min(TotalLength..cm., na.rm = TRUE), 1),
    max_TL = round(max(TotalLength..cm., na.rm = TRUE), 1),
    sd_TL  = round(sd(TotalLength..cm., na.rm = TRUE), 1)
  )

sizetable_named <- sizetable %>%
  rename(
    `Number Captured` = Captured,
    `Number Tagged` = Tagged,
    `Minimum TL cm` = min_TL,
    `Maximum TL cm` = max_TL,
    `Standard Deviation TL cm` = sd_TL
  )

library(knitr) 
library(kableExtra)

tab <- kable(sizetable_named, "latex", align = c("l", "c", "c", "c", "c", "c"), 
              caption = "2025 Sturgeon Capture Summary") %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), full_width = F)


library(dplyr)
library(clipr)

sizetable_named %>%
  tibble::as_tibble() %>%
  write_clip(sep = "\t")


library(knitr)
tab <- kable(sizetable_named)

library(DT)
datatable(sizetable_named, rownames = FALSE)

write.csv(sizetable_named, "QAQC/Outputs/sizetable.csv", row.names = FALSE)



library(gt)
gt(rowname_col = NULL)

table_img <- sizetable_named %>%
  gt(rowname_col = NULL) %>%   # <-- prevents Species from becoming a stub
  
  # Remove ALL borders
  tab_options(
    table.background.color = "white",
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "none",
    row_group.border.top.style = "none",
    row_group.border.bottom.style = "none",
    data_row.padding = px(4),
    row.striping.include_table_body = FALSE
  ) %>%
  
  # Remove borders from body cells
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom", "left", "right"),
      color = "white",
      weight = px(0)
    ),
    locations = cells_body()
  ) %>%
  
  # Remove borders from column labels before adding the thick one
  tab_style(
    style = cell_borders(
      sides = c("top", "left", "right"),
      color = "white",
      weight = px(0)
    ),
    locations = cells_column_labels(everything())
  ) %>%
  
  # Bold headers
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  
  # Add ONLY the thick black line under the header
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(3)
    ),
    locations = cells_column_labels(everything())
  )
table_img
