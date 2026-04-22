# ============================================
# 07 — VISUALISATIONS
# ============================================

# Load dependencies
load("dissertation_working_data.RData")
library(xts)
library(zoo)
library(ggplot2)

# Load event dates
event_dates <- as.Date(c(
  "2009-05-11",
  "2009-07-10",
  "2009-09-10",
  "2010-07-28",
  "2010-12-03",
  "2010-12-15",
  "2011-07-20"
))

names(event_dates) <- c("E1_PLATO_PR", "E2_Prasugrel_FDA",
                        "E3_PLATO_NEJM", "E4_FDA_AdCom",
                        "E5_EMA_Auth", "E6_FDA_CRL",
                        "E7_FDA_Approval")

# Convert AZN to dataframe for ggplot
AZN_df <- data.frame(
  Date  = index(AZN),
  Price = as.numeric(AZN)
)

# Create event labels dataframe
event_df <- data.frame(
  Date  = as.Date(event_dates),
  Label = c("E1\nPLATO PR", "E2\nPrasugrel FDA", 
            "E3\nPLATO NEJM", "E4\nFDA AdCom",
            "E5\nEMA Auth", "E6\nFDA CRL", 
            "E7\nFDA Approval")
)

# Plot
AZN_plot <- ggplot(AZN_df, aes(x = Date, y = Price)) +
  geom_line(colour = "#003087", linewidth = 0.7) +
  geom_vline(data = event_df, 
             aes(xintercept = as.numeric(Date)),
             colour = "red", linetype = "dashed", 
             linewidth = 0.5) +
  geom_text(data = event_df,
            aes(x = Date, y = max(AZN_df$Price) * 0.95, 
                label = Label),
            size = 2.5, colour = "red", 
            hjust = -0.1) +
  labs(
    title    = "AstraZeneca Share Price (2006-2012)",
    subtitle = "Adjusted closing price (GBX) with key Ticagrelor development milestones",
    x        = "Date",
    y        = "Adjusted Closing Price (GBX)",
    caption  = "Source: Yahoo Finance. Vertical lines indicate primary event dates."
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, colour = "grey40"),
    plot.caption  = element_text(size = 8, colour = "grey40"),
    axis.text     = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

# Display plot
print(AZN_plot)

# Save plot
ggsave("AZN_price_series.png", 
       plot = AZN_plot, 
       width = 10, height = 6, dpi = 300)

# Staggered label positions to avoid overlap
event_df <- data.frame(
  Date   = as.Date(event_dates),
  Label  = c("E1\nPLATO PR", 
             "E2\nPrasugrel\nFDA", 
             "E3\nPLATO\nNEJM", 
             "E4\nFDA\nAdCom",
             "E5\nEMA\nAuth", 
             "E6\nFDA\nCRL", 
             "E7\nFDA\nApproval"),
  Height = c(2100, 2250, 2100, 2250, 2100, 2250, 2100)
)

AZN_plot <- ggplot(AZN_df, aes(x = Date, y = Price)) +
  geom_line(colour = "#003087", linewidth = 0.7) +
  geom_vline(data = event_df,
             aes(xintercept = as.numeric(Date)),
             colour = "red", linetype = "dashed",
             linewidth = 0.5) +
  geom_label(data = event_df,
             aes(x = Date, y = Height, label = Label),
             size = 2.5, colour = "red",
             fill = "white", label.size = 0.3,
             hjust = 0.5, lineheight = 0.9) +
  labs(
    title    = "AstraZeneca Share Price (2006-2012)",
    subtitle = "Adjusted closing price (GBX) with key Ticagrelor development milestones",
    x        = "Date",
    y        = "Adjusted Closing Price (GBX)",
    caption  = "Source: Yahoo Finance. Vertical dashed lines indicate primary event dates."
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    plot.caption     = element_text(size = 8, colour = "grey40"),
    axis.text        = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

print(AZN_plot)

ggsave("AZN_price_series.png",
       plot = AZN_plot,
       width = 10, height = 6, dpi = 300)

# Build AZN CAR dataframe
AZN_CAR_df <- data.frame(
  Event = names(event_dates),
  CAR   = c(
    significance_results$E1_PLATO_PR$AZN$CAR,
    significance_results$E2_Prasugrel_FDA$AZN$CAR,
    significance_results$E3_PLATO_NEJM$AZN$CAR,
    significance_results$E4_FDA_AdCom$AZN$CAR,
    significance_results$E5_EMA_Auth$AZN$CAR,
    significance_results$E6_FDA_CRL$AZN$CAR,
    significance_results$E7_FDA_Approval$AZN$CAR
  ),
  Stars = c(
    significance_results$E1_PLATO_PR$AZN$stars,
    significance_results$E2_Prasugrel_FDA$AZN$stars,
    significance_results$E3_PLATO_NEJM$AZN$stars,
    significance_results$E4_FDA_AdCom$AZN$stars,
    significance_results$E5_EMA_Auth$AZN$stars,
    significance_results$E6_FDA_CRL$AZN$stars,
    significance_results$E7_FDA_Approval$AZN$stars
  )
)

# Clean event labels for x axis
AZN_CAR_df$Label <- c(
  "E1\nPLATO PR\n(May 09)",
  "E2\nPrasugrel\nFDA (Jul 09)",
  "E3\nPLATO\nNEJM (Sep 09)",
  "E4\nFDA AdCom\n(Jul 10)",
  "E5\nEMA Auth\n(Dec 10)",
  "E6\nFDA CRL\n(Dec 10)",
  "E7\nFDA Approval\n(Jul 11)"
)

# Keep label order fixed
AZN_CAR_df$Label <- factor(AZN_CAR_df$Label, levels = AZN_CAR_df$Label)

# Plot
CAR_plot <- ggplot(AZN_CAR_df, aes(x = Label, y = CAR * 100, 
                                   fill = CAR > 0)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
  geom_text(aes(label = Stars,
                y = ifelse(CAR > 0, CAR * 100 + 0.3, CAR * 100 - 0.3)),
            size = 5, vjust = 0.5) +
  scale_fill_manual(values = c("TRUE" = "#003087", "FALSE" = "#CC0000"),
                    guide = "none") +
  labs(
    title    = "AstraZeneca Cumulative Abnormal Returns by Event",
    subtitle = "Event window (-1, +1) | Estimation window (-120, -20)",
    x        = NULL,
    y        = "CAR (%)",
    caption  = "*** p<0.01  * p<0.10\nSource: Author calculations using Yahoo Finance data."
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    plot.caption     = element_text(size = 8, colour = "grey40"),
    axis.text.x      = element_text(size = 8, lineheight = 0.9),
    axis.text.y      = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(CAR_plot)

ggsave("~/Desktop/AZN_CAR_chart.png",
       plot = CAR_plot,
       width = 10, height = 6, dpi = 300)

# Build combined CAR dataframe for all firms
firms <- c("AZN", "SAN", "BMY", "LLY", "DS")

all_CAR_df <- data.frame()

for (firm in firms) {
  firm_df <- data.frame(
    Event = names(event_dates),
    Label = c(
      "E1\nPLATO PR\n(May 09)",
      "E2\nPrasugrel\nFDA (Jul 09)",
      "E3\nPLATO\nNEJM (Sep 09)",
      "E4\nFDA AdCom\n(Jul 10)",
      "E5\nEMA Auth\n(Dec 10)",
      "E6\nFDA CRL\n(Dec 10)",
      "E7\nFDA Approval\n(Jul 11)"
    ),
    CAR   = c(
      significance_results$E1_PLATO_PR[[firm]]$CAR,
      significance_results$E2_Prasugrel_FDA[[firm]]$CAR,
      significance_results$E3_PLATO_NEJM[[firm]]$CAR,
      significance_results$E4_FDA_AdCom[[firm]]$CAR,
      significance_results$E5_EMA_Auth[[firm]]$CAR,
      significance_results$E6_FDA_CRL[[firm]]$CAR,
      significance_results$E7_FDA_Approval[[firm]]$CAR
    ),
    Stars = c(
      significance_results$E1_PLATO_PR[[firm]]$stars,
      significance_results$E2_Prasugrel_FDA[[firm]]$stars,
      significance_results$E3_PLATO_NEJM[[firm]]$stars,
      significance_results$E4_FDA_AdCom[[firm]]$stars,
      significance_results$E5_EMA_Auth[[firm]]$stars,
      significance_results$E6_FDA_CRL[[firm]]$stars,
      significance_results$E7_FDA_Approval[[firm]]$stars
    ),
    Firm = firm
  )
  all_CAR_df <- rbind(all_CAR_df, firm_df)
}

# Fix factor ordering
all_CAR_df$Label <- factor(all_CAR_df$Label, levels = unique(all_CAR_df$Label))
all_CAR_df$Firm  <- factor(all_CAR_df$Firm, levels = firms)

# Plot
combined_CAR_plot <- ggplot(all_CAR_df, 
                            aes(x = Label, y = CAR * 100, fill = Firm)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
  geom_text(aes(label = Stars,
                y = ifelse(CAR > 0, CAR * 100 + 0.4, CAR * 100 - 0.4)),
            position = position_dodge(width = 0.7),
            size = 3.5, vjust = 0.5) +
  scale_fill_manual(values = c(
    "AZN" = "#003087",
    "SAN" = "#CC0000", 
    "BMY" = "#006400",
    "LLY" = "#FF8C00",
    "DS"  = "#800080"
  )) +
  labs(
    title    = "Cumulative Abnormal Returns by Event and Firm",
    subtitle = "Event window (-1, +1) | Estimation window (-120, -20)",
    x        = NULL,
    y        = "CAR (%)",
    fill     = "Firm",
    caption  = "*** p<0.01  ** p<0.05  * p<0.10\nSource: Author calculations using Yahoo Finance data."
  ) +
  theme_minimal() +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 10, colour = "grey40"),
    plot.caption       = element_text(size = 8, colour = "grey40"),
    axis.text.x        = element_text(size = 7, lineheight = 0.9),
    axis.text.y        = element_text(size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "top"
  )

print(combined_CAR_plot)

ggsave("~/Combined_CAR_chart.png",
       plot = combined_CAR_plot,
       width = 14, height = 7, dpi = 300)

# Function to extract price window around event
get_price_window <- function(price_series, event_date, 
                             window_days = 15) {
  
  all_dates <- index(price_series)
  event_pos <- which(all_dates >= event_date)[1]
  
  start_pos <- event_pos - window_days
  end_pos   <- event_pos + window_days
  
  if (start_pos < 1) start_pos <- 1
  if (end_pos > length(all_dates)) end_pos <- length(all_dates)
  
  window_data <- price_series[start_pos:end_pos]
  
  # Create relative day index (-15 to +15)
  rel_days <- seq(start_pos - event_pos, end_pos - event_pos)
  
  df <- data.frame(
    RelDay = rel_days,
    Price  = as.numeric(window_data),
    Date   = as.Date(index(window_data))
  )
  
  return(df)
}
# Normalise to day -15 instead of day 0
normalize_to_start <- function(df) {
  base_price <- df$Price[df$RelDay == min(df$RelDay)]
  df$NormPrice <- (df$Price / base_price) * 100
  return(df)
}

E1_AZN <- normalize_to_start(E1_AZN)
E1_SAN <- normalize_to_start(E1_SAN)
E1_BMY <- normalize_to_start(E1_BMY)
E1_LLY <- normalize_to_start(E1_LLY)
E1_DS  <- normalize_to_start(E1_DS)

# Recombine
E1_df <- rbind(E1_AZN, E1_SAN, E1_BMY, E1_LLY, E1_DS)
E1_df$Firm <- factor(E1_df$Firm, levels = c("AZN", "SAN", "BMY", "LLY", "DS"))

# Replot
E1_plot <- ggplot(E1_df, aes(x = RelDay, y = NormPrice,
                             colour = Firm, linewidth = Firm)) +
  geom_line() +
  geom_vline(xintercept = 0, colour = "black",
             linetype = "dashed", linewidth = 0.7) +
  scale_colour_manual(values = c(
    "AZN" = "#003087",
    "SAN" = "#CC0000",
    "BMY" = "#006400",
    "LLY" = "#FF8C00",
    "DS"  = "#800080"
  )) +
  scale_linewidth_manual(values = c(
    "AZN" = 1.2,
    "SAN" = 0.7,
    "BMY" = 0.7,
    "LLY" = 0.7,
    "DS"  = 0.7
  )) +
  annotate("text", x = 0.5, y = max(E1_df$NormPrice) * 0.99,
           label = "PLATO\nPress Release",
           hjust = 0, size = 3, colour = "black") +
  labs(
    title    = "E1 — PLATO Press Release (11 May 2009)",
    subtitle = "Normalised share prices — 15 trading days before and after event",
    x        = "Trading days relative to event",
    y        = "Normalised Price (Day -15 = 100)",
    colour   = "Firm",
    caption  = "Source: Yahoo Finance. Prices normalised to 100 at day -15."
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    plot.caption     = element_text(size = 8, colour = "grey40"),
    axis.text        = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "top"
  ) +
  guides(linewidth = "none")

print(E1_plot)

ggsave("~/Desktop/E1_price_window.png",
       plot = E1_plot,
       width = 10, height = 6, dpi = 300)

# Get price window for E4 - AZN only
E4_date <- as.Date("2010-07-28")

E4_AZN <- get_price_window(AZN, E4_date, 15)
E4_AZN$Firm <- "AZN"
E4_AZN <- normalize_to_start(E4_AZN)

# Plot
E4_plot <- ggplot(E4_AZN, aes(x = RelDay, y = NormPrice)) +
  geom_line(colour = "#003087", linewidth = 1.2) +
  geom_vline(xintercept = 0, colour = "black",
             linetype = "dashed", linewidth = 0.7) +
  annotate("text", x = 0.5, y = max(E4_AZN$NormPrice) * 0.99,
           label = "FDA Advisory\nCommittee\n7-1 Vote",
           hjust = 0, size = 3, colour = "black") +
  labs(
    title    = "E4 — FDA Advisory Committee (28 July 2010)",
    subtitle = "AstraZeneca normalised share price — 15 trading days before and after event",
    x        = "Trading days relative to event",
    y        = "Normalised Price (Day -15 = 100)",
    caption  = "Source: Yahoo Finance. Prices normalised to 100 at day -15."
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    plot.caption     = element_text(size = 8, colour = "grey40"),
    axis.text        = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

print(E4_plot)

ggsave("~/Desktop/E4_price_window.png",
       plot = E4_plot,
       width = 10, height = 6, dpi = 300)


# Extended window covering E5 and E6
E5_date <- as.Date("2010-12-03")
E6_date <- as.Date("2010-12-15")

# Get window starting 15 days before E5
E5_E6_AZN <- get_price_window(AZN, E5_date, 
                              window_days = 30)
E5_E6_AZN <- normalize_to_start(E5_E6_AZN)

# Find relative position of E6
all_dates <- index(AZN)
E5_pos <- which(all_dates >= E5_date)[1]
E6_pos <- which(all_dates >= E6_date)[1]
E6_relative <- E6_pos - E5_pos

# Plot
E5_E6_plot <- ggplot(E5_E6_AZN, aes(x = RelDay, y = NormPrice)) +
  geom_line(colour = "#003087", linewidth = 1.2) +
  geom_vline(xintercept = 0, colour = "darkgreen",
             linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = E6_relative, colour = "red",
             linetype = "dashed", linewidth = 0.7) +
  annotate("text", x = 0.5, 
           y = max(E5_E6_AZN$NormPrice) * 0.99,
           label = "E5: EMA\nAuthorisation",
           hjust = 0, size = 3, colour = "darkgreen") +
  annotate("text", x = E6_relative + 0.5,
           y = max(E5_E6_AZN$NormPrice) * 0.93,
           label = "E6: FDA\nRejection",
           hjust = 0, size = 3, colour = "red") +
  labs(
    title    = "E5 & E6 — EMA Authorisation and FDA Rejection (Dec 2010)",
    subtitle = "AstraZeneca normalised share price — 15 days before E5 to 15 days after E6",
    x        = "Trading days relative to EMA authorisation (E5)",
    y        = "Normalised Price (Day -15 = 100)",
    caption  = "Source: Yahoo Finance. Green line = EMA authorisation (3 Dec 2010). Red line = FDA rejection (15 Dec 2010).\nPrices normalised to 100 at day -15 relative to E5."
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    plot.caption     = element_text(size = 8, colour = "grey40"),
    axis.text        = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

print(E5_E6_plot)

ggsave("~/Desktop/E5_E6_price_window.png",
       plot = E5_E6_plot,
       width = 10, height = 6, dpi = 300)

# Load packages
library(ggplot2)
library(xts)
library(zoo)

# Load data
load("dissertation_working_data.RData")

# Reload event dates
event_dates <- as.Date(c(
  "2009-05-11",
  "2009-07-10",
  "2009-09-10",
  "2010-07-28",
  "2010-12-03",
  "2010-12-15",
  "2011-07-20"
))

names(event_dates) <- c("E1_PLATO_PR", "E2_Prasugrel_FDA",
                        "E3_PLATO_NEJM", "E4_FDA_AdCom",
                        "E5_EMA_Auth", "E6_FDA_CRL",
                        "E7_FDA_Approval")

# Price window function
get_price_window <- function(price_series, event_date,
                             window_days = 15) {
  all_dates <- index(price_series)
  event_pos <- which(all_dates >= event_date)[1]
  start_pos <- event_pos - window_days
  end_pos   <- event_pos + window_days
  if (start_pos < 1) start_pos <- 1
  if (end_pos > length(all_dates)) end_pos <- length(all_dates)
  window_data <- price_series[start_pos:end_pos]
  rel_days <- seq(start_pos - event_pos, end_pos - event_pos)
  df <- data.frame(
    RelDay = rel_days,
    Price  = as.numeric(window_data),
    Date   = as.Date(index(window_data))
  )
  return(df)
}

# Normalise to start function
normalize_to_start <- function(df) {
  base_price <- df$Price[df$RelDay == min(df$RelDay)]
  df$NormPrice <- (df$Price / base_price) * 100
  return(df)
}

# Get E7 price windows for all firms
E7_date <- as.Date("2011-07-20")

E7_AZN <- get_price_window(AZN, E7_date, 15)
E7_AZN$Firm <- "AZN"
E7_AZN <- normalize_to_start(E7_AZN)

E7_SAN <- get_price_window(SAN, E7_date, 15)
E7_SAN$Firm <- "SAN"
E7_SAN <- normalize_to_start(E7_SAN)

E7_BMY <- get_price_window(BMY, E7_date, 15)
E7_BMY$Firm <- "BMY"
E7_BMY <- normalize_to_start(E7_BMY)

E7_LLY <- get_price_window(LLY, E7_date, 15)
E7_LLY$Firm <- "LLY"
E7_LLY <- normalize_to_start(E7_LLY)

E7_DS <- get_price_window(DS, E7_date, 15)
E7_DS$Firm <- "DS"
E7_DS <- normalize_to_start(E7_DS)

# Combine
E7_df <- rbind(E7_AZN, E7_SAN, E7_BMY, E7_LLY, E7_DS)
E7_df$Firm <- factor(E7_df$Firm, levels = c("AZN", "SAN", "BMY", "LLY", "DS"))

# Plot
E7_plot <- ggplot(E7_df, aes(x = RelDay, y = NormPrice,
                             colour = Firm, linewidth = Firm)) +
  geom_line() +
  geom_vline(xintercept = 0, colour = "black",
             linetype = "dashed", linewidth = 0.7) +
  scale_colour_manual(values = c(
    "AZN" = "#003087",
    "SAN" = "#CC0000",
    "BMY" = "#006400",
    "LLY" = "#FF8C00",
    "DS"  = "#800080"
  )) +
  scale_linewidth_manual(values = c(
    "AZN" = 1.2,
    "SAN" = 0.7,
    "BMY" = 0.7,
    "LLY" = 0.7,
    "DS"  = 0.7
  )) +
  annotate("text", x = 1, y = 106,
           label = "FDA Final Approval\n(20 July 2011)",
           hjust = 0, size = 3, colour = "black",
           fontface = "italic") +
  labs(
    title    = "E7 — FDA Final Approval (20 July 2011)",
    subtitle = "Normalised share prices — 15 trading days before and after event",
    x        = "Trading days relative to event",
    y        = "Normalised Price (Day -15 = 100)",
    colour   = "Firm",
    caption  = "Source: Yahoo Finance. Prices normalised to 100 at day -15.\nNote: The broad post-approval decline across all firms reflects the global market sell-off triggered by the\nUS sovereign debt downgrade on 5 August 2011, rather than firm-specific reactions to the FDA approval.\nThe market model controls for this market-wide movement when calculating abnormal returns."
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    plot.caption     = element_text(size = 8, colour = "grey40"),
    axis.text        = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "top"
  ) +
  guides(linewidth = "none")

print(E7_plot)

ggsave("~/Desktop/E7_price_window.png",
       plot = E7_plot,
       width = 10, height = 6, dpi = 300)