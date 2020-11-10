#' add_calendar_week
#'
#' This function does this and that
#'
#' @param table_for_calendar_week data.table with ..
#'
#' @export
#' @author Monica Butnariuc
#' @import data.table xml2 htmltab foreach
add_calendar_week <- function(table_for_calendar_week) {
  Calendar_week <- as.numeric(strftime(table_for_calendar_week$timestamp, format = "%W"))
  result <- cbind(table_for_calendar_week, Calendar_week)
  return(result)
}


create_table_for_recipe <- function(row_to_calculate) {
  n_weeks <- row_to_calculate$life_in_weeks
  id_col <- replicate(n_weeks, row_to_calculate$recipe_id)
  data.table(
    "RECIPE ID" = id_col,
    period_in_weeks = seq(from = 1, to = n_weeks),
    calendar_week = format(seq(row_to_calculate$timestamp, row_to_calculate$end, by ="1 week"), "%U"),
    year = format(seq(row_to_calculate$timestamp, row_to_calculate$end, by ="1 week"), "%Y"),
    total_adopters = calculate_adopters(row_to_calculate$recipe_id, n_weeks,
                                        row_to_calculate$starting_week, row_to_calculate$timestamp),
    mean_rate = calculate_mean(row_to_calculate$recipe_id, n_weeks,
                               row_to_calculate$starting_week, row_to_calculate$timestamp),
    nr_comments = calculate_comments(row_to_calculate$recipe_id, n_weeks,
                                     row_to_calculate$starting_week, row_to_calculate$timestamp),
    population_size = as.numeric(tail(temporary_table_users2$cumsum_users, as.numeric(n_weeks)))
  )
}


# --> calculation of the nr. of adopters
calculate_adopters <- function(single_recipe, n_weeks, start_week, creation_timestamp) {
  adopters_of_rec <- recipes_rate[recipes_rate$recipe_id == single_recipe,]
  week_adoption <- ceiling(difftime(adopters_of_rec$timestamp, creation_timestamp, units = "weeks"))
  adopters_of_rec <- cbind(adopters_of_rec, week_adoption)
  adoption_by_week <- 0
  count_adoption <- 0
  end_week <- start_week + n_weeks
  for (week in start_week:end_week) {
    partial_count <- nrow(filter(adopters_of_rec, adopters_of_rec$week_adoption == week))
    count_adoption <- count_adoption + partial_count
    adoption_by_week[week - start_week] <- count_adoption
  }
  return(adoption_by_week)
}


# --> calculation of the mean rate
calculate_mean <- function(single_recipe, n_weeks, start_week, creation_timestamp) {
  rates_of_rec <- recipes_rate[recipes_rate$recipe_id == single_recipe,]
  week_rate_created <- ceiling(difftime(rates_of_rec$timestamp, creation_timestamp, units = "weeks"))
  rates_of_rec <- cbind(rates_of_rec, week_rate_created)
  rate_by_week <- 0
  end_week <- start_week + n_weeks
  n_rate <- 0
  total_sum_rate <- 0
  avg <- 0
  for (week in start_week:end_week) {
    rate_of_week <- rates_of_rec[rates_of_rec$week_rate_created == week,]
    if (nrow(rate_of_week) > 0) {
      n_rate <- n_rate + nrow(rate_of_week)
      total_sum_rate <- total_sum_rate + sum(rate_of_week$rating)
      avg <- total_sum_rate / n_rate
    }
    rate_by_week[week - start_week] <- avg
  }
  return(rate_by_week)
}


# --> calculation of nr. of comments
calculate_comments <- function(single_recipe, n_weeks, start_week, creation_timestamp) {
  comments_of_rec <- recipe_comments[recipe_comments$recipe_id == single_recipe,]
  week_comment_created <- ceiling(difftime(comments_of_rec$created_at, creation_timestamp, units = "weeks"))
  comments_of_rec <- cbind(comments_of_rec, week_comment_created)
  comments_by_week <- 0
  count_comment <- 0
  end_week <- start_week + n_weeks
  for (week in start_week:end_week) {
    partial_count <- nrow(filter(comments_of_rec, comments_of_rec$week_comment_created == week))
    count_comment <- count_comment + partial_count
    comments_by_week[week - start_week] <- count_comment
  }
  return(comments_by_week)
}

# add weeks of life for every recipe
add_life_time_to_table <- function(table_to_add) {
  life_in_weeks <- ceiling(difftime(max(recipes_rate$timestamp), table_to_add$timestamp, units = "weeks"))
  result <- cbind(table_to_add, life_in_weeks)
  return(result)
}
