library(data.table)

# assumes data files data/{snow|temperature}_{1,2,3}.csv
variables = c("snow", "temperature")
variable_codes = c("snd_lwe", "tas")
locations = c("snezka", "palava", "komorni_hurka")
data = NULL
for (variable in variables) {
  data_variable = NULL
  for (index in 1:3) {
    data_location = read.csv(paste0("data/", variable, "_", index, ".csv"))
    for (column in c("realization", "lat", "lon")) {
      data_location[[column]] = NULL
    }
    data_location$location = as.factor(locations[index])
    data_location$time = as.Date(data_location$time)
    if (variable == "temperature") {
      data_location$tas = data_location$tas - 273.15
    }
    if (is.null(data_variable)) {
      data_variable = data_location
    } else {
      data_variable = rbind(data_variable, data_location)
    }
  }
  if (is.null(data)) {
    data = data_variable
  } else {
    data = cbind(data, data_variable)
  }
}
data = as.data.table(data)
data = melt(data, id.vars = c("time", "location"), measure.vars = variable_codes)

data_to_plot = data[location == "snezka" & variable == "snd_lwe"]
ggplot(data_to_plot, aes(x = time, y = value)) + geom_line()
