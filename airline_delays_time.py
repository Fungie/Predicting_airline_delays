# importin
import pandas as pd
import matplotlib.pyplot as plt
plt.style.use('ggplot')

# reading data in
data = pd.read_csv("/Users/aurenferguson/Documents/Predicting_airline_delays/data/DelayedFlights.csv")

# What are the worst times of the year for delays?
# getting date
data["Day"] = data["DayofMonth"]
data['date'] = pd.to_datetime(data[["Year", "Month", "Day"]])

# setting date to index
data.index = data.date

# plotting mean delay on a daily, weekly and monthly basis
data_month = data.resample('M').mean()
data_week = data.resample('W').mean()
data_day = data.resample('D').mean()

# plotting the mean delay
data_month.ArrDelay.plot()
data_week.ArrDelay.plot()
data_day.ArrDelay.plot()

# getting a moving average on monthly basis to account for seasonality
data_week['ArrDelay_month_moving_avg'] = pd.rolling_mean(data_week.ArrDelay,5)
data_week['ArrDelay_norm'] = data_week.ArrDelay - data_week.ArrDelay_month_moving_avg

# plotting to find extreme events
data_week.ArrDelay_norm.plot()

# looks like the holidy periods have spikes in delays, summer, thanksgiving, christmas
holidays = data_week['2008-11' : '2008-12']


# plotting holiday period
fig, ax = plt.subplots(figsize = (15,10))
ax.plot_date(holidays.index, holidays.ArrDelay_norm, linestyle='-', markersize = 0)

ax.annotate('Thanksgiving',
             (holidays.index[4], holidays.ArrDelay_norm[4]),xytext=(-100, 50), 
             textcoords='offset points',
             arrowprops=dict( facecolor = 'black', headlength = 10),
             horizontalalignment='centre')

ax.annotate('Christmas',
             (holidays.index[7], holidays.ArrDelay_norm[7]),
             xytext=(-100, -5), 
             textcoords='offset points',
             arrowprops=dict( facecolor = 'black', headlength = 10),
             horizontalalignment='centre')
plt.ylabel('Delay time relative to monthly average')
plt.xlabel('Date')
plt.savefig("/Users/aurenferguson/Documents/Predicting_airline_delays/images/dates_zoomed.png", orientation = 'landscape')
plt.show()



# saving full year data
fig, ax = plt.subplots(figsize = (15,10))
ax.plot_date(data_week.index, data_week.ArrDelay_norm, linestyle='-', markersize = 0)
plt.ylabel('Delay time relative to monthly average')
plt.xlabel('Date')
plt.savefig("/Users/aurenferguson/Documents/Predicting_airline_delays/images/dates.png", orientation = 'landscape')
plt.show()


