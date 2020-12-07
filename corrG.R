getPearsonCor <- function(A, B){
  correlation = cor(A, B,
                    use = "complete.obs",
                    method = "pearson")
  return(correlation)
}

data <- corrRawData

cor11 = getPearsonCor(data$Global_active_power,
                      data$Global_active_power)
cor12 = getPearsonCor(data$Global_active_power,
                      data$Global_reactive_power)
cor13 = getPearsonCor(data$Global_active_power,
                      data$Voltage)
cor14 = getPearsonCor(data$Global_active_power,
                      data$Global_intensity)
cor15 = getPearsonCor(data$Global_active_power,
                      data$Sub_metering_1)
cor16 = getPearsonCor(data$Global_active_power,
                      data$Sub_metering_2)
cor17 = getPearsonCor(data$Global_active_power,
                      data$Sub_metering_3)


cor21 = getPearsonCor(data$Global_reactive_power,
                      data$Global_active_power)
cor22 = getPearsonCor(data$Global_reactive_power,
                      data$Global_reactive_power)
cor23 = getPearsonCor(data$Global_reactive_power,
                      data$Voltage)
cor24 = getPearsonCor(data$Global_reactive_power,
                      data$Global_intensity)
cor25 = getPearsonCor(data$Global_reactive_power,
                      data$Sub_metering_1)
cor26 = getPearsonCor(data$Global_reactive_power,
                      data$Sub_metering_2)
cor27 = getPearsonCor(data$Global_reactive_power,
                      data$Sub_metering_3)

cor31 = getPearsonCor(data$Global_intensity,
                      data$Global_active_power)
cor32 = getPearsonCor(data$Global_intensity,
                      data$Global_reactive_power)
cor34 = getPearsonCor(data$Global_intensity,
                      data$Voltage)
cor33 = getPearsonCor(data$Global_intensity,
                      data$Global_intensity)
cor35 = getPearsonCor(data$Global_intensity,
                      data$Sub_metering_1)
cor36 = getPearsonCor(data$Global_intensity,
                      data$Sub_metering_2)
cor37 = getPearsonCor(data$Global_intensity,
                      data$Sub_metering_3)
# similarly do for all , WIP
cor41 = getPearsonCor(data$Voltage,
                      data$Global_active_power)
cor42 = getPearsonCor(data$Voltage,
                      data$Global_reactive_power)
cor44 = getPearsonCor(data$Voltage,
                      data$Voltage)
cor43 = getPearsonCor(data$Voltage,
                      data$Global_intensity)
cor45 = getPearsonCor(data$Voltage,
                      data$Sub_metering_1)
cor46 = getPearsonCor(data$Voltage,
                      data$Sub_metering_2)
cor47 = getPearsonCor(data$Voltage,
                      data$Sub_metering_3)

cor51 = getPearsonCor(data$Sub_metering_1,
                      data$Global_active_power)
cor52 = getPearsonCor(data$Sub_metering_1,
                      data$Global_reactive_power)
cor54 = getPearsonCor(data$Sub_metering_1,
                      data$Voltage)
cor53 = getPearsonCor(data$Sub_metering_1,
                      data$Global_intensity)
cor55 = getPearsonCor(data$Sub_metering_1,
                      data$Sub_metering_1)
cor56 = getPearsonCor(data$Sub_metering_1,
                      data$Sub_metering_2)
cor57 = getPearsonCor(data$Sub_metering_1,
                      data$Sub_metering_3)

cor61 = getPearsonCor(data$Sub_metering_2,
                      data$Global_active_power)
cor62 = getPearsonCor(data$Sub_metering_2,
                      data$Global_reactive_power)
cor64 = getPearsonCor(data$Sub_metering_2,
                      data$Voltage)
cor63 = getPearsonCor(data$Sub_metering_2,
                      data$Global_intensity)
cor65 = getPearsonCor(data$Sub_metering_2,
                      data$Sub_metering_1)
cor66 = getPearsonCor(data$Sub_metering_2,
                      data$Sub_metering_2)
cor67 = getPearsonCor(data$Sub_metering_2,
                      data$Sub_metering_3)

cor71 = getPearsonCor(data$Sub_metering_3,
                      data$Global_active_power)
cor72 = getPearsonCor(data$Sub_metering_3,
                      data$Global_reactive_power)
cor74 = getPearsonCor(data$Sub_metering_3,
                      data$Voltage)
cor73 = getPearsonCor(data$Sub_metering_3,
                      data$Global_intensity)
cor75 = getPearsonCor(data$Sub_metering_3,
                      data$Sub_metering_1)
cor76 = getPearsonCor(data$Sub_metering_3,
                      data$Sub_metering_2)
cor77 = getPearsonCor(data$Sub_metering_3,
                      data$Sub_metering_3)