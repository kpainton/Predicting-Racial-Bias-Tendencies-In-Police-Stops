df <- read.csv("complete_clean_data.csv")
colnames(df)
head(df)

# Completes chi-squared independence tests for DUI violations
DUI <- table(df$driver_race, df$DUIviolation)
DUItest <- chisq.test(DUI)
# Prints expected vs observed values for chi-squared DUI Test
DUItest$observed
DUItest$expected

# Completes chi-squared independence tests for Speeding violations
Speed <- table(df$driver_race, df$SpeedingViolation)
Speedtest <- chisq.test(Speed)
# Prints expected vs observed values for chi-squared Speeding Test
Speedtest$observed
Speedtest$expected

# Completes chi-squared independence tests for Equipment violations
Equipment <- table(df$driver_race, df$EquipmentViolation)
Equipmenttest <- chisq.test(Equipment)
# Prints contingency table for chi-squared Equipments Test
Equipmenttest$observed
Equipmenttest$expected

# Completes chi-squared independence tests for License violations
License <- table(df$driver_race, df$LicenseViolation)
Licensetest <- chisq.test(License)
# Prints expected vs observed values for chi-squared Equipments Test
Licensetest$observed
Licensetest$expected

# Completes chi-squared independence tests for Lights violations
Lights <- table(df$driver_race, df$LightsViolation)
Lightstest <- chisq.test(Lights)
# Prints contingency table for chi-squared Lights Test
Lightstest$observed
Lightstest$expected

# Completes chi-squared independence tests for Paperwork violations
Paperwork <- table(df$driver_race, df$PaperworkViolation)
Paperworktest <- chisq.test(Paperwork)
# Prints contingency table for chi-squared Paperwork Test
Paperworktest$observed
Paperworktest$expected

# Completes chi-squared independence tests for Safe Movement violations
SafeMovement <- table(df$driver_race, df$SafeMovementViolation)
SafeMovementtest <- chisq.test(SafeMovement)
# Prints contingency table for chi-squared Safe Movement Test
SafeMovementtest$observed
SafeMovementtest$expected

# Completes chi-squared independence tests for Stopping violations
Stop <- table(df$driver_race, df$StoppingViolation)
Stoptest <- chisq.test(Stop)
# Prints contingency table for chi-squared Stopping Test
Stoptest$observed
Stoptest$expected

# Completes chi-squared independence tests for Registration violations
Registration <- table(df$driver_race, df$RegistrationViolation)
Registrationtest <- chisq.test(Registration)
# Prints contingency table for chi-squared Registration Test
Registrationtest$observed
Registrationtest$expected

# Completes chi-squared independence tests for Seat Belt violations
SeatBelt <- table(df$driver_race, df$SeatBeltViolation)
SeatBelttest <- chisq.test(SeatBelt)
# Prints contingency table for chi-squared Seat Belt Test
SeatBelttest$observed
SeatBelttest$expected

# Completes chi-squared independence tests for Moving violations
Moving <- table(df$driver_race, df$MovingViolation)
Movingtest <- chisq.test(Moving)
# Prints contingency table for chi-squared Moving Test
Movingtest$observed
Movingtest$expected

# Completes chi-squared independence tests for Cell Phone violations
CellPhone <- table(df$driver_race, df$CellPhoneViolation)
CellPhonetest <- chisq.test(CellPhone)
# Prints contingency table for chi-squared Cell Phone Test
CellPhonetest$observed
CellPhonetest$expected

# Completes chi-squared independence tests for Truck violations
Truck <- table(df$driver_race, df$TruckViolation)
Trucktest <- chisq.test(Truck)
# Prints contingency table for chi-squared Truck Test
Trucktest$observed
Trucktest$expected

# Completes chi-squared independence tests for Other violations
Other <- table(df$driver_race, df$OtherViolation)
Othertest <- chisq.test(Other)
# Prints contingency table for chi-squared Other Test
Othertest$observed
Othertest$expected
Othertest$residuals

# Completes chi-squared independence tests for Arrest or Citations
Arrest <- table(df$driver_race, df$ArrestOrCitation)
Arresttest <- chisq.test(Arrest)
# Prints contingency table for chi-squared Arrest of Citations Test
Arresttest$observed
Arresttest$expected