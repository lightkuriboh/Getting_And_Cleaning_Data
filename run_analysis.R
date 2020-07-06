library(dplyr)
### Some utilities I need to use to explore the data
utilities <- function () {
    check_type_table <- function (dat, type_func, check_type) {
        ans <- 0
        for (i in 1:ncol(dat)) {
            ans <- ans + check_type(type_func(dat[, i]))
        }
        ans == ncol(dat)
    }
    
    list(check_type_table = check_type_table)
}
my_util <- utilities()

### Set working directory
working_dir <- 'projects/get_and_clean_data/data/getdata_project_data_UCI_HAR_Dataset'
getwd()
setwd(working_dir)

### File names and path to respective files
x_train_path <- 'train/X_train.txt'
y_train_path <- 'train/y_train.txt'
subject_train_path <- 'train/subject_train.txt'

inertial_body_acc_x_train_path <- 'train/Inertial Signals/body_acc_x_train.txt'
inertial_body_acc_y_train_path <- 'train/Inertial Signals/body_acc_y_train.txt'
inertial_body_acc_z_train_path <- 'train/Inertial Signals/body_acc_z_train.txt'
inertial_body_gyro_x_train_path <- 'train/Inertial Signals/body_gyro_x_train.txt'
inertial_body_gyro_y_train_path <- 'train/Inertial Signals/body_gyro_y_train.txt'
inertial_body_gyro_z_train_path <- 'train/Inertial Signals/body_gyro_z_train.txt'
inertial_total_acc_x_train_path <- 'train/Inertial Signals/total_acc_x_train.txt'
inertial_total_acc_y_train_path <- 'train/Inertial Signals/total_acc_y_train.txt'
inertial_total_acc_z_train_path <- 'train/Inertial Signals/total_acc_z_train.txt'

x_test_path <- 'test/X_test.txt'
y_test_path <- 'test/y_test.txt'
subject_test_path <- 'test/subject_test.txt'

inertial_body_acc_x_test_path <- 'test/Inertial Signals/body_acc_x_test.txt'
inertial_body_acc_y_test_path <- 'test/Inertial Signals/body_acc_y_test.txt'
inertial_body_acc_z_test_path <- 'test/Inertial Signals/body_acc_z_test.txt'
inertial_body_gyro_x_test_path <- 'test/Inertial Signals/body_gyro_x_test.txt'
inertial_body_gyro_y_test_path <- 'test/Inertial Signals/body_gyro_y_test.txt'
inertial_body_gyro_z_test_path <- 'test/Inertial Signals/body_gyro_z_test.txt'
inertial_total_acc_x_test_path <- 'test/Inertial Signals/total_acc_x_test.txt'
inertial_total_acc_y_test_path <- 'test/Inertial Signals/total_acc_y_test.txt'
inertial_total_acc_z_test_path <- 'test/Inertial Signals/total_acc_z_test.txt'

activities_label_path <- 'activity_labels.txt'
feature_path <- 'features.txt'

### Load data from files to memory
x_train <- read.table(x_train_path)
y_train <- read.table(y_train_path)
subject_train <- read.table(subject_train_path)

inertial_body_acc_x_train <- read.table(inertial_body_acc_x_train_path)
inertial_body_acc_y_train <- read.table(inertial_body_acc_y_train_path)
inertial_body_acc_z_train <- read.table(inertial_body_acc_z_train_path)
inertial_body_gyro_x_train <- read.table(inertial_body_gyro_x_train_path)
inertial_body_gyro_y_train <- read.table(inertial_body_gyro_y_train_path)
inertial_body_gyro_z_train <- read.table(inertial_body_gyro_z_train_path)
inertial_total_acc_x_train <- read.table(inertial_total_acc_x_train_path)
inertial_total_acc_y_train <- read.table(inertial_total_acc_y_train_path)
inertial_total_acc_z_train <- read.table(inertial_total_acc_z_train_path)

x_test <- read.table(x_test_path)
y_test <- read.table(y_test_path)
subject_test <- read.table(subject_test_path)

inertial_body_acc_x_test <- read.table(inertial_body_acc_x_test_path)
inertial_body_acc_y_test <- read.table(inertial_body_acc_y_test_path)
inertial_body_acc_z_test <- read.table(inertial_body_acc_z_test_path)
inertial_body_gyro_x_test <- read.table(inertial_body_gyro_x_test_path)
inertial_body_gyro_y_test <- read.table(inertial_body_gyro_y_test_path)
inertial_body_gyro_z_test <- read.table(inertial_body_gyro_z_test_path)
inertial_total_acc_x_test <- read.table(inertial_total_acc_x_test_path)
inertial_total_acc_y_test <- read.table(inertial_total_acc_y_test_path)
inertial_total_acc_z_test <- read.table(inertial_total_acc_z_test_path)

activities_label <- read.table(activities_label_path)
features <- read.table(feature_path)

### Rename label's variable name
activity_name <- 'activity_name'
names(y_train) <- activity_name
names(y_test) <- activity_name

subject_name <- 'subject_name'
names(subject_train) <- subject_name
names(subject_test) <- subject_name

names(activities_label) <- c('V1', activity_name)

### Merge data sets
### Task 1
merge_data_sets <- function () {
    
    merged <- rbind(x_train, x_test)
    
    ### Name the table
    names(merged) <- features[, 2]
    
    merged
}
my_merged_data <- merge_data_sets()

### Check types of merged data file
if (!my_util$check_type_table(my_merged_data, is.na, any)) {
    print('ok')
} else {
    print('have some na(s)')
}
if (my_util$check_type_table(my_merged_data, is.numeric, all)) {
    print('ok')
} else {
    print('not numeric')
}

### Get mean and standard deviation for measurements
### (all except the label column)
### Task 2
merged_table_names <- names(my_merged_data)
my_merged_data <- dplyr::select(
                                my_merged_data,
                                merged_table_names[
                                    grepl('mean', merged_table_names) | 
                                    grepl('std', merged_table_names)    
                                ]
                            )
head(my_merged_data)
sum(grepl('mean', names(my_merged_data)) | grepl('std', names(my_merged_data)))
ncol(my_merged_data)

### Merge activities' name and subjects' name
merge_activity_and_subject <- function () {
    
    merged_y <- rbind(y_train, y_test)
    merged_subject <- rbind(subject_train, subject_test)
    
    my_merged_data %>%
        cbind(merged_y) %>%
        cbind(merged_subject)
}
my_merged_data <- merge_activity_and_subject()
ncol(my_merged_data)
head(my_merged_data)

### Replace labels by descriptive activities
### Task 3
my_merged_data$activity_name <- sapply(
    my_merged_data$activity_name,
    function (val) {
        for (i in 1:nrow(activities_label)) {
            if (activities_label[i, 1] == val) {
                return (activities_label[i, 2])
            }
        }
        return('NA')
    }
)
head(my_merged_data)


### Labels the data set with descriptive variable names
### Task 4
library(stringr)

##### Replace shorts by full words, replace in decreasing order of keys' length (1)
replace_list <- list(
    'meanFreq' = 'Mean_Frequency-',
    'Gravity' = 'Gravity-',
    'mean' = 'Mean_Value-',
    'Body' = 'Body-',
    'Gyro' = 'Gyroscope-',
    'Jerk' = 'Jerk-',
    'std' = 'Standard_Deviation_Value-',
    'Acc' = 'Accelation-',
    'Mag' = 'Magnitude-',
    'X' = 'by_X_axis',
    'Y' = 'by_Y_axis',
    'Z' = 'by_Z_axis'
)

##### Replace prefixes by descritive words
prefix_replace <- list(
    't' = 'time_series_of-',
    'f' = 'Fast_Fourier_Transform_of-'
)

#### Characters that need to be removed before processing
special_characters <- c('\\(\\)', '-')

##### (1) Step
names(my_merged_data) <- sapply(names(my_merged_data), function (value) {
    ####### Remove special characters
    for (i in 1:length(special_characters)) {
        value <- str_replace_all(value, special_characters[i], '')    
    }
    
    replace_names <- names(replace_list)
    for (i in 1:length(replace_list)) {
        value <- str_replace_all(value, replace_names[i], replace_list[[i]])
    }
    
    value
})

##### Just remove traling '-' characters
names(my_merged_data) <- sapply(names(my_merged_data), function (value) {
    len <- nchar(value)
    if (substring(value, len, len) == '-') {
        value <- substring(value, 1, len - 1)
    }
    
    value
})

##### Replace prefixes
names(my_merged_data) <- sapply(names(my_merged_data), function (value) {
    if (substring(value, 1, 1) %in% names(prefix_replace)) {
        value <- paste(
                        prefix_replace[substring(value, 1, 1)],
                        substring(value, 2),
                        sep = ''
                )
    }
    
    value
})

names(my_merged_data)
dim(my_merged_data)

### Create new data set as required:
### Independent tidy data set with the average of each variable for
### each activity and each subject.
### Task 5
new_data_set <- my_merged_data %>%
                    ##### Group by activity name and subject name
                    dplyr::group_by(activity_name, subject_name) %>%
                    ##### Get mean value of all variables for each activity and subject
                    dplyr::summarise_all(mean)
class(new_data_set)
head(new_data_set)
names(new_data_set)
dim(new_data_set)

### Write the recently created data set to a file
write.table(new_data_set, '../../tiny_tidy_data/finalDataSet.txt', row.names = F)
getwd()
