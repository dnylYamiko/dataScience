The repo includes the following files and directories:
=======================================================
- ReadMe.md
- "data": This is the directory that contains the "UCI HAR Dataset" dataset(files listed below)
- "run_analysis.R": The .R script that is reading relevant .txt files from "./data/UCI HAR Dataset". There is documentation included to explain the different commands used.
- "SecondaryTidyData.txt": This is the tidy dataset created by the .R script.
- "CodeBook.md": This contain the markup script for the codebook. The code book describes the variables, the data, and any transformations or work that you performed to clean up the data
- "CodeBook.html": This an html document for the codebook, this was done using the knit2html()


The dataset(./data/UCI HAR Dataset/) includes the following files:
==================================================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 
