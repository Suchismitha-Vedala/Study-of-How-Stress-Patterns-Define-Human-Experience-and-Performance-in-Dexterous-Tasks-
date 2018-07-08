Study-of-How-Stress-Patterns-Define-Human-Experience-and-Performance-in-Dexterous-Tasks

Given the Microsurgery Performance Data,We are interested in finding any new observations
that can effect the performance. Statistical Tests and Analysis help in exploring the unexplored
data andmaking assumptions and conclusions that can lead to better performance. The analysis
can always lead to performing better experiments in the future.

Data Organization :


The data was structured into main 3main files.
1. MicroSurgeryPerformance file :
This file indicates the Age, Sex, Year, Time Taken for Cutting and Suturing in each session, Scores
of Cutting and Suturing of both the Scorers in Each Session, Number of Sutures made in each
session for each subject.
2. MasterFileMethodistSurgery:
This file indicates the presence and absence of the files of Baseline Perinasal Perspiration(PP),
Cutting PP, Suturing PP, Cutting NASA,Suturing NASA, Mbg, and MPOST for each subject and
each session.
3. TaiScore.txt:
This file indicates the tai scores of all subjects.
Along with this, there is a folder for each subject.Each Subject folder consists of :
1. Subject_tai.csv : which is a behavior analysis questionnaire.
2. Mbg and Mpost csv files which contain geographic and personal information of the subject
before and after the tasks.
3. tp.csv, which gives pre and post Session values.
4.A folder. for each session.
Each Session Folder consists of five files:
1.Baseline.csv: Consists of the values of the Perspiration when the subjects were relaxed. This
data is collected over every frame in every second for 5 minutes
2.Cutting.csv: Consists of the values of the Perspiration when the subjects were performing
cutting task. This data is collected over every frame during the time taken to complete the task
3.Cutting_NASA.csv: Consists of Responses to the NASA TLX questionnaire.
4.Suturing.csv: Consists of the values of the Perspiration when the subjects were performing
Suturing task. This data is collected over every frame during the time taken to complete the task
5.Suturing_NASA.csv: Consist of Responses to the NASA TLX questionnaire


Data Cleaning:

To perform any analysis , we first need to clean the data to removed any redundant data and
reshape the data to help us performany statistical tests.
We performour data cleaning on theMicroSurgeryPerformance.csv file.
1.For each row, where Sex value is 1 we rename toMale and 2 to Female.
2.We melt the data frame to obtain the Score values pertaining to each subject and each session.
3.We mention the Task to which the Scores belong.
4.With two Scorers being present, we extend the rows of the data to append Score values of the
Scorer2.
5. For each subject, for each session and task, we take the Mean_Perspiration(mean persinal
perspiration)value asMean_(task)Perspiration-Mean(Baseline)Perspiration.
This is done because, to get the stress signal caused only by the task can be obtained by
stress signal obtained during task - stress signal during relaxed.
6.We nownormalize theMean_Perspiration values, by taking the absoluteminimumvalue(excluding
NA) and adding to each value along with an error coefficient. We add this as Normalised_PP
7.The time taken for each task per session is totally converted into seconds .
