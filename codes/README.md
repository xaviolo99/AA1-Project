Explanation:
- "1_prep.r": Contains the code which preprocesses and extracted the necessary features from churn.csv.
- "2_mod_LDA_QDA_kNN_bayes.r": Executes the methods LDA, QDA, kNN and Naive Bayes, with Cross Validation and test accuracy.
- "3_mod_GLM.r": Contains the code of the GLMs showcased in the report, plus some more.
- "4_mod_MLP.r": Contains the code of the MLP models presented in the report.
- "5_mod_RF.r": Is a basic script executing the Random Foest models.

Requirements:
- Set the working directory to source file location while executing the scripts.
- Have the required packages installed (class, klaR, e1071, caret, randomForest).
* The code has been proven to work on a 64 bit Windows 10 with R 3.5.1.

Reproduction:
- The first script to be executed has to be "1_prep.r", which will generate "churn.Rdata", which is needed for every other script.
- "1_prep.r", "2_mod_LDA_QDA_kNN_bayes.r", "3_mod_GLM" and "5_mod_RF" are meant to be executed sequentially.
- "3_mod_GLM" reproduction needs the user to execute only the "model" and "cv_model" instruction of the model which wants to be reproduced.