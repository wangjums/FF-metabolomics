# FF-metabolomics
The first code with the name "1_Feature selection with RFfuncs" is used to choose the best feature number based on RFfuncs from Caret.The .csv data is needed here with the samples
in the first colume, label in the second colume and features in the following columes. You can set the range feature number in the code and change other parameters in bulding the 
model. The code might help you to chooose the best feature number according to the AUC of different models. After that, you could set the specific feature number and then choose 
the suitable feature pattern based on different criteria such as the frequency of being selected.

The second code with the name "2_Build model and ROC analysis" is also based on Caret to build random forest model and evaluate that with ROC. The ROC analysis of the test set is
appled here and you could also change the times of running according to your experience or reqiurement.In addition, the paameters such as sample spliting and hyperparameters could
also be changed. And you could also acqiure the plot of feature importance in the code.

Good luck.
