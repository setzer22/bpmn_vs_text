package edu.upc.modelvsdocument;

import java.util.List;

public class Match {
    
    int sentenceIndex;
    String taskId;
    String taskName;
    List<String> commonFeatures;
    double score;
    boolean isGood;

    public Match (int sentenceIndex, String taskId, String taskName, List<String> commonFeatures, double score, boolean isGood) {
        this.sentenceIndex = sentenceIndex;
        this.taskId = taskId;
        this.commonFeatures = commonFeatures;
        this.isGood = isGood;
        this.score = score;
        this.taskName = taskName;
    }

    public int getSentenceIndex() {return sentenceIndex;}
    public String getTaskId() {return taskId;}
    public String getTaskName() {return taskName;}
    public List<String> getCommonFeatures() {return commonFeatures;}
    public double getScore() {return score;}
    public boolean isGood() {return isGood;}
}
