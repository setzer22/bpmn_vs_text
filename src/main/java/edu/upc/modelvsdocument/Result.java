package edu.upc.modelvsdocument;

import java.util.List;

public class Result {
    
    private double total_score;
    private List<Match> matches;
    private List<String> sentences;
    private List<SentenceIdsPair> tasksOfSentence;
    private String log;

    public Result (double total_score, String log,  List<Match> matches, List<SentenceIdsPair> tasksOfSentence, List<String> sentences) {
        this.total_score = total_score;
        this.matches = matches;
        this.sentences = sentences;
        this.tasksOfSentence = tasksOfSentence;
        this.log = log;
    }

    // Returns the match information for a given sentence and task pair.
    public Match getMatch(int sentenceIndex, String taskId) {
        for (Match m : matches) {
            if(m.getTaskId() == taskId && m.getSentenceIndex() == sentenceIndex) {
                return m;
            }
        }
        return null;
    }

    public double getTotalScore() {return total_score;}
    public List<String> getSentences() {return sentences;}
    public List<Match> getMatches() {return matches;}
    public List<SentenceIdsPair> getTasksOfSentence() {return tasksOfSentence;}
    public String getLog() {return log;}
    
}
