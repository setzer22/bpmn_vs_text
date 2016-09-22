# BPMN vs Natural Language Comparison Tool

This is the repository for the source code of **BPMNvsText**. My final degree's thesis project. This algorithm combines Natural Language Processing with Process mining techniques in order to compare BPMN business process description with a (potentially) equivalent natural language representation. It can be used in order to detect inconsistencies between both representations or to compute a similarity score of several versions of the same document to the reference model, amongst other uses.

# Folder structure

`src/main/clojure/` contains the main implementation code for the project, in Clojure. The folder `src/main/java/` contains the Java wrapper classes in order to integrate the project in a JSF web interface easily.

Folder `benchmarks` contains all the BPMN and text examples used in the experiments. `logs` contains the execution logs for various runs of the algorithm in its latest version.

## Compiling

Folder `Doc` contains the *LaTeX* thesis document for the project. In order to compile it run the following commands (*LaTeX* must be installed):
```bash
$ pdflatex main
$ biber main
$ pdflatex main # Must be run twice
```
A main.pdf file will be generated.

To compile the project run the following commands (maven must be installed):

```bash
$ mvn compile
$ mvn clojure:compile
```







