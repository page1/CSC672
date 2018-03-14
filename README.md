# CSC672
## Brief
This code was used to investigate C. elegans movement for the MS of Predictive Analytics Capstone at DePaul.

* preliminary_analysis.R - Generates several summary statistic plots and places them in the prelim folder
* feature_discovery.R - Runs several trials of investigation, completing KML3D clustering of worm trajectory, and fitting of trajectory sequence with HMM. Many plots are generated summarizing findings in results folder.

## Quick Findings

* C. elegans movement is clusterable into 4 major clusters representing: Ahead, Ahead Slow, Left, and Right. 
* Worms with food tend to make smooth paths, where worms without food tend to make many sharp and erradic turns. 
* The mutant worm in our study that can not be satiated but was provided with food exibited behaviors of both fed and unfed worms.

## Setup
1. Place subfolders of "sample data" in the "data" folder. This folder's content is not checked into github due to size.
