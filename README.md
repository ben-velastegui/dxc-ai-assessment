# DXC AI Team Assesment
This repository contains the code, notebooks, and resources for a proof-of-concept dashboard (https://ben-lopez.shinyapps.io/DXC-AI-Team-Assesment/)  that demonstrates how AI-assisted analytics can unify fragmented IT telemetry—logs, metrics, and traces—into actionable insights for incident diagnosis.

The project delivers an interactive web dashboard (View here) that highlights anomalies, correlations, and system health summaries in a format that is both actionable for engineers and understandable to non-technical stakeholders.

## Executive Summary
Modern enterprises generate vast amounts of telemetry across servers, applications, and networks. Diagnosing incidents that span multiple systems is slow and error-prone, leading to downtime, lost revenue, and wasted engineer hours.

This workflow demonstrates how to:
*  Ingest and unify heterogeneous signals (logs, metrics, traces).
*  Detect unusual system behavior with multiple anomaly detection models.
*   ghlight meaningful correlations across systems.
*   Present insights in a single, interpretable dashboard.

Rather than focusing on raw predictive accuracy, the project emphasizes clarity, explainability, and actionable insights.


## Dashboard
### Explore the dashboard -> https://ben-lopez.shinyapps.io/DXC-AI-Team-Assesment/
The dashboard shows:
* System health summaries (at a glance).
* Timelines of anomalies detected by multiple models.
* Explanations and confidence scores for flagged anomalies.
* Drill-downs into raw records for engineer validation.


## Models and Methods
I experimented with a range of models to understand how different architectures capture system behavior:
* Deep learning models: CNNs, LSTMs, Transformers.
* Tree-based methods: Random Forest, Gradient Boosted Trees.
* Class balancing techniques: SMOTE, weighting.
* Labeling strategies: strict vs. exploratory anomaly labels.

## Key Insights
* No single model captures all signals perfectly.
* Tree-based or attention models provide better explainability, even if deep learning models achieve higher numerical accuracy.
* A multi-pronged approach improves robustness across multi-modal telemetry.


## Data Source: Multi-Source Distributed System Data for AI-powered Analytics
Title: Multi-Source Distributed System Data for AI-Powered Analytics 
Authors: Sasho Nedelkoski, Jasmin Bogatinovski, Ajay Kumar Mandapati, Soeren Becker, Jorge Cardoso, Odej Kao 
Published: October 14, 2019 (Version 1.1) 
License: Creative Commons Attribution 4.0 (CC BY 4.0) 

### What’s in the Dataset
Data modalities:
The dataset includes three kinds of observability / telemetry data from a complex distributed system (OpenStack):
* Application logs
* Metrics
* Distributed traces 

Workload / Fault-Injection Scripts:
It also provides the workload scripts (user requests), fault scripts, and a Rally report which can serve as ground truth for experiments. 

Two variants of workload execution:
  * Sequential workload (“sequential_data”) — user requests executed sequentially.
  * Concurrent workload (“concurrent_data”) — user requests executed in parallel / concurrently. 

Data synchronization and timing:
* The logs and metrics are time-synchronized, and recorded in Central European Summer Time (CEST).
* The traces are in UTC (which is two hours behind CEST). When combining modalities (logs, metrics, traces), care must be taken to align timestamps correctly.
* There is a file „IMPORTANT_experiment_start_end.txt“ that gives the proper time windows for filtering / aligning the logs and metrics with respect to each dataset. 

Size / Structure
The datasets are large (hundreds of MBs per zipped variant):
* concurrent_data.zip (~335.4 MB)
* sequential_data.zip (~315.1 MB)  

How It Maps to My Project
* Because it has multi-modal telemetry (logs, metrics, traces), it supports your goal to experiment with different models and see how each modality contributes to anomaly detection, correlation, etc.
* The ground-truth via workloads and fault scripts + Rally report gives you a concrete basis for evaluating model accuracy, precision/recall, etc.
* The need to synchronize different time zones / timestamp sources is a practical challenge I can demonstrate in my data-layer / preprocessing workflows.
