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
