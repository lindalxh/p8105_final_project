Project Proposal
================
Xinwei Han (xh2601), Xinhui Lin (xl3054), Ruohan Lyu (rl3610), Xinyin
Miao (xm2356), Fenglin Xie (fx2212)
2025-11-07

**Project Title:**

Tiny Paws, Big Rescue: A Data Story of Seasonal and Spatial Animal
Saving

**The motivation for this project**

Animals play an essential role in maintaining ecological balance, yet
those living in NYC face unique challenges. By analyzing this dataset,
we aim to identify when and where animals need the most help, ultimately
raising public awareness and supporting timely rescue and conservation
efforts.

**The intended final products**

The goal of our project is to develop an interactive, data-driven web
that allows visitors to explore the animal rescue data through various
visualizations. Interactive maps will be the centerpiece of our web,
visually representing the distribution of animals encountered by Park
Rangers. Animal distribution map will display the geographical locations
of animal aid incidents, allowing users to filter by animal species and
seasons to identify “hot spots” for specific types of animals or
high-risk areas.

**The anticipated data sources**

This dataset is provided by the Department of Parks and Recreation (DPR)
and is made publicly available through the NYC OpenData portal.

**The planned analyses / visualizations / coding challenges**

We plan to analyze the association between animal conditions and time,
both across the year and throughout the day. A heatmap will be used to
visualize the density of injured animals by month and time of day.
Another analysis will focus on factors that may influence response time,
including park location, animal condition, animal type or number, and
temporal factors such as time of day or frequency within a week or
month.

These relationships can be visualized in several ways. For example,
violin plots grouped by boroughs can illustrate the distribution of
response times across different regions. We also plan to conduct
statistical tests, such as ANOVA, to examine whether the differences in
response time between boroughs are statistically significant.

One key challenge is that the dataset does not include the latitude and
longitude of parks, making it difficult to generate accurate map
visualizations. To address this, we will need to match each park with
its corresponding geographic coordinates. However, detailed location
information recorded in the dataset (e.g., “West 97th St entrance” or
“Garbage can”) lacks standardization, which may limit the consistency
and interpretability of location-based analyses. We plan to use natural
language processing (NLP) strategies to identify and understand patterns
in the location information.

**Project Timeline**

October 31- November 7: brainstorm project ideas, confirm dataset,
finalize analysis plans

November 10-15: clean and integrate data (with geocoded location),
revise hypotheses and methods based on feedback, begin preliminary
analysis

November 16-23: conduct exploratory analysis and generate visualizations

November 24-30: draft written report and begin building project website

December 1-6: finalize report, webpage, screencast, and peer assessment
for submission
