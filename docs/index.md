---
title: climodr
layout: splash
date: '2024-11-09 13:00:00 +0100'
header:
  overlay_color: "#000"
  overlay_filter: 0.6
  overlay_image: "/assets/images/titleimage/titleimage.png"
  caption: 'Image: Environmental Informatics Marburg'
  cta_label: Install now
  cta_url: "/units/getting_started.html"
excerpt: Create good quality climate models in an easy to use workflow. Newbie or Expert in climate modelling? This package suits all skill levels. 
feature_row_intro:
- excerpt: climodr is free and open source under a GPL 3.0 license. It's brought to you by the Department of [Physical Geography](https://www.uni-marburg.de/en/fb19/disciplines/physisch){:target="_blank"} at [Marburg University](https://www.uni-marburg.de/en){:target="_blank"}
feature_row_ilos:
- image_path: "/assets/images/envobs_ilos.jpg"
  alt: PC monitor laying in the garden of the institute.
  title: Pacakge deliverables
  excerpt: "Template..."
---

 
{% include feature_row id="feature_row_intro" type="center" %}

Welcome to climate modeler in R, short climodr. 
This package uses point data from climate stations, spectral imagery and 
elevation models to automatically create ready-to-use climate maps. 

First of all, the idea of climodr is to deliver an easy to use method for 
creating high quality climate maps. Like the one we create in this vignette:


# Package deliverables

This Package delivers

* a tight and easy to use workflow to create resilient climate models and maps
* wrapper functions that make your life in R easier
* a fast alternative to months of coding and bugfixing


# Package Features

Climodr is mainly split into four steps:

* *Environment* Management for Easy Use, Reproducability and Shareability
* *Pre-Processing* of climate station data as well as spatial raster data, generation of spectral indices
* *Processing* of Pre-Processed data into climate models and predictions 
* *Plotting* of climate maps


# Syllabus

| Session |  Date | Topic                        | Content                                                                          |
|---------|-------|------------------------------|----------------------------------------------------------------------------------|
||| **SDM Basics** ||
| 01 | DD.MM.YYYY | The very basics     | Course introduction, expectations, organisational matters, R, R Studio, R Markdown, GitHub     |
| 02 | DD.MM.YYYY | SDM Basics                   | What is SDM, how does it works and why do we need it?                  |
| 03 | DD.MM.YYYY | SDM methods     | Common methods, software and R packages used for SDM |
| 04 | DD.MM.YYYY | Exemplary SDM workflow                           |  |
| 05 | DD.MM.YYYY |   ??                |                           |
||| **Virtual species**                   |        |
| 06 | DD.MM.YYYY | Artificial landscapes | Create neutral landscape models           |
| 06 | DD.MM.YYYY | Virtual species | Generate virtual species with different ecological niches          |
| 07 | DD.MM.YYYY | ...                 | Model fitting, assessment, and predictions                                 |
||| **From idea to insight: Your personal research project**           ||
| 08 | DD.MM.YYYY | Research gaps in SDM         |                                         |
| 09 | DD.MM.YYYY | Create your own experimental setup   | What do you want to research and why? How to set up an experimental design to test your hypothesis|
| 10 | DD.MM.YYYY | Your SDM project             | You can use the time of this session to work on your project and ask questions    |
| 11 | DD.MM.YYYY | Your SDM project II          | You can use the time of this session to work on your project and ask questions    |
| 12 | DD.MM.YYYY | **Present your project outline**  |                         |
| 13 | DD.MM.YYYY | Your SDM project III          |  You can use the time of this session to work on your project and ask questions   |
||| **Synthesis**                                ||
| 14 | DD.MM.YYYY | Wrap up                      | Get and give feedback from your peers and instructors, tell us how you self-assess your skills, and happy holidays |





# Preparation and prerequisites

A basic understanding of R and of handling spatial data is beneficial. Initial experience with climate modeling is helpful. 
All software needed to run climodr is free and open source.

If you have absolutely no experience with R we highly recommend our base R course, 
which can be found [here](https://geomoer.github.io/moer-base-r/){:target="_blank"}.
{: .notice--success}


# Team

{% for author in site.data.authors %} {% include author-profile.html %}
{% endfor %}


<!--
[Go to course units]({{ site.baseurl }}{% link _pages/units.md %}){: .btn .btn--success .btn--large .align-center}
-->


