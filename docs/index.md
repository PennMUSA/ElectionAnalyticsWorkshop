---
title: "The PA Election Relational Database"
author: "Jonathan Tannen"
date: "March 20, 2019"
output: html_document
---



# Predicting the State House, Part 1

I'm teaching a workshop at Penn's [Masters of Urban Spatial Analytics](https://www.eventbrite.com/e/penn-musa-election-analytics-workshop-tickets-56058974903) program. I'm presenting my work [predicting the 2018 State House race](https://sixtysixwards.com/home/model-update-is-the-race-really-is-the-race-really-a-tossup-it-depends-on-the-formerly-uncontested-incumbents). It's forced me to organize my code, and I think it's useful enough that I'll be posting the course resources here as well. This tutorial is organized into four parts: the Relational Database, [Processing the GIS Geographies](02_geographies.html), [Prepping the Data for Analysis](03_rectangular_data.html), and [Making the Predictions](04_model.html). In this post, the Relational Database.

*All of the data is available at https://github.com/jtannen/sth_predictions or on the [Penn MUSA GitHub repo](https://github.com/PennMUSA/ElectionAnalyticsWorkshop)*

## The Relational Database

Before we can talk about modelling, I'll outline the data as I currently have it organized. I've built a "Relational Database" [(Chapter 5)](https://web.stanford.edu/~gentzkow/research/CodeAndData.pdf) out of data from the wonderful [Open Elections Project](https://github.com/openelections/openelections-data-pa). 

A relational database consists of separate rectangular datasets. Each dataset has a column or set of columns that provide a unique key. The other data in that row should be only data that applies to that row; you don't want data in multiple rows that represent the same information.

For example, part of my data-base is the data.frame `results`. The key for this data.frame is four columns: 

- `race` (e.g. \"2016 G STH STH-8\", meaning the competition for the 2016 General, State House district 8)
- `candidate` (e.g. \"JUDITH D HINES (STH)\")
- `county_code_.` and `precinct_code`, which identify unique precincts.
These four values provide unique identifiers for the row. `vote_total` is the votes received for that row. The dataframe should contain no data that isn't unique across rows. For example, it doesn't contain the candidate's gender (which should be in another data.frame, `candidates`), or the total turnout for the race (which should be in `races`). 

The magic of this setup is that it prevents duplication of data entry or other possible mistakes, while when done well, you can easily join tables to get whatever combination you need. We could join `results` with `candidates` to get `gender` if we need to for a given application.

(Note: I've already violated the structure above, since `results` contains `party`, which is duplicated across precincts and should really belong to the table `candidates_to_races`, which has unique rows for each candidate and race, ignoring precincts. Rules are made to be broken. \_shrugemoji\_)


```r
library(tidyverse)
load("data/relational_db.rda")
```

The database has six dataframes. `elections` has a single row for each election, meaning each year and each primary or general (separating the primaries by party). The column `election` is the unique identifier:

```r
head(elections)
```

```
## # A tibble: 6 x 4
##   election_year election_type election   cycle        
##   <chr>         <chr>         <chr>      <chr>        
## 1 2016          P-DEM         2016 P-DEM Presidential 
## 2 2016          P-REP         2016 P-REP Presidential 
## 3 2016          P-            2016 P-    Presidential 
## 4 2016          G             2016 G     Presidential 
## 5 2014          P-REP         2014 P-REP Gubernatorial
## 6 2014          P-DEM         2014 P-DEM Gubernatorial
```

`candidates` has a single row for each candidate. I've done manual cleaning to match up candidates across years who use slightly different names (e.g. different middle initials), so in theory these are unique people. This has one big exception: the same candidate running for different offices will show up as two candidates. This felt right to me, it will later mean that success in a State House race doesn't inform success is a Gubernatorial race. The table is actually unique candidate + office combinations. 

In `candidates`, column `names` is a list of all of the names that candidate has used on a ballot. Column `parties` is the list of `parties` the candidate has ever run as, and `party_guess` is my guess at whether a candidate is one of (Democrat, Republican), which I will use when they appear on the ballot as a third party. This is usually easy: a candidate who ran as a Republican four years ago might run as D/R if they're unopposed. If they've never run as a Dem or a Rep, I leave them as the third party on the ballot. The column `gender` is just my guess of the candidate's gender based on their name/googling. (Let me know if you find any issues!).  The column `candidate` is the unique identifier:


```r
head(candidates)
```

```
## # A tibble: 6 x 6
##   office candidate                 names       parties  party_guess gender
##   <chr>  <chr>                     <list>      <list>   <chr>       <fct> 
## 1 GOV    ALLYSON Y SCHWARTZ (GOV)  <chr [3 x ~ <chr [1~ DEM         F     
## 2 GOV    ANTHONY HARDY WILLIAMS (~ <chr [3 x ~ <chr [1~ DEM         M     
## 3 GOV    BOB CASEY (GOV)           <chr [3 x ~ <chr [1~ DEM         M     
## 4 GOV    DAN ONORATO (GOV)         <chr [3 x ~ <chr [2~ DEM         M     
## 5 GOV    ED RENDELL (GOV)          <chr [3 x ~ <chr [4~ DEM         M     
## 6 GOV    JACK WAGNER (GOV)         <chr [3 x ~ <chr [1~ DEM         M
```

Elections are divided into races. Each `race` is the competition for a given office in a given election. State-wide races are given the district `PA`. I populate the column `is_contested` as whether the second-place candidate won at least 5% of the vote; the goal is to identify whether two candidates' names were actually on the ballot, and rule out write-ins. All entries in the `.*_candidate` columns match the unique identifier in `candidates`. The column `race` is the unique identifier here:


```r
head(races)
```

```
## # A tibble: 6 x 15
##   election office district is_contested race_turnout race  winner_party
##   <chr>    <chr>  <chr>    <lgl>               <dbl> <chr> <chr>       
## 1 2002 G   GOV    GOV-PA   TRUE              3580986 2002~ DEM         
## 2 2002 G   STH    STH-1    TRUE                12238 2002~ DEM         
## 3 2002 G   STH    STH-10   FALSE               14724 2002~ REP         
## 4 2002 G   STH    STH-100  TRUE                13605 2002~ REP         
## 5 2002 G   STH    STH-101  TRUE                16806 2002~ REP         
## 6 2002 G   STH    STH-102  TRUE                18222 2002~ REP         
## # ... with 8 more variables: winner_candidate <chr>,
## #   winner_pct_vote <dbl>, is_demrep <lgl>, dem_candidate <chr>,
## #   dem_votes <dbl>, rep_candidate <chr>, rep_votes <dbl>,
## #   pct_dem_2party <dbl>
```

The dataframe `candidates_to_races` maps candidates to races. This includes their total `candidate_race_votes`, the `party` that they were listed on the ballot as for this race as well as `party_replaced`, which imputes Dem/Rep using `party_guess` if they're listed as a third party and there are no other Dems or no other Reps. Note: `is_incumbent` indicates whether the candidate won any seat for this office in the last cycle, regardless of district (which is important since there was redistricting in 2012.) For example, I consider Jason Altmire an incumbent in his 2012 race for USC-12, even though two years earlier he won in USC-4. 


```r
head(candidates_to_races)
```

```
## # A tibble: 6 x 7
##   race  candidate party candidate_race_~ is_incumbent party_guess
##   <chr> <chr>     <chr>            <dbl> <lgl>        <chr>      
## 1 2002~ ED RENDE~ DEM            1911587 NA           DEM        
## 2 2002~ KEN V KR~ LIB              40918 NA           LIB        
## 3 2002~ MICHAEL ~ GRN              38378 NA           GRN        
## 4 2002~ MIKE FIS~ REP            1589030 NA           REP        
## 5 2002~ BILL STE~ REP               3343 NA           REP        
## 6 2002~ LINDA BE~ DEM               8895 NA           DEM        
## # ... with 1 more variable: party_replaced <chr>
```

The table `precincts_to_districts` is a mapping of which precincts voted for which districts in each year. I'll discuss this (and why we need it) in [Processing GIS](02_geographies.html).

Finally, `results` is the good stuff: the precinct-level results.

```r
head(results)
```

```
## # A tibble: 6 x 6
##   race         candidate      county_code_. precinct_code party vote_total
##   <chr>        <chr>          <chr>         <chr>         <chr>      <dbl>
## 1 2016 P-DEM ~ HILLARY CLINT~ 1             10            DEM           37
## 2 2016 P-DEM ~ HILLARY CLINT~ 1             20            DEM           42
## 3 2016 P-DEM ~ HILLARY CLINT~ 1             30            DEM           13
## 4 2016 P-DEM ~ HILLARY CLINT~ 1             40            DEM          105
## 5 2016 P-DEM ~ HILLARY CLINT~ 1             50            DEM           35
## 6 2016 P-DEM ~ HILLARY CLINT~ 1             60            DEM           59
```

That's it! Now for the hard part: [Processing the GIS geographies](02_geographies.html).
