### psycholinguistics discovery sims
###### [timothy leffel](http://lefft.xyz), july24/2017
<hr>

this is a project investigating consequences of various design and analysis decisions in RT-based psycholinguistics experiments. main tool is simulation. after code infrastructure is in place, will use the literature to derive realistic effect sizes, means/sd's, etc. publicly available RT datasets also high on the wishlist. 


inspiration comes in part from this very nice/concise/thought-provoking [note by Emmanuel Chemla](http://www.emmanuel.chemla.free.fr/thresholds/OutliersCriterion.html). 
<br><br>


##### contents of repo
<hr>

- `psycholing_discovery_sims.{rmd, html}` -- main document 
- `functions.r` -- setting things up for simulating experiments + quickly displaying results (sourced at top of main doc)
- `chemla_code.r` -- code for original chemla note (with edits/rewrite)
- `boosh.css` -- style for main doc
- `out/` -- plots, simulated datasets, etc. 


<br>

##### TODO list
<hr>

###### items that need attention


next: impute w mean; impute w mean +/- sd of non-missing

	> TODO: write func for sampling from real data + simming that
	> TODO: parameterize `noeffect_sim` for easier exploration
	> TODO: fix axis ranges + ticks for comparison across sims
	> TODO: see what the diff is for false *negatives* -- 
					if also bad, then seems like excluding is never good...
					or even conservative?! 
	> TODO: another thing to do wd be to cut off anything 
					e.g. 2sd's above the mean; or the top + bottom 5 pctiles; ...
	> TODO: add "impute w mean" as an option
	> TODO: abstract over global params + make shiny app for easier comparison
	> TODO: allow option for maintaining attributes of derived columns
	> TODO: make outlier subj id's available globally (related to next two items...)
	> TODO: GLOBAL ARG PASSING!!!
	> TODO: need to make outlier_subj + other params globally available...
	> TODO: implement outlier *items*, not just subj's
	> TODO: add subj param(?)
	> TODO: clean up passing of specify mean_rt and sd_rt!!!
	> TODO: option to work with whole numbers(?)
	> TODO: decide if we shd have a func for analyzing or just do it interactively..
	> TODO: finish summary function
	> TODO: implement rt-type labeling in plot fonce
	> TODO: get rid of grid info in rmd output 
	        "don't print(), grid.arrange() draws by itself. 
	        If you want to store the result and then draw it, 
	        use arrangeGrob()+grid.draw()"


###### completed items

	-x- > TODO: add param for number of outlier subj's
	-x- > TODO: finish plotting function
	-x- > TODO: write func to extract relevant bits from model summaries

<br>

##### misc notes to self
<hr>

> nice [tool](http://meshugga.ugent.be/snaut-english/) for computing similarity between words, finding neighbors, etc. 
> also [cool](http://crr.ugent.be/programs-data/wuggy), pseudoword generator
> ...

<br>


##### estimates of mean/sd/effect size in rt priming studies
<hr>

- location of [blp datasets](/Users/timothyleffel/Google Drive/work UoC/teaching/AU2016 psycholinguistics/lab-materials/datasets/BLP/)

- holcomb-neville 1990 lcp, format: mean (sd); pct error (sd) 
	- words (visual)
		- related: 653 (92); .5 (1)
		- unrelated: 686 (79); 1.6 (1.8)
	- pseudowords (visual): 808 (103); 2.8 (2.8)
	- nonwords (visual): 630 (74); .7 (1.3)
	- words (auditory)
		- related: 718 (89); .4 (1)
		- unrelated: 827 (87); 1.8 (1.6)
	- pseudowords (auditory): 932 (110); 3.8 (3)
	- nonwords (auditory): 716 (85); .4 (.8)

- NEXT UP



<br>

##### notable quotes/citations/etc.
<hr>


Keuleers et al. 2011, p290:

> Before computing mean RTs and zRTs, 2.3% of outliers were removed. Outliers were defined per participant and block, using a method commonly applied for box plots: First the inter- quartile distance (the distance between quartile 1 and quartile 3) was computed; RTs were then defined as outliers when they were higher than 3 interquartile distances above quartile 3 or lower than 3 interquartile distances below quartile 1. Since there were no time limits for responses in our study, this method, which is robust to the influence of extreme outliers, is particulary suitable. Of course, other researchers are invited to use their own choice of trimming method on the raw data. 

Lucas 2000, p621: **Central Tendency and Variability**

> For the 116 observations, the average d for semantic priming was .26, which is a little more than half the ef- fect size found for associative priming (73 observations), which was .51. The average d by study was .30 for se- mantic priming (26 studies) and.49 for associative prim- ing (20 studies). The total number of subjects across all 116 observations was 3,882. Because observations some- times involved repeated measures, some of these sub- jects accounted for more than one observation. Cor- rected for repeated measures, the total number of subjects across all the 26 studies was 2,243. Because the range of subjects was so varied (from 7 to 359 subjects in a single study), the weighted mean for each observa- tion was also calculated, using the formula in Hedges and Olkin (1985). The weighted average d for semantic priming, by observation, was .25, and for associative priming, it was .49. By study, the weighted average ds were .29 for semantic priming and .47 for associative priming. In subsequent analyses, only the weighted mean effect sizes will be reported. NEWPAR Fifty percent ofthe observations for semantic priming that were in the midrange had effect sizes between .05 and .36 (25th and 75th percentile values); this range was .17 to .59 for associative priming. For the studies, the range was somewhat narrower-between .09 and .36 for semantic priming and between .18 and .54 for associa- tive priming. NEWPAR These results indicate that the effect ofsemantic prim- ing, while small, is nonetheless clearly present in the studies reviewed. The small effect size for semantic priming suggests one reason for the failures of some studies to yield statistically significant semantic priming effects. A power analysis indicates that one would need more than twice as many subjects (N = 188) to find sta- tistically significant semantic priming as one would need to find statistically significant associative priming (N = 72) with power of .80 and an alpha level of .05.


Lucas 2000, **Response Speed**

> Observations and studies were partitioned into fast (500 msec or less) and slow (greater than 500 msec) av- erage reaction times (the cutoffs were based on sugges- tions by Williams, 1996). Fast response times should be associated with automatic priming; slow response times, with strategic priming. Because in the Stroop task slower rather than faster reaction times signal automatic prim- ing, the Stroop study o f Shelton (1993) was not included in the following analysis.
Response time had a dramatic effect on effect sizes. When reaction times were faster than 500 msec, seman- tic priming vanished. The average d was only .03 over 14 observations and .05 over 5 studies. In contrast, priming was clearly available with slower response speeds; aver- age d = .30 by both observations (99) and studies (II).


BLP paper, p290

> Figure 1 displays the effect of practice by plotting the average accuracy and RT over blocks. For RT, the effect is on the order of 100 ms on the word trials, which is larger than the effect observed in the DLP (where it was 40 ms). In addition, participants’ response pattern to words and nonwords seems to have shifted during the experiment. Whereas the beginning of the experiment showed the usual pattern of longer RTs to nonwords than to words, around block 16 responses to nonwords became faster than responses to words. In our opinion, this is because a reasonably large number of words (up to 25%) were perceived as nonwords, so that participants had the impression that the experiment contained more nonword trials than word trials and adapted their response bias accordingly. In this respect, a study by Wagenmakers, Ratcliff, Gomez, and McKoon (2008, Experiment 2) may be particularly informative. These authors showed that in a lexical decision task with 25% nonwords and 75% words, responses to words were faster than responses to nonwords, whereas in a task with 75% nonwords and 25% words, the difference was reversed, with a similar effect for error rates.


blah 2011, p315

> In this section, we define a new imputation algorithm, called column and row adjusted random imputation (CRARI), that allows replacing missing data by imputed values in such a way that the resulting item means are the same as those of the initial data table, and the ICC of the data table under imputation can be set to any desired value in a wide range of possible values, including the ICC of the initial data table (approximate- ly ρp), and the estimate ρcor of ρ0 defined by Eq. 7, when appropriate. In particular, this last option is always suitable for tables of Z scores...


NEXT

> ...


NEXT

> ...



...

