### psycholinguistics discovery sims
###### [timothy leffel](http://lefft.xyz), july24/2017
<hr>

this is a project investigating consequences of various design and analysis decisions in RT-based psycholinguistics experiments. main tool is simulation. after code infrastructure is in place, will use the literature to derive realistic effect sizes, means/sd's, etc. publicly available RT datasets also high on the wishlist. 


inspiration comes in part from this very nice/concise/thought-provoking [note by Emmanuel Chemla](http://www.emmanuel.chemla.free.fr/thresholds/OutliersCriterion.html). 

repo contents, notes to self, TODO list below. 
<hr>

<br><br><br><br><br>


##### contents of repo:
<hr>

- `psycholing_discovery_sims.{rmd, html}` -- main document 
- `functions.r` -- setting things up for simulating experiments + quickly displaying results (sourced at top of main doc)
- `chemla_code.r` -- code for original chemla note (with edits/rewrite)
- `boosh.css` -- style for main doc
- `out/` -- plots, simulated datasets, etc. 


<br>

##### TODO list
<hr>

###### TODO items that need attention


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


###### completed TODO items

	-x- > TODO: add param for number of outlier subj's
	-x- > TODO: finish plotting function
	-x- > TODO: write func to extract relevant bits from model summaries

<br>

##### misc notes to self
<hr>

	> 
	> 
	> ...

<br>


##### estimates of mean/sd/effect size in rt priming studies
<hr>

- holcomb-neville 1990 lcp: 
	> visual 
		- 
		- 
		
	> auditory  
	> 
	> ...

