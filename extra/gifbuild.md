## Interesting Frames

0022: EOL
0023: Show code 5 sec
0052: EOL
0053: Show code 5 sec

0092: EOL
0133: Pre Exec
0136: Pause
0171: EOL
0196: EOL

0200: Pause
0235: EOL
0239: Pre Exec
0240: Pause
0255: EOL
0256: Pause
0297: EOL
0340: Pause
0365: EOL
0368: Pause 2 sec
0371: Pause 2 sec
0373: Pause
0388: Pause
0414: Pause
0418: Pause
0444: EOL
0446: EOL
0477: EOL
0505: EOL
0526: Pause
0532: EOL
0536: Pause 2 sec
0574: Pause
0591: EOL
0593: Pause
0605: EOL
0607: Pause
0630: Pause 2 sec
0632: EOL
0648: Pause 2 sec

## Code to Create Animated Gif

Do several passes:

1. Simple encoding with default delay for all frames
2. Encode the different delays

```
gifsicle out* --optimize=3 > preproc.gif

tar -jcvf out.tbz2 out*.gif

# for some godforsaken reason we can't just modify delays on spefic frames so
# we're stuck setting delay on all of them

cp preproc.gif preproc2.gif

# we're storing our gif frames as the optimized gif, so we need to explode into
# frames, rebuild an unoptimized animated gif, and then change the delays on
# that

gifsicle --unoptimize --explode preproc.gif --output frame && \
gifsicle frame* > preproc2.gif && \
rm frame* && \
gifsicle preproc2.gif -d7 "#0-21" \
  -d100 "#22" -d400 "#23" -d7 "#24-51" -d100 "#52" -d400 "#53" \
  -d7 "#54-91" -d100 "#92" -d7 "#93-132" -d200 "#133" -d7 "#134-135" \
  -d200 "#136" -d7 "#137-158" -d100 "#159" -d7 "#160-170" -d100 "#171" \
  -d7 "#172-195" -d100 "#196" \
  -d7 "#197-199" -d200 "#200" -d7 "#201-234" -d100 "#235" -d7 "#236-238" \
  -d200 "#239" -d200 "#240" -d7 "#241-254" -d200 "#255" -d200 "#256" \
  -d7 "#257-279" -d200 "#280" -d7 "#281-296" \
  -d100 "#297" -d7 "#298-325" -d200 "#326" -d7 "#327-339" -d200 "#340" \
  -d7 "#341-364" -d100 "#365" -d7 "#366-367" -d100 "#368" -d200 "#369" \
  -d7 "#370" -d200 "#371" \
  -d7 "#372" -d200 "#373" -d7 "#374-387" -d200 "#388" -d7 "#389-413" \
  -d200 "#414" -d7 "#415-417" -d200 "#419" -d7 "#419-443" -d100 "#444" \
  -d7 "#445" -d100 "#446" -d7 "#447-476" -d100 "#477" -d7 "#478-504" \
  -d100 "#505" -d7 "#506-525" -d200 "#526" -d7 "#527-531" -d100 "#532" \
  -d7 "#533-535" -d200 "#536" -d7 "#537-573" -d200 "#574" -d7 "#575-590" \
  -d100 "#591" -d7 "#592" -d200 "#593" -d7 "#594-604" -d200 "#605" \
  -d7 "#606" -d200 "#607" -d7 "#608-629" -d300 "#630" -d7 "#631" -d100 "#632" \
  -d7 "#633-647" -d700 "#648" \
  --optimize=3 > fin.gif &&
rm preproc2.gif
```

## Splitting Into two

We want to split the gif into two to make it a bit more accessible.  Last frame
of first one is #418.  In the end, we're not doing this but keeping code here in
case we change our minds.

```
gifsicle --unoptimize --explode preproc.gif --output frame
Rscript split_by_frame.R # scriplet to re-name files into two groups

gifsicle gif1-* > preproc-gif1.gif
gifsicle gif2-* > preproc-gif2.gif

rm *frame*

gifsicle preproc-gif1.gif -d7 "#0-21" \
  -d100 "#22" -d400 "#23" -d7 "#24-51" -d100 "#52" -d400 "#53" \
  -d7 "#54-91" -d100 "#92" -d7 "#93-132" -d200 "#133" -d7 "#134-135" \
  -d200 "#136" -d7 "#137-158" -d100 "#159" -d7 "#160-170" -d100 "#171" \
  -d7 "#172-195" -d100 "#196" \
  -d7 "#197-199" -d200 "#200" -d7 "#201-234" -d100 "#235" -d7 "#236-238" \
  -d200 "#239" -d200 "#240" -d7 "#241-254" -d200 "#255" -d200 "#256" \
  -d7 "#257-279" -d200 "#280" -d7 "#281-296" \
  -d100 "#297" -d7 "#298-325" -d200 "#326" -d7 "#327-339" -d200 "#340" \
  -d7 "#341-364" \
  -d100 "#365" -d7 "#366-367" -d200 "#368" -d7 "#369-370" -d200 "#371" \
  -d7 "#372" -d200 "#373" -d7 "#374-387" -d200 "#388" -d7 "#389-413" \
  -d200 "#414" -d7 "#415-417" -d700 "#418" \
  --optimize=3 > fin1.gif

gifsicle preproc-gif2.gif \
  -d200 "#0" -d7 "#1-24" -d100 "#25" \
  -d7 "#26" -d100 "#27" -d7 "#28-57" -d100 "#58" -d7 "#59-85" \
  -d100 "#86" -d7 "#87-106" -d200 "#107" -d7 "#108-112" -d100 "#113" \
  -d7 "#114-116" -d200 "#117" -d7 "#118-154" -d200 "#155" -d7 "#156-171" \
  -d100 "#172" -d7 "#173" -d200 "#174" -d7 "#175-185" -d200 "#186" \
  -d7 "#187" -d200 "#188" -d7 "#189-210" -d300 "#211" -d7 "#212" -d100 "#213" \
  -d7 "#214-228" -d700 "#229" \
  --optimize=3 > fin2.gif

# Log in to R to do all this

xx <- '
  -d200 "#419" -d7 "#420-443" -d100 "#444" \
  -d7 "#445" -d100 "#446" -d7 "#447-476" -d100 "#477" -d7 "#478-504" \
  -d100 "#505" -d7 "#506-525" -d200 "#526" -d7 "#527-531" -d100 "#532" \
  -d7 "#533-535" -d200 "#536" -d7 "#537-573" -d200 "#574" -d7 "#575-590" \
  -d100 "#591" -d7 "#592" -d200 "#593" -d7 "#594-604" -d200 "#605" \
  -d7 "#606" -d200 "#607" -d7 "#608-629" -d300 "#630" -d7 "#631" -d100 "#632" \
  -d7 "#633-647" -d200 "#648" \
  --no-loopcount --optimize=3 > fin.gif
'
xx.2 <- xx
frames.2 <- gregexpr('(?<=#|-)[0-9]+', xx.2, perl=TRUE)
frames.char <- as.character(as.integer(regmatches(xx.2, frames.2)[[1L]]) - 419)
regmatches(xx.2, frames.2) <- list(frames.char)



