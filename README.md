# racket-gedcom

This is actually in semi-decent shape in the following sense: it
does a fairly good job of parsing the gedcom files produced by wikitree.
I put it together by following the formal spec of GEDCOM files (ugh,
blecch), and I'm optimistic that it would be able to parse GEDCOM files
from other sources, though given the overall nastiness of the spec,
I strongly suspect that every different GEDCOM producer would have its
own peculiarities.

Its most conspicuous omission at this point is a reasonably large GEDCOM
file that it parses that's included in the repo; the problem here is
that ... oh, you know, I should download a publicly available GEDCOM
from wikitree, that would be pretty simple. Okay, next time...
