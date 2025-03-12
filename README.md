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
that these generated GEDCOM files appear to be copyrighted, and not legal
to include in this distribution, which is painful; having a running
example is more or less the first and best line of insurance for any
piece of code.

Take a look at example.rkt for a file that *would* work if I were allowed
to include a gedcom file.