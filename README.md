Plawireg: Play With The Reference Graph
=======================================

This project is an experiment on representing the reference genome as a graph.

Nothing really interesting to see for now …

Build
-----

This project uses the `omake` build-system, just run:

    omake

Run Tests
---------

One can generate some very small test data files:

    ./plawireg generate test fasta mini-reference.fasta
    ./plawireg generate test dbsnp mini-dbsnp.vcf

and run some stuff:

    ./plawireg test-load all 6 mini-reference.fasta mini-dbsnp.vcf
    
You can make things more verbose:

    VERBOSE=true ./plawireg test-load all 6 mini-reference.fasta mini-dbsnp.vcf

Or ask for “linux-only” memory-usage stats along the way:

    ./plawireg test-load all 6 mini-reference.fasta mini-dbsnp.vcf memory-stats

The test dumps a `out.dot` file that one can observe:

    dot -Tpng out.dot -O

It should give something like this:

![Output as of 2015-03-06][plawireg-dot-output]

[plawireg-dot-output]: https://cloud.githubusercontent.com/assets/617111/6535827/f161ffd8-c415-11e4-8b21-b2f97655c407.png

