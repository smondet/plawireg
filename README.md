Plawireg: Play With The Reference Graph
=======================================

This project is an experiment on representing the reference genome as a graph.

Nothing really interesting to see for now …

Build
-----

This project tries the `obuild` build-system:

    obuild configure
    obuild build

Run Tests
---------

One can generate some very small test data files:

    ./plawireg generate test fasta mini-reference.fasta
    ./plawireg generate test dbsnp mini-dbsnp.vcf

and run some stuff:

    ./plawireg test-load mini-reference.fasta mini-dbsnp.vcf
    
You can make things more verbose:

    VERBOSE=true ./plawireg test-load mini-reference.fasta mini-dbsnp.vcf

Or ask for “linux-only” memory-usage stats along the way:

    ./plawireg test-load mini-reference.fasta mini-dbsnp.vcf memory-stats

The test dumps a `out.dot` file that one can observe:

    dot -Tpng out.dot -O



