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
    
For now the `verbose` option works only on Linux:

    ./plawireg test-load mini-reference.fasta mini-dbsnp.vcf verbose

(The dbSNP part is not yet implemented ;) )




