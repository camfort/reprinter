# Scrap Your Reprinter - A Datatype Generic Algorithm for Layout-Preserving Refactoring

Refactoring tools are extremely useful in software development:
they provide automatic source code transformation tools for a variety
of laborious tasks like renaming, formatting, standardisation,
modernisation, and modularisation. A refactoring tool transforms
part of a code base and leaves everything else untouched, including
secondary notation like whitespace and comments. We describe a
novel datatype generic algorithm for the last step of a refactoring
tool – the reprinter – which takes a syntax tree, the original source
file, and produces an updated source file which preserves secondary
notation in the untransformed parts of the code. As a result of being
datatype generic, the algorithm does not need changing even
if the underlying syntax tree definition changes. We impose only
modest preconditions on this underlying data type. The algorithm
builds on existing work on datatype generic programming and zippers.
This is useful technology for constructing refactoring tools
as well as IDEs, interactive theorem provers, and verification and
specification tools.

https://www.cs.kent.ac.uk/people/staff/dao7/publ/reprinter2017.pdf
