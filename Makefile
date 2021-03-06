# MUB-Search
#
# 2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)


# Flags.
#
# CC is the Haskell compiler to use.
# FLAGS are any additional command line flags passed to each compilation.
# PROF is for any profiling information.
#
CC = ghc
#FLAGS = -XFlexibleInstances -XFlexibleContexts -static -optl-static -optl-pthread -fforce-recomp -O -O2 -optc-O2 -optc-O3 -fexcess-precision
#FLAGS = -XFlexibleInstances -XFlexibleContexts -optl-pthread -fforce-recomp -O -O2 -optc-O2 -optc-O3 -fexcess-precision -fvia-C
FLAGS = -XFlexibleInstances -XFlexibleContexts -static -optl-static -optl-pthread -fforce-recomp -O2 -fexcess-precision 
#PROF = -prof -auto-all -caf-all
PROF = 
PACKAGES = -package containers

# Operations.
#
all: utilities major

utilities: MUBs2LaTeX CheckFourierFamily EquivBases MakeUniqPerms
clean_utilities:
	rm -f MUBs2LaTeX CheckFourierFamily EquivBases MakeUniqPerms

major: FundamentalNeighbors Simplify MUB-Search HNSS MUB-Search2 FundamentalNeighborsSimple
clean_major:
	rm -f FundamentalNeighbors Simplify MUB-Search HNSS MUB-Search2

clean: clean_utilities clean_major
	rm -f *.hi *.o *.prof *.aux *.hp *.ps

commit:
	git commit -a

push:
	git push git@github.com:nblythe/MUB-Search.git master


# Modules that provide specific encapsulated functionalities.
#
Bound.o: Bound.hs
	$(CC) -c Bound.hs $(FLAGS) $(PACKAGES) $(PROF)

Cyclotomic.o: Cyclotomic.hs Bound.o
	$(CC) -c Cyclotomic.hs $(FLAGS) $(PACKAGES) $(PROF)

Perms.o: Perms.hs
	$(CC) -c Perms.hs $(FLAGS) $(PACKAGES) $(PROF)

Combinadics.o: Combinadics.hs
	$(CC) -c Combinadics.hs $(FLAGS) $(PACKAGES) $(PROF)

SublistPred.o: SublistPred.hs Combinadics.o
	$(CC) -c SublistPred.hs $(FLAGS) $(PACKAGES) $(PROF)

Polynomial.o: Polynomial.hs
	$(CC) -c Polynomial.hs $(FLAGS) $(PACKAGES) $(PROF)

Cliques.o: Cliques.hs
	$(CC) -c Cliques.hs $(FLAGS) $(PACKAGES) $(PROF)


# Small utilities.
#
MUBs2LaTeX: MUBs2LaTeX.hs
	$(CC) -o MUBs2LaTeX MUBs2LaTeX.hs $(FLAGS) $(PACKAGES) $(PROF)

CheckFourierFamily: CheckFourierFamily.hs Perms.o
	$(CC) -o CheckFourierFamily CheckFourierFamily.hs Perms.o $(FLAGS) $(PACKAGES) $(PROF)

EquivBases: EquivBases.hs Perms.o
	$(CC) -o EquivBases EquivBases.hs Perms.o $(FLAGS) $(PACKAGES) $(PROF)

MakeUniqPerms: MakeUniqPerms.hs
	$(CC) -o MakeUniqPerms MakeUniqPerms.hs $(FLAGS) $(PROF)

# The big boys.
#
FundamentalNeighbors: FundamentalNeighbors.hs Cyclotomic.o SublistPred.o Combinadics.o Perms.o Bound.o
	$(CC) -o FundamentalNeighbors FundamentalNeighbors.hs Cyclotomic.o Bound.o SublistPred.o Combinadics.o Perms.o $(FLAGS) $(PACKAGES) $(PROF)

FundamentalNeighborsSimple: FundamentalNeighborsSimple.hs Cyclotomic.o Bound.o
	$(CC) -o FundamentalNeighborsSimple FundamentalNeighborsSimple.hs Cyclotomic.o Bound.o $(FLAGS) $(PACKAGES) $(PROF)

Simplify: Simplify.hs
	$(CC) -o Simplify Simplify.hs $(FLAGS) $(PACKAGES) $(PROF)

MUB-Search: MUB-Search.hs Cliques.o
	$(CC) -o MUB-Search MUB-Search.hs Cliques.o $(FLAGS) $(PACKAGES) $(PROF)

MUB-Search2: MUB-Search2.hs Cliques.o
	$(CC) -o MUB-Search2 MUB-Search2.hs Cliques.o $(FLAGS) $(PACKAGES) $(PROF)

HNSS: HNSS.hs Polynomial.o
	$(CC) -o HNSS HNSS.hs Polynomial.o $(FLAGS) $(PACKAGES) $(PROF)

