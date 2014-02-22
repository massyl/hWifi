#
# The program to run to compile .hs files
#
COMP_PROG = ghc --make

#
# Regex to select all .hs files to do bulk compilation
#
FILES = $(wildcard *.hs)

#
# The target file to compile or to run
#
target-file=

# -O2            = to output maximum optimized code
# -prof          = to enable profiling
# -rtsopts       = to enable runtime statistics options
# -auto-all 	 = enable cost center(profiling of all top level functions(you can add manually this pragma = {-# SCC "name" ""#-})
# -caf-all 	 = Constant applicative forms (used for expression that evaluated once. for exampe variables, functions without parameters)
# -fforce-recomp = fore recompilation for all files
GHC_OPTS= -O2 -prof -rtsopts -auto-all -caf-all -fforce-recomp

# Stack size
STACK = -K100M

#
# Compiles all .hs files selected by $(FILES)
#
compile-all :
	$(COMP_PROG) $(FILES)

#
# Compiles all .hs files selected by $(FILES) with profiling enabled
#
compile-prof-all :
	$(COMP_PROG) $(FILES) $(GHC_OPTS)

#
# Compiles a single .hs file given by $(target-file)
#
compile :
	$(COMP_PROG) $(target-file).hs

#
# Compiles a single .hs file and enabling profiling options
#
compile-prof :
	$(COMP_PROG) $(target-file).hs $(GHC_OPTS)

#
# Runs a single file and printing GC statistics to stderr
# -s = gather statistics about GC
# -N = gather statistics about number of OS Threads
# -kxxxM = increase the heap limit by xxx Mega-bytes
#
run-pro-gc : compile-prof
	@time ./$(target-file) +RTS -sstder $(STACK) -RTS

#
# Runs a given program and collects statistics about time execution of each expression.
# Note that the resulting statistics will be put in $(target-file).prof
#
run-pro-time : compile-prof
	time ./$(target-file) +RTS -p $(STACK) -RTS

#
# Runs a given program and gathers statistics related to heap usage
# -hc = outputs standard heap usage
# It will put the result in $(target-file).hp and will contain samples taken at regular interval
# If you need to increase or specify anther frequency, use:
# -iN = where N is the number of seconds.
# It generates a postscript file that contains a graph of the gathered results
#
run-pro-heap : compile-prof
	time ./$(target-file) +RTS -hc -p $(STACK) -RTS
	hp2ps -e8in -c $(target-file).hp

#
# Same as run-pro-heap but gives statistics per data type
# -hy = gather statistics of heap usage by data type
#
run-pro-heap-type : compile-prof
	time ./$(target-file) +RTS -hy -p $(STACK) -RTS
	hp2ps -e8in -c $(target-file).hp


#
# Same as run-pro-heap but gives statistics per data constructors
# -hd = gather statistic of heap usage by data constructors
#
run-pro-heap-ctor : compile-prof
	time ./$(target-file) +RTS -hd -p $(STACK) -RTS
	hp2ps -e8in -c $(target-file).hp

#

#
#
clean :
	@rm -f $(FILES:.hs=.o)
	@rm -f $(FILES:.hs=.hi)


# compilexp
# $(COMP_PROG)  profiling.hs -prof -rtsopts -auto-all -caf-all -fforce-recomp


# 3) activate profiling of installed libraries in .cabal/config
#  Turn all entries related to profiling to True


# 4) If you try to run your executable with
# ./profiling 1e5 +RTS -sstder (-s stands for GC)
# and you get that you need to link with profiling options enabled then
# run the compiler with

# $(COMP_PROG)-rtsopts profiling.hs

# 5) get the profiling informations

# time ./program args +RTS -p -K100M (-k for fixing heap limit)

install:
	sudo apt-get install -y haskell-platform

deps:
	echo "Nothing yet"

sandbox-init:
	cabal sandbox init

sandbox-delete:
	cabal sandbox delete

cabal-init:
	cabal init

run:
	runhaskell Network/HWifi.hs

tests:
	runhaskell HWifiTests.hs
