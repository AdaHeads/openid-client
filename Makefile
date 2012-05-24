GNATMAKE_ARGS=-s -gnat05 -gnatyO -gnatE -gnato -gnatv -gnati1 -gnatf -gnatn -fstack-check -gnatyO -m

all:
	GPR_PROJECT_PATH=/opt/aws/lib/gnat:/opt/gnatcoll/lib/gnat:/opt/yolk/lib/gnat gnatmake $(GNATMAKE_ARGS) -Popenid
