
dslc_src := $(wildcard com/lovullo/dslc/*.java)
dslc_bin := $(dslc_src:.java=.class)

.PHONY: all all-nodoc dslc clean check info pdf html

export CLASSPATH=$(DSLC_CLASSPATH)

all: dslc
dslc: dslc.jar

%.class: %.java
	javac $<

# recurisve targets
check:
info:
pdf:
html:

# we explicitly specify a glob on the path because inner classes are compiled
# into their own files
dslc.jar: $(dslc_bin)
	jar cfm $@ dslc.manifest com/lovullo/dslc/*.class

clean:
	rm -f $(dslc_bin) dslc.jar
