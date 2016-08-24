
dslc_src := $(wildcard com/lovullo/dslc/*.java)
dslc_bin := $(dslc_src:.java=.class)

.PHONY: dslc clean

dslc: dslc.jar

%.class: %.java
	javac $<

# we explicitly specify a glob on the path because inner classes are compiled
# into their own files
dslc.jar: $(dslc_bin)
	jar cfm $@ dslc.manifest com/lovullo/dslc/*.class

clean:
	rm -f $(dslc_bin) dslc.jar
