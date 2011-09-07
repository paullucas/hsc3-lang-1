css=$(HOME)/sw/hsc3/Help/hsc3.css

%.html : %.lhs
	pandoc -f markdown+lhs -t html -c $(css) --smart < $< > $*.html

all:
	make pattern.html
	make id.html

clean:
	rm -f *.html
