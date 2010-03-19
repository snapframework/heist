all: out/templates.html

clean:
	rm -Rf out

out/templates.html: templates.md templates.css
	mkdir -p out
	cp templates.css out/
	pandoc -c templates.css -s -f markdown  -o out/templates.html templates.md
