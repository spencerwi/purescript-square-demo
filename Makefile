output.js: clean
	pulp build -O --to output.js

clean:
	-rm -rf output/
	$(RM) output.js
