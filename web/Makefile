.PHONY: website
website: index.html distributions.html

index.html: parts/index.html parts/head.html parts/foot.html viz.js
	cat parts/head.html parts/index.html parts/foot.html > index.html

distributions.html: parts/distributions.html parts/head.html parts/foot.html distributions.js
	cat parts/head.html parts/distributions.html parts/foot.html > distributions.html

distributions.js: src/distributions.jsx
	npx babel src --out-dir . --presets react-app/prod	

viz.js: src/viz.jsx
	npx babel src --out-dir . --presets react-app/prod	

