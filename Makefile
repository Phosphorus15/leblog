NPM = npm

STACK = stack

default: react deploy run

react:
	cd leblog-front && $(NPM) install
	cd leblog-front && $(NPM) run build

deploy:
	cp leblog-front/build/* ./static/

run:
	stack run