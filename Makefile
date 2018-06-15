TOKEN := $(shell echo $${TOKEN})

all:
	npm i --global yarn
	cd ui; sed -i "s|<your-token-here>|$(TOKEN)|g" src/Main.elm
	cd ui; yarn install
	cd ui; yarn build
	cd ui; yarn optimize
