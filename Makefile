docker-lint:
	docker run --rm -i hadolint/hadolint < Dockerfile

lint: docker-lint

build-docker:
	docker build -f Dockerfile -t shiny_meta_analyzer .

local-test: build-docker
	docker run --rm -p 80:80 --name my-shiny shiny_meta_analyzer