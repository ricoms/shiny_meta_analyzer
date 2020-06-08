build-docker:
	docker build -f Dockerfile -t shiny_meta_analyzer .

local-test: build-docker
	docker run --rm -p 3838:3838 shiny_meta_analyzer