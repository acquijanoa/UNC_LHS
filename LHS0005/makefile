# Get the current directory
current_directory := $(shell pwd)

# Target to build the Docker image
build_docker:
	docker build --platform=linux/x86_64 -t lhs0005-project .

# Run the Docker image
run_docker:	
	docker run -it --rm --platform=linux/x86_64 -p 8787:8787 \
	-e PASSWORD=lhs0005 \
	-v $(current_directory):/home/project lhs0005-project