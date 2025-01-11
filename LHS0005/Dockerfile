# Use the rocker/verse base image
FROM rocker/verse:latest

# Install additional R packages
RUN R -e "install.packages(c('survey','haven','sf'))"

# Install required system packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    python3-pip \
    python3-venv \
    python3-dev \
    pipx \
    nano \		
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install Snakemake using pipx
RUN pipx install snakemake

# Add pipx to PATH
ENV PATH="/root/.local/bin:$PATH"

# Set working directory
WORKDIR /home/project/
ENV HOME=/home/project/

# Command to run RStudio server
CMD ["/init"]
