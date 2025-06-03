
# Installation

```
# bash devtools dependencies Ubuntu 22
sudo apt -y install libfontconfig1-dev
sudo apt install libharfbuzz-dev libfribidi-dev libtiff5-dev
sudo apt-get install libssl-dev
sudo apt-get install libcurl4-openssl-dev

# R
install.packages('rlang')
install.packages('devtools')
```

# Run tests

```bash
Rscript -e "devtools::test()"
```

# GUI

```bash
Rscript -e "devtools::load_all(); PPRL::launch_app();"
```