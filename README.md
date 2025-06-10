
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

# Options

 - `MAX_POSITION` - maximum length of value to be hashed
 - `OFFSET_RANGE` - range around the position of the substring where we attempt to do fuzzy match
 - `SALT_LENGTH` - salt length
 - `SUBSTRING_LENGTH` - length of the substring window that we do fuzzy match for
 - `LOG_LEVEL` - `INFO`, `DEBUG` etc.

# Example

| ![Hashing example](img/hash.drawio.svg "Hashing example") | 
|:--:| 
| At position = 3 we have 5 different possible positions. Each position has its own salt. Hash is generated for each position. Config: OFFSET_RANGE=2 SUBSTRING_LENGTH=2 |

# Run tests

```bash
PPRL_LOG_LEVEL=DEBUG Rscript -e "devtools::test()"
```

# GUI

```bash
Rscript -e "devtools::load_all(); PPRL::launch_app();"
```