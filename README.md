
# Table Of Contents

- [ExRunner-bash](#exrunner-bash)
- [Replication](#replication)
  - [Crash Reproduction](#crash-reproduction)
    - [Prepare input generator container](#prepare-input-generator-container)
    - [Run `WS`:](#run-ws)
    - [Run `WS+BBC`:](#run-wsbbc)
    - [Run `STD`:](#run-std)
    - [Run `STD+BBC`:](#run-stdbbc)
  - [Analysis](#analysis)
  - [RQ1: Crash Reproduction Effectiveness](#rq1-crash-reproduction-effectiveness)
  - [RQ2: Crash Reproduction Efficiency](#rq2-crash-reproduction-efficiency)
- [License & Copyright](#license--copyright)
# ExRunner-bash

Exrunner-bash contains multiple python and bash scripts to evaluate the newly developed features of botsing.

This project contains the following fixed directories:

1- `/bins/` Contains the bytecodes of software under test in JCrashPack.

2- `/crashes/` Contains the stack trace logs in JCrashPack.

3- `/lib/` Contains the botsing-reproduction jar file.

4- `/model-generation/` this directory contains the scripts generating the models for model seeding.

The other directories which start with `crash-reproduction` are for running botsing with different features. Each of these directories contains the input CSV file (`input.csv`), and bash/python scripts needed for running the tool. For instance, `crash-reproduction-model-seeding` is for running botsing model seeding. 

The rational behind having multiple directories is that it makes the experiment easier to be executed on multiple servers. 



For running botsing, first, we need to choose the right directory. Then, we should update the input CSV file (For generating input CSV file use input generator). Finally, we can start botsing execution by running `main.sh`. 

Each `crash-reproduction-*` directory contains two other bash files: `observer.sh` and `parsing.sh`. The former observes the botsing executions and kill them if they get stuck for `X` minutes. The latter parses the valuable information from the execution log and save it in `results/results.csv`.

Botsing's execution logs will be saved in `logs/` directory. Also, the important information about each execution is saved in `results/results.csv`.

Moreover, each `crash-reproduction-*` directory has a `python/` directory. This directory contains small python scripts that utilize the main bash scripts.

# Replication

## Crash Reproduction
**Attention:** The current test suites and CSV files located in this replication package are the ones that are generated during our study. By running more crash reproduction, the generated test cases will be overwritten, and the search process information will be appended to the existing ones in `results.csv` files.

In this study, we run crash reproduction with 4 different configurations:

1- Botsing with `WeightedSum` as the primary search objective (called `WS` hereafter).

2- Botsing with `WeightedSum` as the primary search objective and `Basic Block Coverage` as the second objective (called `WS+BBC` hereafter).

3- Botsing with `STDistance` as the primary search objective (called `STD` hereafter).

4- Botsing with `STDistance` as the primary search objective and `Basic Block Coverage` as the second objective (called `STD+BBC` hereafter).

### Prepare input generator container

For generating the proper inputs for each of the experiment runs, we need to run a seperate docker container. To run this container, you just need to run the following script:

```bash
bash docker-inputs.sh      
```
This script will make a docker container called `exrunner-input-container`. 
### Run `WS`:

**(I) Prepare Input:**
__!__ Before running the input generator, you should create `exrunner-input-container`. See [here](#prepare-input-generator-container).

Run input generator with the following parameters:

```docker
docker exec -it exrunner-input-container bash -c "python input-generator/__init__.py crash-reproduction-ws/inputs.csv WeightedSum <number_of_runs>"
```
Since we repeated each execution for 30 times, in our study, we set the `<number_of_runs>` to 30.

This command will update the `input.csv` in `crash-reproduction-ws` directory.

**(II) Run Botsing with Docker:**
Run the following bash file:
```
. docker-run.sh crash-reproduction-ws <number_of_parallel_executions>
```
The generated test cases will store in `crash-reproduction-ws/results/` directory.

The important data about the executions and the search process will be saved in `crash-reproduction-ws/results/results.csv`.

### Run `WS+BBC`:

**(I) Prepare Input:**
__!__ Before running the input generator, you should create `exrunner-input-container`. See [here](#prepare-input-generator-container).

Run input generator with the following parameters:

```docker
docker exec -it exrunner-input-container bash -c "python input-generator/__init__.py crash-reproduction-new-fitness/inputs.csv WeightedSum <number_of_runs> BasicBlockCoverage"
```
Since we repeated each execution for 30 times, in our study, we set the `<number_of_runs>` to 30.

This command will update the `input.csv` in `crash-reproduction-new-fitness` directory.

**(II) Run Botsing with Docker:**
Run the following bash file:
```
. docker-run.sh crash-reproduction-new-fitness <number_of_parallel_executions>
```
The generated test cases will store in `crash-reproduction-new-fitness/results/` directory.

The important data about the executions and the search process will be saved in `crash-reproduction-new-fitness/results/results.csv`.


### Run `STD`:

**(I) Prepare Input:**
__!__ Before running the input generator, you should create `exrunner-input-container`. See [here](#prepare-input-generator-container).

Run input generator with the following parameters:

```docker
docker exec -it exrunner-input-container bash -c "python input-generator/__init__.py crash-reproduction-new-fitness/inputs.csv IntegrationSingleObjective <number_of_runs>"
```
Since we repeated each execution for 30 times, in our study, we set the `<number_of_runs>` to 30.

This command will update the `input.csv` in `crash-reproduction-new-fitness` directory.


**(II) Run Botsing with Docker:**
Run the following bash file:
```
. docker-run.sh crash-reproduction-new-fitness <number_of_parallel_executions>
```
The generated test cases will store in `crash-reproduction-new-fitness/results/` directory.

The important data about the executions and the search process will be saved in `crash-reproduction-new-fitness/results/results.csv`.


### Run `STD+BBC`:

**(I) Prepare Input:**
__!__ Before running the input generator, you should create `exrunner-input-container`. See [here](#prepare-input-generator-container).

Run input generator with the following parameters:

```docker
docker exec -it exrunner-input-container bash -c "python input-generator/__init__.py crash-reproduction-new-fitness/inputs.csv IntegrationSingleObjective <number_of_runs> BasicBlockCoverage"
```
Since we repeated each execution for 30 times, in our study, we set the `<number_of_runs>` to 30.

This command will update the `input.csv` in `crash-reproduction-new-fitness` directory.


**(II) Run Botsing with Docker:**
Run the following bash file:
```
. docker-run.sh crash-reproduction-new-fitness <number_of_parallel_executions>
```
The generated test cases will store in `crash-reproduction-new-fitness/results/` directory.

The important data about the executions and the search process will be saved in `crash-reproduction-new-fitness/results/results.csv`.


## Analysis
**Attention:** If you do not run any new crash reproduction and use the existing results, you should get the same results as those reported in the paper.


First, go to the `analysis` directory:

```
cd analysis/
```

## RQ1: Crash Reproduction Effectiveness

For analysing `WS` and `WS+BBC` results, run the following R script:

```
Rscript rq1-reproduction-ws.R 
```

For analysing `STD` and `STD+BBC` results, run the following R script:

```
Rscript rq1-reproduction-recore.R 
```

All of the figures will be ssaved in the `analysis/figures` directory.

## RQ2: Crash Reproduction Efficiency

For analysing `WS` and `WS+BBC` results, run the following R script:

```
Rscript rq2-efficiency-ws.R 
```

For analysing `STD` and `STD+BBC` results, run the following R script:

```
Rscript rq2-efficiency-recore.R 
```

All of the figures will be ssaved in the `analysis/figures` directory.

# License & Copyright

The materials herein are all copyright of Pouria Derakhshanfar and Xavier Devroey. The material was produced while working at Delft University of Technology and the University of Namur.

All the source code in this repository is released under [MIT License](LICENSE).

<a rel="license" href="https://creativecommons.org/licenses/by/4.0/">
<img alt="Creative Commons License" style="border-width:0;width:100px" 
src="https://mirrors.creativecommons.org/presskit/buttons/88x31/png/by.png" /></a>
<br />

The documentation and data are licensed under a [Creative Commons Attribution 4.0 License](https://creativecommons.org/licenses/by/4.0/).