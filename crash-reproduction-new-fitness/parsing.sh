
## inputs
pid=$1
execution_idx=$2
application=$3
case=$4
version=$5
frame=$6
search_budget=$7
search_algorithm=$8
objectives=$9
secondary_objective=${10}
##

## wait for the process to finish
while kill -0 "$pid"; do
            sleep 1
done
echo ">>> $secondary_objective"
python python/write_on_csv_file.py $execution_idx $application $case $version $frame $search_budget $search_algorithm $objectives $secondary_objective &
