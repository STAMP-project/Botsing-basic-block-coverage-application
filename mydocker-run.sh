experiment=$1
LIMIT=$2
# IFS=$'\n'
# Stop the container from previous experiments
mydocker stop exrunner-container
# Remove old containers
mydocker rm exrunner-container
# Remove previous docker image
mydocker rmi tudelft/exrunner
# Build a new docker image
mydocker image build -t tudelft/exrunner $(pwd)
# Execution
# After building the the image, we run the container
mydocker run -dit --name exrunner-container  \
--mount type=bind,source="$(pwd)/$experiment/consoleLog",target=/reproduction/$experiment/consoleLog \
--mount type=bind,source="$(pwd)/$experiment/logs",target=/reproduction/$experiment/logs \
--mount type=bind,source="$(pwd)/$experiment/results",target=/reproduction/$experiment/results \
--mount type=bind,source="$(pwd)/crashes",target=/reproduction/crashes \
--mount type=bind,source="$(pwd)/bins",target=/reproduction/bins \
tudelft/exrunner
# Execute main.sh in the running container
 mydocker exec -it exrunner-container bash -c "cd $experiment; bash main.sh $LIMIT > consoleLog/consoleOut.txt 2> consoleLog/consoleErr.txt"
echo "Done!"