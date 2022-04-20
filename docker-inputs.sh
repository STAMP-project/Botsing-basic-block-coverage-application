docker build -t exrunner-input-img $(pwd) -f Dockerfile.input-generator

docker run -dit --name exrunner-input-container  \
--mount type=bind,source="$(pwd)/input-generator",target=/workspace/input-generator \
--mount type=bind,source="$(pwd)/crash-reproduction-ws",target=/workspace/crash-reproduction-ws \
--mount type=bind,source="$(pwd)/crash-reproduction-new-fitness",target=/workspace/crash-reproduction-new-fitness \
exrunner-input-img

