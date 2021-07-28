

echo "Stopping and removing old docker container......"
docker stop plumber-api && docker rm plumber-api
echo "Building dockerfile..."
docker build -t plumber-api .
echo "Running new container......"
docker run -d -p 8000:8000 --name plumber-api plumber-api
echo "Checking docker image......"
docker container ls
