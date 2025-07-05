docker run --name nodejs -itdp 3000:3000 -p 3001:3001 -v ~/Documents/Projects/docker/node:/app node

docker exec -itt nodejs /bin/bash

apt update && apt upgrade && apt install nano
