## Build
 
    make

## Docker

    Build: 
        
        docker build . -t seabattle/seaserver
    Run:
        
        docker run -d -p 8080:8080 --name seaserver --hostname seaserver.ws seabattle/seaserver