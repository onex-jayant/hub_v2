
FROM ubuntu:latest

COPY _build/default/rel/hub_v2 /work/hub_v2/

WORKDIR /work/hub_v2

COPY set_args.sh /work/hub_v2

RUN chmod +x /work/hub_v2/set_args.sh

EXPOSE 9000

ENTRYPOINT [ "./set_args.sh"]

# docker run -e NODE_NAME=hub@jayant -e NODE_COOKIE=test -p 9001:9000 -it hub

# test:bind(1001,206,"jayant","Jayant@123","10.22.21.87",5555,10).