#!/bin/sh

# Replace NODE_NAME in vm.args with environment variable value
sed -i "s/hub_v2/$NODE_NAME/g; s/test/$NODE_COOKIE/g" releases/0.0.1/vm.args

exec ./bin/hub_v2 console