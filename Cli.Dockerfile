FROM --platform=linux/amd64 qualified/purescript:0.15

# Create app directory
WORKDIR /app

COPY . .

# These dependencies are unused. To fix this warning, remove the following packages from the list of dependencies in your config:
# - argonaut
# - datetime
# - default-values
# - dom-indexed
# - halogen-hooks
# - js-fileio
# - lazy
# - node-streams
# - psci-support
# - purescript-wire
# - random
# - variant

# Install PureScript global
# RUN npm cache clean --force && \
#    npm install -g purescript@0.15 --unsafe-perm spago parcel-bundler
RUN npm cache clean --force && \
    npm install

RUN spago -x ./example.dhall build

ENTRYPOINT [ "spago run" ]