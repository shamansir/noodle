FROM --platform=linux/amd64 qualified/purescript:0.15

# Create app directory
WORKDIR /app

COPY . .

# Install PureScript global
# RUN npm cache clean --force && \
#    npm install -g purescript@0.15 --unsafe-perm spago parcel-bundler
RUN npm cache clean --force && \
    npm install

RUN spago -x ./example.dhall build

RUN apt-get -qq -y update; apt-get install -qq -y curl wget

RUN curl https://hydra.ojack.xyz/bundle.min.js\?1.3.27 -o ./hydra.bundle.v1.3.27.min.js

RUN mkdir static

FROM nginx:alpine

WORKDIR /usr/share/nginx/html

# RUN mkdir output

COPY --from=0 /app/static .

# COPY --from=0 /app/output ./output
# COPY --from=0 /app/index.html .

#RUN mkdir ./css
#RUN mkdir ./example-css

#COPY --from=0 /app/src/App/WebApp.css ./css/
# COPY --from=0 /app/examples/raydraw/Toolkit/Render/Html/*.css ./example-css/
#COPY --from=0 /app/index.docker.css ./index.css

COPY nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 8080