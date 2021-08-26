FROM --platform=linux/amd64 tprimke/purescript:0.14.3

# Create app directory
WORKDIR /app

COPY . .

# Install PureScript global
# RUN npm cache clean --force && \
#    npm install -g purescript@0.14 --unsafe-perm spago parcel-bundler
RUN npm cache clean --force && \
    npm install -g --unsafe-perm parcel@next

RUN spago -x ./example.dhall build

RUN parcel -V

RUN mkdir static

RUN parcel build --dist-dir ./static index.html

RUN curl https://hydra.ojack.xyz/bundle.min.js\?1.2.6 --output ./hydra.bundle.v1.2.6.min.js

FROM nginx:alpine

WORKDIR /usr/share/nginx/html

# RUN mkdir output

COPY --from=0 /app/static .

# COPY --from=0 /app/output ./output
# COPY --from=0 /app/index.html .

#RUN mkdir ./css
#RUN mkdir ./example-css

#COPY --from=0 /app/src/App/App.css ./css/
# COPY --from=0 /app/examples/raydraw/Toolkit/Render/Html/*.css ./example-css/
#COPY --from=0 /app/index.docker.css ./index.css

COPY nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 8080