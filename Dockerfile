FROM node:latest

# Create app directory
WORKDIR /app

# Install PureScript global
RUN npm cache clean --force && \
    npm install -g purescript@0.13 --unsafe-perm spago

# Install yarn global
# RUN npm install -g yarn

# Install spago global
# RUN npm install -g --unsafe-perm spago

COPY . .

RUN yarn clean

RUN yarn install

RUN yarn spago:bundle

FROM nginx:alpine

WORKDIR /usr/share/nginx/html

# RUN mkdir output

COPY --from=0 /app/output ./output
COPY --from=0 /app/index.html .

RUN mkdir ./css
RUN mkdir ./example-css

COPY --from=0 /app/src/Noodle/Render/Html/*.css ./css/
COPY --from=0 /app/examples/raydraw/Toolkit/Render/Html/*.css ./example-css/
COPY --from=0 /app/index.docker.css ./index.css

COPY nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 8080
