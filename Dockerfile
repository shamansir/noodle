FROM node:latest

# Create app directory
WORKDIR /app

# Install PureScript global
RUN npm cache clean --force && \
    npm install -g purescript --unsafe-perm spago

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

EXPOSE 80
# CMD [ "yarn", "parcel:serve" ]
