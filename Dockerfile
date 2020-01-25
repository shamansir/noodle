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

RUN npm install

RUN npm run spago:build

EXPOSE 8080
CMD [ "npm", "run", "purs:server" ]
