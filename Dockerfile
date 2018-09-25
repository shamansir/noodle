FROM node:8

RUN mkdir -p /usr/src/rpd-purs
WORKDIR /usr/src/rpd-purs

COPY . .

RUN apt-get update -yqq

RUN apt-get -y install g++ gcc libc6-dev libffi-dev libgmp-dev make && \
    apt-get -y install xz-utils zlib1g-dev git gnupg

RUN curl -sSL https://get.haskellstack.org/ | sh

# TODO: move to parcel / psc-package
RUN npm install -g pulp bower && \
    npm install -g purescript && \
    npm run purs:build && \
    npm run purs:test

EXPOSE 1337

CMD [ "npm", "run", "purs:server" ]
