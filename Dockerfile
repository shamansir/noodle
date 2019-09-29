FROM node:12

ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
ENV PATH=$PATH:/home/node/.npm-global/bin

RUN apt-get update -yqq

RUN apt-get -y install g++ gcc libc6 libc6-dev libffi-dev libgmp-dev make && \
    apt-get -y install xz-utils zlib1g-dev git gnupg

RUN ldd --version

# add user
RUN addgroup --system user && adduser --system --group user
RUN mkdir -p /usr/src/rpd-purs
RUN chown -R user:user /usr/src/rpd-purs && chmod -R 755 /usr/src/rpd-purs

USER user

WORKDIR /usr/src/rpd-purs

COPY . .

USER node

# RUN curl -sSL https://get.haskellstack.org/ | sh

# TODO: move to parcel / psc-package
RUN npm install -g pulp && \
    npm install -g spago && \
    npm install -g purescript@0.12

USER user

RUN spago install && \
    npm run purs:build && \
    npm run purs:test

EXPOSE 1337

CMD [ "npm", "run", "purs:server" ]
