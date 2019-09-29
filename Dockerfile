FROM ubuntu:19.04

RUN addgroup --system user && adduser --system --group user
RUN addgroup --system node && adduser --system --group node

RUN mkdir -p /home/node
RUN mkdir -p /home/node/.nvm
RUN chown -R node:node /home/node && chmod -R 755 /home/node

ENV STACK_DIR /home/stack
RUN mkdir -p $STACK_DIR
RUN chown -R user:user $STACK_DIR && chmod -R 755 $STACK_DIR


ENV NODE_VERSION 12.11.0
# ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
# ENV PATH=$PATH:/home/node/.npm-global/bin

RUN apt-get update -yqq

RUN apt-get -yqq install curl && \
    apt-get -yqq install g++ gcc libc6 libc6-dev libffi-dev libgmp-dev make && \
    apt-get -yqq install xz-utils zlib1g-dev git gnupg && \
    apt-get -yqq install ghc ghc-prof ghc-doc

RUN ldd --version

ENV NVM_DIR /home/node/.nvm
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash \
    && . $NVM_DIR/nvm.sh \
    && nvm install $NODE_VERSION \
    && nvm alias default $NODE_VERSION \
    && nvm use default

ENV NODE_DIR  $NVM_DIR/versions/node/v$NODE_VERSION
ENV NODE_PATH $NODE_DIR/lib/node_modules
ENV PATH      $NODE_DIR/bin:$PATH

RUN chown -R node:node $NODE_DIR && chmod -R 755 $NODE_DIR

RUN node -v

# add user
RUN mkdir -p /usr/src/rpd-purs
RUN chown -R user:user /usr/src/rpd-purs && chmod -R 755 /usr/src/rpd-purs

USER user

WORKDIR /usr/src/rpd-purs

COPY . .

# RUN curl -sSL https://get.haskellstack.org/ | sh -s - -d $STACK_DIR

ENV STACK_VERSION 2.1.3
ENV STACK_SOURCE linux-x86_64-static

RUN curl -sSL https://get.haskellstack.org/stable/$STACK_SOURCE.tar.gz --output stack.tar.gz \
    && tar -xvzf ./stack.tar.gz \
    && mv ./stack-$STACK_VERSION-$STACK_SOURCE/stack $STACK_DIR \
    && rm ./stack.tar.gz \
    && rm -R ./stack-$STACK_VERSION-$STACK_SOURCE \
    && chmod +x $STACK_DIR/stack

RUN ls -laF $STACK_DIR

ENV PATH $STACK_DIR:$PATH

# RUN stack -v

USER node

# TODO: move to parcel / psc-package
RUN npm cache clean --force && \
    npm install -g pulp && \
    npm install -g spago && \
    npm install -g purescript@0.12

# USER user

RUN spago install && \
    npm run purs:build && \
    npm run purs:test

EXPOSE 1337

CMD [ "npm", "run", "purs:server" ]
