FROM ubuntu:19.04
# 19.04 fails for `spago`, see https://github.com/spacchetti/spago/issues/104

RUN addgroup --system user && adduser --system --group user
RUN addgroup --system node && adduser --system --group node

RUN mkdir -p /home/node
RUN mkdir -p /home/node/.nvm
RUN chown -R node:node /home/node && chmod -R 755 /home/node

# ENV STACK_DIR /home/stack
# RUN mkdir -p $STACK_DIR
# RUN chown -R user:user $STACK_DIR && chmod -R 755 $STACK_DIR

ENV PS_DIR /home/purescript
RUN mkdir -p $PS_DIR
RUN chown -R user:user $PS_DIR && chmod -R 755 $PS_DIR

# ENV SPAGO_DIR /home/spago
# RUN mkdir -p $SPAGO_DIR
# RUN chown -R user:user $SPAGO_DIR && chmod -R 755 $SPAGO_DIR

ENV NODE_VERSION 12.11.0
# ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
# ENV PATH=$PATH:/home/node/.npm-global/bin

RUN apt-get update -yqq

RUN apt-get -yqq install curl && \
    apt-get -yqq install g++ gcc libc6 libc6-dev libffi-dev libgmp-dev make && \
    apt-get -yqq install xz-utils zlib1g-dev git gnupg && \
    # apt-get -yqq install libncurses5-dev libncursesw5-dev && \
    apt-get -yqq install libtinfo5 && \
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

ENV PURESCRIPT_DOWNLOAD_SHA1 6838ae5972a6130608c04002e46e96915e05f256

ENV PURESCRIPT_VERSION 0.13.3

RUN curl -sSL https://github.com/purescript/purescript/releases/download/v$PURESCRIPT_VERSION/linux64.tar.gz --output purescript.tar.gz \
    && echo "$PURESCRIPT_DOWNLOAD_SHA1 purescript.tar.gz" | sha1sum -c - \
    && tar -xvzf ./purescript.tar.gz \
    && mv ./purescript/* $PS_DIR \
    && rm -R ./purescript \
    && rm ./purescript.tar.gz

ENV PATH $PS_DIR:$PATH

# ENV SPAGO_VERSION 0.10.0.0

# RUN curl -sSL https://github.com/spacchetti/spago/releases/download/$SPAGO_VERSION/linux.tar.gz --output spago.tar.gz \
#     && tar -xvzf ./spago.tar.gz \
#     && mv ./spago $SPAGO_DIR \
#     && rm ./spago.tar.gz

# ENV PATH $SPAGO_DIR:$PATH

USER node

RUN npm cache clean --force && \
    npm install -g pulp && \
    npm install -g bower

USER root

RUN chown -R user:user /usr/src/rpd-purs && chmod -R 755 /usr/src/rpd-purs

USER user

RUN pwd && \
    ls -laF . && \
    bower install -F && \
    pulp build
    # spago install && \
    # spago build

EXPOSE 1337

CMD [ "npm", "run", "purs:server" ]
