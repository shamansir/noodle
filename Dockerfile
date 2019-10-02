FROM ubuntu:18.04
# 19.04 fails for `spago`, see https://github.com/spacchetti/spago/issues/104

RUN addgroup --system user && adduser --system --group user
RUN addgroup --system node && adduser --system --group node

RUN mkdir -p /home/node
RUN mkdir -p /home/node/.nvm
RUN chown -R node:node /home/node && chmod -R 755 /home/node

ENV STACK_DIR /home/stack
RUN mkdir -p $STACK_DIR
RUN chown -R user:user $STACK_DIR && chmod -R 755 $STACK_DIR

ENV PS_DIR /home/purescript
RUN mkdir -p $PS_DIR
RUN chown -R user:user $PS_DIR && chmod -R 755 $PS_DIR


ENV NODE_VERSION 12.11.0
# ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
# ENV PATH=$PATH:/home/node/.npm-global/bin

RUN apt-get update -yqq

RUN apt-get -yqq install curl && \
    apt-get -yqq install g++ gcc libc6 libc6-dev libffi-dev libgmp-dev make && \
    apt-get -yqq install xz-utils zlib1g-dev git gnupg && \
    apt-get -yqq install libncurses5-dev libncursesw5-dev && \
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

# ENV STACK_VERSION 2.1.3
# ENV STACK_SOURCE linux-x86_64-static

# RUN curl -sSL https://get.haskellstack.org/stable/$STACK_SOURCE.tar.gz --output stack.tar.gz \
#     && tar -xvzf ./stack.tar.gz \
#     && mv ./stack-$STACK_VERSION-$STACK_SOURCE/stack $STACK_DIR \
#     && rm ./stack.tar.gz \
#     && rm -R ./stack-$STACK_VERSION-$STACK_SOURCE \
#     && chmod +x $STACK_DIR/stack

# RUN ls -laF $STACK_DIR

# ENV PATH $STACK_DIR:$PATH

# RUN stack --version

# RUN stack setup

ENV PURESCRIPT_DOWNLOAD_SHA1 1969df7783f1e95b897f5b36ab1e12ab9cbc9166

RUN curl -sSL https://github.com/purescript/purescript/releases/download/v0.12.5/linux64.tar.gz --output purescript.tar.gz \
    && echo "$PURESCRIPT_DOWNLOAD_SHA1 purescript.tar.gz" | sha1sum -c - \
    && tar -xvzf ./purescript.tar.gz \
    && mv ./purescript/* $PS_DIR \
    && rm -R ./purescript \
    && rm ./purescript.tar.gz

ENV PATH $PS_DIR/purescript:$PATH

USER root

RUN find / -name "libtinfo"
RUN ln -s /path/to/libtinfo.so.6 /path/to/libtinfo.so.5

USER node

# TODO: move to parcel / psc-package
RUN npm cache clean --force && \
    npm install -g pulp && \
    npm install -g spago --unsafe-perm

# USER user

RUN spago install && \
    npm run purs:build && \
    npm run purs:test

EXPOSE 1337

CMD [ "npm", "run", "purs:server" ]
