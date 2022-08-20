FROM node:18-bullseye

RUN mkdir /app
COPY package.json /app/package.json
COPY yarn.lock /app/yarn.lock
WORKDIR /app
RUN yarn
COPY src/ /app/src

CMD yarn server