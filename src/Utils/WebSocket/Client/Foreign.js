"use strict";

// import WebSocket, { WebSocketServer } from 'ws';

export function createWebSocket_(host, port, protocols) {
  return new WebSocket("ws://" + host + ":" + port, protocols);
}

export function onMessage_ (ws, handleMessage) {
  ws.addEventListener('message', (evt) => {
      if (evt.data instanceof Blob) {
        evt.data.text().then(handleMessage);
      } else {
        handleMessage(evt.data);
      }
  });
}

export function onOpen_(ws, handleOpen) {
  ws.addEventListener('open', (evt) => handleOpen(evt.data));
}

export function onClose_ (ws, handleClose) {
  ws.addEventListener('close', (evt) => handleClose(evt.data));
}

export function onError_ (ws, handleError) {
  ws.addEventListener('error', (evt) => handleError(evt.data));
}

export function sendMessage_ (ws, message) {
  ws.send(message);
}

export function close_ (ws, code, reason) {
  ws.close(code, reason);
}