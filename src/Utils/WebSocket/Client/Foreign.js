"use strict";

// import WebSocket, { WebSocketServer } from 'ws';

export function createWebSocket_(host, port, protocols) {
  return new WebSocket("ws://" + host + ":" + port, protocols);
}

export function onMessage_ (ws, handleMessage) {
  ws.on('message', handleMessage);
}

export function onOpen_(ws, handleOpen) {
  ws.on('open', handleOpen);
}

export function onClose_ (ws, handleClose) {
  ws.on('close', handleClose);
}

export function onError_ (ws, handleError) {
  ws.on('error', handleError);
}

export function sendMessage_ (ws, message) {
  ws.send(message);
}

export function close_ (ws, code, reason) {
  ws.close(code, reason);
}