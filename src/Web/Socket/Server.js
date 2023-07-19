"use strict";

import WebSocket, { WebSocketServer } from 'ws';

/* Server methods */
export function createWebSocketServer_ (options, callback) {
  return new WebSocketServer(options, callback);
}

export function onConnection_ (wss, handleConnection) {
  wss.on('connection', handleConnection);
}

export function onServerError_ (wss, handleError) {
  wss.on('error', handleError);
}

/* WebSocket methods */
export function onMessage_ (ws, handleMessage) {
  ws.on('message', handleMessage);
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