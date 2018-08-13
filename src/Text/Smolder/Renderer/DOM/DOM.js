/* taken from https://pursuit.purescript.org/packages/purescript-smolder-dom/2.0.0 */
/* https://github.com/bodil/purescript-smolder-dom */

exports.makeElement = function(name) {
  return function() {
    return window.document.createElement(name);
  };
};

exports.makeText = function(text) {
  return function() {
    return window.document.createTextNode(text);
  };
};

exports.nodeListToArray = function(list) {
  return function() {
    return Array.from(list);
  };
}

exports.foldlAList = function(f) {
  return function(b) {
    return function(l) {
      for (var i = l.index, len = l.array.length; i < len; i++) {
        b = f(b)(l.array[i]);
      }
      return b;
    };
  };
}

// The following patch functions are ported from:
// https://github.com/yoshuawuyts/nanomorph/blob/master/lib/morph.js

exports.patchAttributes = function(node) {
  return function(newAttrs) {
    return function() {
      var oldAttrs = node.attributes;
      var fromValue, attrValue, attr;

      for (var attrName in newAttrs) {
        attrValue = newAttrs[attrName];
        fromValue = node.getAttribute(attrName);
        if (fromValue !== attrValue) {
          node.setAttribute(attrName, attrValue);
        }
      }

      for (var i = oldAttrs.length - 1; i >= 0; --i) {
        attr = oldAttrs[i];
        if (attr.specified !== false) {
          attrName = attr.name;
          if (!newAttrs.hasOwnProperty(attrName)) {
            node.removeAttribute(attrName);
          }
        }
      }
    };
  };
};

exports.patchEventListeners = function(node) {
  return function(events) {
    return function() {
      for (var i = 0; i < eventsLength; i++) {
        var ev = eventNames[i];
        if (events.hasOwnProperty(ev)) {
          node[ev] = events[ev];
        } else if (node[ev]) {
          node[ev] = undefined;
        }
      }
    };
  };
};

var eventNames = [
  // attribute events (can be set with attributes)
  "onclick",
  "ondblclick",
  "onmousedown",
  "onmouseup",
  "onmouseover",
  "onmousemove",
  "onmouseout",
  "onmouseenter",
  "onmouseleave",
  "ondragstart",
  "ondrag",
  "ondragenter",
  "ondragleave",
  "ondragover",
  "ondrop",
  "ondragend",
  "onkeydown",
  "onkeypress",
  "onkeyup",
  "onunload",
  "onabort",
  "onerror",
  "onresize",
  "onscroll",
  "onselect",
  "onchange",
  "onsubmit",
  "onreset",
  "onfocus",
  "onblur",
  "oninput",
  // other common events
  "oncontextmenu",
  "onfocusin",
  "onfocusout"
];

var eventsLength = eventNames.length;
