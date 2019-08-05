//////////////////////////////////////////////////////////////////////
//
// write-clipboard.js
// Custom HTML element to write a string to the clipboard.
// Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

customElements.define('write-clipboard', class extends HTMLElement {
  constructor() {
    super();
    // { id: "node id", text: "foo", count: 2 }
    this._write = {};
  }

  connectedCallback() {
    // Move along. Nothing to see here.
  }

  get write() {
    return this._write;
  }

  set write(value) {
    var write = this._write;
    if ('object' != typeof(write)) {
      write = {};
    }
    this._write = value;
    if ('object' == typeof(value) &&
        'function' == typeof(document.execCommand)) {
      if (write.id != value.id ||
          write.text != value.text ||
          write.count != value.count) {
        var id = value.id
        var text = value.text;
        if (id && text) {
          var node = document.getElementById(id);
          if (node) {
            if ('function' == typeof(node.focus) &&
                'function' == typeof(node.select)) {
              node.value = text;
              node.focus();
              node.select();
              try {
                document.execCommand('copy');
              } catch(e) {
                console.log(e);
              }            
            }
          }
        }
      }
    }
  }
});
