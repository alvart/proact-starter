/*
  @license MIT
  Document.js
*/

"use strict";

exports.setDocumentTitle =
  function (title)
  {
    return function ()
    {
      window.document.title = title;
    };
  };
