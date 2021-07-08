"use strict";

exports.alert = msg => () =>
  window.alert(msg);

exports.confirm = msg => () => {
  return window.confirm(msg);
}
 