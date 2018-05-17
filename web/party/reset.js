/**
 * Reset password
 * @mixin party/reset
 */

'use strict';

app.controller('party/reset', [
  'displayService',
  function (display) {
    display.title = "Password Reset";
  }
]);
