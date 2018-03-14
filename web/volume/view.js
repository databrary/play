'use strict';

app.controller('volume/view', [
  '$scope', 'volume', 'displayService', '$location',
  function ($scope, volume, display, $location) {
    $scope.volume = volume;
    display.title = volume.name;
    $scope.checkpublicsharefull = volume.publicsharefull || volume.publicsharefull === null;
  }
]);
