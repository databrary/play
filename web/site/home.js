'use strict';

app.controller('site/home', [
  '$scope', 'constantService', 'displayService', 'volume', 'tags', 'activity', 'pageService',
  function ($scope, constants, display, volume, tags, activity, page) {
    display.title = constants.message('welcome.title');
    $scope.volume = volume;
    $scope.tags = tags;
    $scope.activity = activity.activity;
    $scope.stats = activity.stats;
    var modal = document.getElementById('loginModal');
    var span = document.getElementsByClassName("close")[0];

    $('.modal-button').click(function(e) {
        e.preventDefault();
        modal.style.display = "block";
    });

    span.onclick = function() {
        modal.style.display = "none";
    }

    // When the user clicks anywhere outside of the modal, close it
    window.onclick = function(event) {
        if (event.target == modal) {
            modal.style.display = "none";
        }
    }

    if (page.models.Login.isLoggedIn())
    	modal.style.display = "none";
  }
]);
