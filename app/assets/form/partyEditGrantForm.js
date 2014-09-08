'use strict';

module.directive('partyEditGrantForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.partyEditGrantForm;

      form.data = [];

      //

      form.init = function (party, children) {
        form.party = form.party || party;
        form.data = children;
      };

      //

      var subforms = [];

      $scope.$watch(function () {
        var clean = true;

        angular.forEach(subforms, function (subform) {
          if (subform.$dirty) {
            clean = false;
            return false;
          }
        });

        if (clean) {
          form.$setPristine();
        }
      });

      form.saveAll = function () {
        angular.forEach(subforms, function (subform) {
          if (subform.$dirty) {
            subform.save(false);
          }
        });
      };

      form.resetAll = function(force){
	if(force || confirm(page.constants.message('navigation.confirmation'))){
	  page.$route.reload();
	  return true;
	}
	return false;
      };

      form.scrollToFuture = function (party) {
        var remove = $scope.$watch(function () {
          return subforms[subforms.length - 1];
        }, function (subform) {
          if (subform && subform.other && subform.other.party == party) {
            page.display.scrollTo(subform.$element);
            remove();
          }
        });
      };

      //

      page.events.listen($scope, 'authGrantForm-init', function (event, grantForm) {
        subforms.push(grantForm);

        grantForm.successFn = function () {
          form.messages.add({
            body: page.constants.message('auth.grant.save.success'),
            type: 'green',
            countdown: 3000,
          });
        };

        grantForm.denySuccessFn = function () {
          form.messages.add({
            body: page.constants.message('auth.grant.remove.success'),
            type: 'green',
            countdown: 3000,
          });

          form.data.splice(form.data.indexOf(grantForm.other), 1);
          subforms.splice(subforms.indexOf(grantForm), 1);
        };

        event.stopPropagation();
      });

      page.events.listen($scope, 'authSearchForm-init', function (event, searchForm) {
        if (searchForm.principal != 'child') {
          return;
        }

        searchForm.selectFn = function (found) {
	  form.data.push({
	    new: true,
	    party: found,
	    site: 0,
	    member: 0,
	  });
	  //warning: next line is template dependent! if classnames change this will no longer work
	  page.$timeout(function() {
	    var newEl = $('fieldset article.permission-auth.peg').last();
	    page.display.scrollTo(newEl);
	  });
        };

        searchForm.notFoundFn = function () {
          page.messages.add({
            type: 'yellow',
            countdown: 3000,
            body: page.constants.message('auth.grant.notfound')
          });
        };

        event.stopPropagation();
      });

      //

      var $float = $('.peg-float');
      var $floater = $('.peg-float-floater');
      $scope.scrollFn = page.display.makeFloatScrollFn($float, $floater, 24*2.5);
      page.$w.scroll($scope.scrollFn);

      page.events.talk('partyEditGrantForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'partyEditGrantForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
